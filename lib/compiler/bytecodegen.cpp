#include "bytecodegen.hpp"

#include <string.h>

#include "../rt.hpp"
#include "../util/avec.hpp"

namespace {

// Bytecode Method Builder
// =================================================================================================

struct MethodBuilderLoc {
    ORef maybeFilename;
    size_t srcIdx;

    static MethodBuilderLoc fromORef(RT const& state, ORef maybeLoc) {
        if (isa<Loc>(state, maybeLoc)) {
            auto const loc = HRef<Loc>::fromUnchecked(maybeLoc);
            return MethodBuilderLoc{loc->filename, (uint64_t)loc->byteIdx.val()};
        } else {
            return MethodBuilderLoc{Default, 0};
        }
    }
};

bool MethodBuilder::mark(RT& state) {
    for (Const& c : consts_) {
        c.val = TRY_NULLOPT_TO_FALSE(state.heap.mark(c.val));
    }

    maybeFilename_ = TRY_NULLOPT_TO_FALSE(state.heap.mark(maybeFilename_));

    size_t const filenameRunCount = revFilenameRuns_.count();
    for (size_t i = 1; i < filenameRunCount; i += 2) { // Skip fixnums at 0, 2, 4...
        revFilenameRuns_[i] = TRY_NULLOPT_TO_FALSE(state.heap.mark(revFilenameRuns_[i]));
    }

    if (parent_) {
        if (!parent_->mark(state)) { return false; }
    }

    return true;
}

void MethodBuilder::assertInTospace(RT const& state) const {
    for (Const const& c : consts_) {
        ORef const v = c.val;
        if (isHeaped(v)) {
            assert(state.heap.evacuated(&*HRef<Object>::fromUnchecked(v)));
        }
    }

    if (isHeaped(maybeFilename_)) {
        assert(state.heap.evacuated(&*HRef<Object>::fromUnchecked(maybeFilename_)));
    }

    size_t const filenameRunCount = revFilenameRuns_.count();
    for (size_t i = 1; i < filenameRunCount; i += 2) { // Skip fixnums at 0, 2, 4...
        ORef const v = revFilenameRuns_[i];
        if (isHeaped(v)) {
            assert(state.heap.evacuated(&*HRef<Object>::fromUnchecked(v)));
        }
    }

    if (parent_) { parent_->assertInTospace(state); }
}

HRef<Method> MethodBuilder::buildMethod(RT& state, IRFn& toplevelFn, IRFn const& fn) && {
    flushMethodBuilderDeltas();

    // Allocate method code:
    Fixnum const codeCount = Fixnum{int64_t(code_.count())};
    ByteArray* maybeCode = tryAllocByteArray(&state, codeCount);
    if (mustCollect(maybeCode)) {
        collectTracingIR(&state, &toplevelFn, this);
        maybeCode = allocByteArrayOrDie(&state, codeCount);
    }
    HRef<ByteArray> code = HRef<ByteArray>(maybeCode);
    auto const codeG = state.pushRoot(&code);

    { // Initialize method (reversing copy):
        uint8_t* codePtr = (uint8_t*)maybeCode->flexData();
        for (size_t i = code_.count(); i-- > 0; ++codePtr) {
            *codePtr = code_[i];
        }
    }

    // Create method consts:
    Fixnum const constCount = Fixnum{int64_t(consts_.count())};
    ArrayMut* maybeConsts = tryAllocArrayMut(&state, constCount);
    if (mustCollect(maybeConsts)) {
        collectTracingIR(&state, &toplevelFn, this);
        maybeConsts = allocArrayMutOrDie(&state, constCount);
    }
    HRef<ArrayMut> consts = HRef<ArrayMut>{maybeConsts};
    auto const constsG = state.pushRoot(&consts);
    { // Initialize:
        size_t const constCount = consts_.count();
        for (size_t i = 0; i < constCount; ++i) {
            const_cast<ORef*>(maybeConsts->flexData())[i] = consts_[i].val;
        }
    }

    // Copy `revFilenameRuns` to GC heap:
    size_t const filenamesSlotCount = revFilenameRuns_.count();
    auto const fxFilenamesSlotCount = Fixnum{(int64_t)filenamesSlotCount};
    Array* maybeFilenames = tryAllocArray(&state, fxFilenamesSlotCount);
    if (mustCollect(maybeFilenames)) {
        collectTracingIR(&state, &toplevelFn, this);
        maybeFilenames = allocArrayOrDie(&state, fxFilenamesSlotCount);
    }
    auto filenames = HRef<Array>{maybeFilenames};
    auto const filenamesG = state.pushRoot(&filenames);
    // Part of initialization, so `const_cast`:
    ORef* const filenamesData = const_cast<ORef*>(filenames->flexData());
    for (size_t i = 0; i < filenamesSlotCount; ++i) { // Reversing copy
        filenamesData[i] = revFilenameRuns_[filenamesSlotCount - 1 - i];
    }

    // Copy initial src byte index and `revIdxDeltas` to GC heap:
    size_t const firstSrcIdx = srcIdx_;
    size_t const revIdxDeltasCount = revIdxDeltas_.count();
    size_t const firstSrcIdxBitSize = requiredBitsize((int64_t)firstSrcIdx);
    // `firstSrcIdxBitSize` rounded up to nearest mutiple of `bytecodeVarIntPayloadWidth`.
    // `firstSrcIdxBitSize` cannot be 0 because no number can be stored in less than one bit:
    size_t const firstSrcIdxSize = 1 + ((firstSrcIdxBitSize - 1) / bytecodeVarIntPayloadWidth);
    // First code byte index is always zero, so not encoded:
    size_t const srcByteIdxsSize = firstSrcIdxSize + revIdxDeltasCount;
    auto const fxByteIdxsSize = Fixnum{(int64_t)srcByteIdxsSize};
    ByteArray* maybeSrcByteIdxs = tryAllocByteArray(&state, fxByteIdxsSize);
    if (mustCollect(maybeSrcByteIdxs)) {
        collectTracingIR(&state, &toplevelFn, this);
        maybeSrcByteIdxs = allocByteArrayOrDie(&state, fxByteIdxsSize);
    }
    HRef<ByteArray> srcByteIdxs = HRef<ByteArray>(maybeSrcByteIdxs);
    auto const srcByteIdxsG = state.pushRoot(&srcByteIdxs);
    {
        // Part of initialization, so `const_cast`:
        uint8_t* const srcByteIdxsData = const_cast<uint8_t*>(srcByteIdxs->flexData());
        size_t i = 0;

        // Initial src byte index:
        for (size_t shift = (firstSrcIdxSize - 1) * bytecodeVarIntPayloadWidth;
             shift > 0; // Last byte needs `bytecodeVarIntTerminalBit` besides having `shift == 0`
             ++i, shift -= bytecodeVarIntPayloadWidth
        ) {
            srcByteIdxsData[i] = (firstSrcIdx >> shift) & bytecodeVarIntPayloadMask;
        }
        srcByteIdxsData[i++] =
            bytecodeVarIntTerminalBit | (firstSrcIdx & bytecodeVarIntPayloadMask);

        // Following deltas:
        for (size_t j = 0; j < revIdxDeltasCount; ++j) { // Reversing copy starting at `i`
            srcByteIdxsData[i + j] = revIdxDeltas_[revIdxDeltasCount - 1 - j];
        }
    }

    size_t const arity = fn.blocks[0]->params.count() - 2;
    Fixnum const fxArity = Fixnum((intptr_t)arity);
    Bool const hasVarArg = Bool(fn.hasVarArg);
    std::span<uint8_t const> const codeSlice = code->flexItems();
    uintptr_t const hash = fnv1aHash_n(codeSlice.data(), codeSlice.size());
    Fixnum const fxHash = Fixnum((intptr_t)hash);
    Method* maybeMethod =
        tryAllocBytecodeMethod(&state, code, consts, fxArity, hasVarArg, fxHash, fn.maybeName,
                               filenames, srcByteIdxs);
    if (mustCollect(maybeMethod)) {
        collectTracingIR(&state, &toplevelFn, this);
        maybeMethod = allocBytecodeMethodOrDie(&state, code, consts, fxArity, hasVarArg, fxHash,
                                               fn.maybeName, filenames, srcByteIdxs);
    }
    if (fn.domain.count == 0) {
        for (size_t i = 0; i < arity; ++i) {
            const_cast<ORef*>(maybeMethod->flexData())[i] = state.types.any;
        }
    } else {
        for (size_t i = 0; i < arity; ++i) {
            IRName const typeName = fn.domain.vals[i];
            if (!typeName.isValid()) {
                const_cast<ORef*>(maybeMethod->flexData())[i] = state.types.any;
            } // else leave zeroed for specialization to fill in
        }
    }
    HRef<Method> const method = HRef<Method>{maybeMethod};

    return method;
}

MethodBuilder::MethodBuilder(RT const& state, Arena* arena, MethodBuilder* parent, IRFn const& fn) :
    code_{arena},
    labelIdxs_{arena, fn.blocks.count()},
    consts_{arena},
    prevMaybeLocRevIdx_{0},
    prevDeltaCodeByteRevIdx_{0},
    filenameCount_{0},
    revIdxDeltas_{arena},
    revFilenameRuns_{arena},
    parent_{parent}
{
    assert(fn.blocks.count() >= 1);
    IRBlock const& lastBlock = *fn.blocks[fn.blocks.count() -  1];
    ORef const lastMaybeLoc = lastBlock.stmts.count() > 0
        ? lastBlock.stmts[lastBlock.stmts.count() - 1].maybeLoc
        : lastBlock.transfer.maybeLoc;
    MethodBuilderLoc const lastLoc = MethodBuilderLoc::fromORef(state, lastMaybeLoc);

    srcIdx_ = lastLoc.srcIdx;
    maybeFilename_ = lastLoc.maybeFilename;
}

void encodeRevDelta(AVec<uint8_t>& revIdxDeltas, int64_t delta, size_t bitsize) {
    // This could become slightly negative since we are encoding at a granularity of
    // `bytecodeVarIntPayloadWidth`, so make it signed:
    auto remBits = (intptr_t)bitsize;

    uint8_t const byte = bytecodeVarIntTerminalBit | ((uint8_t)delta & bytecodeVarIntPayloadMask);
    revIdxDeltas.push(byte);
    delta = delta >> bytecodeVarIntPayloadWidth;
    remBits -= bytecodeVarIntPayloadWidth;

    while (remBits > 0) {
        uint8_t const byte = (uint8_t)delta & bytecodeVarIntPayloadMask;
        revIdxDeltas.push(byte);
        delta = delta >> bytecodeVarIntPayloadWidth;
        remBits -= bytecodeVarIntPayloadWidth;
    }
}

void MethodBuilder::pushMaybeLoc(RT const& state, ORef maybeLoc) {
    auto const loc = MethodBuilderLoc::fromORef(state, maybeLoc);

    filenameCount_ += code_.count() - prevMaybeLocRevIdx_;
    if (!(eq(loc.maybeFilename, maybeFilename_))) {
        revFilenameRuns_.push(Fixnum{(int64_t)filenameCount_});
        revFilenameRuns_.push(maybeFilename_);

        maybeFilename_ = loc.maybeFilename;
        filenameCount_ = 0;
    }

    if (loc.srcIdx != srcIdx_) {
        auto const srcIdxDelta = (int64_t)srcIdx_ - (int64_t)loc.srcIdx;
        encodeRevDelta(revIdxDeltas_, srcIdxDelta, requiredBitsize(srcIdxDelta));
        auto const codeByteRevIdxDelta =
            (int64_t)(code_.count() - prevDeltaCodeByteRevIdx_);
        encodeRevDelta(revIdxDeltas_, codeByteRevIdxDelta,
                       requiredBitsize(codeByteRevIdxDelta));

        srcIdx_ = loc.srcIdx;
        prevDeltaCodeByteRevIdx_ = code_.count();
    }

    prevMaybeLocRevIdx_ = code_.count();
}

void MethodBuilder::flushMethodBuilderDeltas() {
    revFilenameRuns_.push(Fixnum{(int64_t)filenameCount_});
    revFilenameRuns_.push(maybeFilename_);
}

void MethodBuilder::pushOp(RT const& state, Opcode op, ORef maybeLoc) {
    pushCodeByte(uint8_t(op));
    pushMaybeLoc(state, maybeLoc);
}

void MethodBuilder::pushDisplacement(size_t displacement) {
    assert(displacement <= UINT16_MAX); // TODO: Enable even bigger displacements
    pushCodeByte(uint8_t(displacement & UINT8_MAX));
    pushCodeByte(uint8_t((displacement >> UINT8_WIDTH) & UINT8_MAX));
}

void MethodBuilder::emitBitSet(BytefulBitSet const& bits) {
    // Encode bitset backwards into `builder`:
    size_t const byteCount = bytefulBitSetByteCount(&bits);
    for (size_t i = byteCount; i-- > 0;) {
        pushCodeByte(bytefulBitSetByte(&bits, i));
    }
    assert(byteCount < UINT8_MAX);
    pushCodeByte((uint8_t)byteCount);
}

void MethodBuilder::emitRegBits(std::span<IRName const> names, bool specializeHack) {
    // OPTIMIZE: Use `&compiler.arena`:
    BytefulBitSet bits = newBytefulBitSet(names.size()); // Need at least `count` bits, likely more

    // Set bits for each register:
    for (IRName const name : names) {
        size_t const regIdx = name.index;
        // FIXME: Hack for `specialize`, assumes that r0 cannot happen:
        if (!specializeHack || regIdx != 0) {
            bytefulBitSetSet(&bits, regIdx, true);
        }
    }

    emitBitSet(bits);

    freeBytefulBitSet(&bits);
}

uint8_t MethodBuilder::freshConstIndex(MethodBuilder::Const c) {
    size_t const i = consts_.count();
    consts_.push(c);
    assert(i <= UINT8_MAX);
    return uint8_t(i);
}

uint8_t MethodBuilder::constIndex(MethodBuilder::Const c) {
    // Linear search is actually good since there usually aren't that many constants per fn:
    auto const it = std::find(consts_.begin(), consts_.end(), c);
    if (it != consts_.end()) {
        auto const i = size_t(std::distance(consts_.begin(), it));
        assert(i <= UINT8_MAX);
        return uint8_t(i);
    }

    return freshConstIndex(c);
}

// Emit Bytecode over IR into Builder
// =================================================================================================

void emitConstDef(
    RT const& state, MethodBuilder& builder, IRName name, MethodBuilder::Const c, ORef maybeLoc
) {
    builder.emitConstArg(c);
    builder.pushReg(name);
    builder.pushOp(state, OP_CONST, maybeLoc);
}

HRef<Method> emitMethod(
    RT& state, Compiler& compiler, IRFn& toplevelFn, MethodBuilder* parentBuilder, IRFn const& fn);

void emitStmt(
    RT& state, Compiler& compiler, IRFn& toplevelFn, MethodBuilder& builder, IRStmt const& stmt
) {
    switch (stmt.type) {
    case IRStmt::GLOBAL_DEF: {
        Define const& define = stmt.define;

        builder.pushReg(define.val);
        builder.emitConstArg({define.name, MethodBuilder::Const::GLOBAL_NAME});
        builder.pushOp(state, OP_DEFINE, stmt.maybeLoc);
    }; break;

    case IRStmt::GLOBAL_SET: {
        GlobalSet const& globalSet = stmt.globalSet;

        builder.pushReg(globalSet.val);
        builder.emitConstArg({globalSet.name, MethodBuilder::Const::GLOBAL_NAME});
        builder.pushOp(state, OP_GLOBAL_SET, stmt.maybeLoc);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal const& global = stmt.global;

        builder.emitConstArg({global.name, MethodBuilder::Const::GLOBAL_NAME});
        builder.pushReg(global.tmpName);
        builder.pushOp(state, OP_GLOBAL, stmt.maybeLoc);
    }; break;

    case IRStmt::CONST_DEF: {
        ConstDef const& constDef = stmt.constDef;
        emitConstDef(state, builder, constDef.name, {constDef.v, MethodBuilder::Const::VALUE},
                     stmt.maybeLoc);
    }; break;

    case IRStmt::METHOD_DEF: {
        MethodDef const& methodDef = stmt.methodDef;
        IRFn const& fn = methodDef.fn;

        HRef<Method> const method = emitMethod(state, compiler, toplevelFn, &builder, fn);

        if (fn.domain.count == 0) {
            emitConstDef(state, builder, methodDef.name, {method, MethodBuilder::Const::VALUE},
                         stmt.maybeLoc);
        } else {
            builder.emitRegBits(std::span{fn.domain.vals, fn.domain.count}, true);
            builder.emitConstArg({method, MethodBuilder::Const::VALUE});
            builder.pushReg(methodDef.name);
            builder.pushOp(state, OP_SPECIALIZE, stmt.maybeLoc);
        }
    }; break;

    case IRStmt::CLOSURE: {
        IRClosure const& closure = stmt.closure;
        builder.emitClose(*closure.closes);
        builder.pushReg(closure.method);
        builder.pushReg(closure.name);
        builder.pushOp(state, OP_CLOSURE, stmt.maybeLoc);
    }; break;

    case IRStmt::CLOVER: {
        Clover const& clover = stmt.clover;
        builder.pushCodeByte(clover.idx);
        builder.pushReg(clover.closure);
        builder.pushReg(clover.name);
        builder.pushOp(state, OP_CLOVER, stmt.maybeLoc);
    }; break;

    case IRStmt::MOVE: {
        MoveStmt const& mov = stmt.mov;
        builder.pushReg(mov.src);
        builder.pushReg(mov.dest);
        builder.pushOp(state, OP_MOVE, stmt.maybeLoc);
    }; break;

    case IRStmt::SWAP: {
        SwapStmt const& swap = stmt.swap;
        builder.pushReg(swap.reg2);
        builder.pushReg(swap.reg1);
        builder.pushOp(state, OP_SWAP, stmt.maybeLoc);
    }; break;

    case IRStmt::KNOT: {
        KnotStmt const& knot = stmt.knot;
        builder.pushReg(knot.name);
        builder.pushOp(state, OP_KNOT, stmt.maybeLoc);
    }; break;

    case IRStmt::KNOT_INIT: {
        KnotInitStmt const& knotInit = stmt.knotInit;
        builder.pushReg(knotInit.v);
        builder.pushReg(knotInit.knot);
        builder.pushOp(state, OP_KNOT_INIT, stmt.maybeLoc);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt const& knotGet = stmt.knotGet;
        builder.pushReg(knotGet.knot);
        builder.pushReg(knotGet.name);
        builder.pushOp(state, OP_KNOT_GET, stmt.maybeLoc);
    }; break;

    case IRStmt::FFI_CALL: {
        FFICall const& ffiCall = stmt.ffiCall;

        {
            // OPTIMIZE: Use `&compiler.arena`:
            BytefulBitSet bits = newBytefulBitSet(1 + ffiCall.args.count()); // codomain, args

            size_t i = 0;

            bytefulBitSetSet(&bits, i++, ffiCall.codomain.box);

            for (FFICall::Arg const& arg : ffiCall.args) {
                bytefulBitSetSet(&bits, i++, arg.unbox);
            }

            builder.emitBitSet(bits);

            freeBytefulBitSet(&bits);
        }

        assert(ffiCall.args.count() <= UINT8_MAX);
        builder.pushCodeByte(uint8_t(ffiCall.args.count()));
        builder.pushReg(ffiCall.codomain.name);
        builder.pushReg(ffiCall.name);
        builder.pushOp(state, OP_FFICALL, stmt.maybeLoc);
    }; break;
    }
}

void emitTransfer(RT const& state, MethodBuilder& builder, IRTransfer const& transfer) {
    switch (transfer.type) {
    case IRTransfer::CALL: {
        Call const& call = transfer.call;

        // Guaranteed not to need an `OP_BR` to return block here.

        builder.emitClose(call.closes);

        size_t const regCount = 2 + call.args.count();
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        builder.pushCodeByte((uint8_t)regCount);

        builder.emitFreshConstArg({Default, MethodBuilder::Const::INLINE_CACHE});
        builder.freshConstIndex({Default, MethodBuilder::Const::INLINE_CACHE});

        builder.pushOp(state, OP_CALL, transfer.maybeLoc);
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall const& tailcall = transfer.tailcall;

        size_t const regCount = 2 + tailcall.args.count();
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        builder.pushCodeByte((uint8_t)regCount);

        builder.emitFreshConstArg({Default, MethodBuilder::Const::INLINE_CACHE});
        builder.freshConstIndex({Default, MethodBuilder::Const::INLINE_CACHE});

        builder.pushOp(state, OP_TAILCALL, transfer.maybeLoc);
    }; break;

    case IRTransfer::IF: {
        IRIf const& iff = transfer.iff;

        size_t const postIndex = builder.codeCount() - 1;
        size_t const destIndex = builder.getLabelIndex(iff.alt);
        size_t const displacement = postIndex - destIndex;

        builder.pushDisplacement(displacement);
        builder.pushReg(iff.cond);
        builder.pushOp(state, OP_BRF, transfer.maybeLoc);
    }; break;

    case IRTransfer::GOTO: {
        IRGoto const& gotoo = transfer.gotoo;

        size_t const postIndex = builder.codeCount() - 1;
        size_t const destIndex = builder.getLabelIndex(gotoo.dest);
        size_t const displacement = postIndex - destIndex;

        if (displacement > 0) { // Only emit branches that actually jump a distance.
            builder.pushDisplacement(displacement);
            builder.pushOp(state, OP_BR, transfer.maybeLoc);
        }
    }; break;

    case IRTransfer::RETURN: {
        builder.pushOp(state, OP_RET, transfer.maybeLoc);
    }; break;
    }
}

void emitBlock(
    RT& state, Compiler& compiler, IRFn& toplevelFn, MethodBuilder& builder, IRBlock const& block
) {
    emitTransfer(state, builder, block.transfer);

    for (size_t i = block.stmts.count(); i-- > 0;) {
        emitStmt(state, compiler, toplevelFn, builder, block.stmts[i]);
    }

    builder.setLabelIndex(block.label, builder.codeCount() - 1);
}

HRef<Method> emitMethod(
    RT& state, Compiler& compiler, IRFn& toplevelFn, MethodBuilder* parentBuilder, IRFn const& fn
) {
    auto builder = MethodBuilder{state, &compiler.arena, parentBuilder, fn};

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    for (size_t i = fn.blocks.count(); i-- > 0;) {
        emitBlock(state, compiler, toplevelFn, builder, *fn.blocks[i]);
    }

    return std::move(builder).buildMethod(state, toplevelFn, fn);
}

HRef<Method> emitToplevelMethod(RT& state, Compiler& compiler, IRFn& fn) {
    return emitMethod(state, compiler, fn, nullptr, fn);
}

}
