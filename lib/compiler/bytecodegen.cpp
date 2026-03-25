#include "bytecodegen.hpp"

#include <string.h>

#include "../rt.hpp"
#include "../primops.hpp"
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

    size_t const filenameRunCount = filenameRuns_.count();
    for (size_t i = 0; i < filenameRunCount; i += 2) { // Skip fixnums at 1, 3, 5...
        filenameRuns_[i] = TRY_NULLOPT_TO_FALSE(state.heap.mark(filenameRuns_[i]));
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

    size_t const filenameRunCount = filenameRuns_.count();
    for (size_t i = 0; i < filenameRunCount; i += 2) { // Skip fixnums at 1, 3, 5...
        ORef const v = filenameRuns_[i];
        if (isHeaped(v)) {
            assert(state.heap.evacuated(&*HRef<Object>::fromUnchecked(v)));
        }
    }

    if (parent_) { parent_->assertInTospace(state); }
}

HRef<Method> MethodBuilder::buildMethod(RT& state, IRFn& toplevelFn, IRFn const& fn) && {
    // Allocate method code:
    Fixnum const codeCount = Fixnum{int64_t(code_.count())};
    ByteArrayMut* maybeCode = tryAllocByteArrayMut(&state, codeCount);
    if (mustCollect(maybeCode)) {
        collectTracingIR(&state, &toplevelFn, this);
        maybeCode = allocByteArrayMutOrDie(&state, codeCount);
    }
    HRef<ByteArrayMut> code = HRef{maybeCode};
    auto const codeG = state.pushRoot(&code);
    std::copy(code_.begin(), code_.end(), code->itemsMut().begin());

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

    // Copy `filenameRuns` to GC heap:
    size_t const filenamesSlotCount = filenameRuns_.count();
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
    std::copy(filenameRuns_.begin(), filenameRuns_.end(), filenamesData);

    // Copy initial src byte index and `revIdxDeltas` to GC heap:
    assert(fn.blocks.count() >= 1);
    IRBlock const& entryBlock = *fn.blocks[0];
    ORef const firstMaybeLoc = entryBlock.stmts.count() > 0
                                   ? entryBlock.stmts[0].maybeLoc
                                   : entryBlock.transfer.maybeLoc;
    MethodBuilderLoc const firstLoc = MethodBuilderLoc::fromORef(state, firstMaybeLoc);
    size_t const firstSrcIdx = firstLoc.srcIdx;
    size_t const idxDeltasCount = idxDeltas_.count();
    size_t const firstSrcIdxBitSize = requiredBitsize((int64_t)firstSrcIdx);
    // `firstSrcIdxBitSize` rounded up to nearest mutiple of `bytecodeVarIntPayloadWidth`.
    // `firstSrcIdxBitSize` cannot be 0 because no number can be stored in less than one bit:
    size_t const firstSrcIdxSize = 1 + ((firstSrcIdxBitSize - 1) / bytecodeVarIntPayloadWidth);
    // First code byte index is always zero, so not encoded:
    size_t const srcByteIdxsSize = firstSrcIdxSize + idxDeltasCount;
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
        std::copy(idxDeltas_.begin(), idxDeltas_.end(), srcByteIdxsData + i);
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
    branchTargets_{arena},
    consts_{arena},
    prevMaybeLocIdx_{0},
    prevDeltaCodeByteIdx_{0},
    filenameCount_{0},
    idxDeltas_{arena},
    filenameRuns_{arena},
    parent_{parent}
{
    for (size_t i = 0; i < sizeof(MethodCode); ++i) { code_.push(0); } // OPTIMIZE
    *std::bit_cast<MethodCode*>(code_.data()) = callBytecode;

    assert(fn.blocks.count() >= 1);
    IRBlock const& entryBlock = *fn.blocks[0];
    ORef const firstMaybeLoc = entryBlock.stmts.count() > 0
       ? entryBlock.stmts[0].maybeLoc
       : entryBlock.transfer.maybeLoc;
    MethodBuilderLoc const firstLoc = MethodBuilderLoc::fromORef(state, firstMaybeLoc);

    srcIdx_ = firstLoc.srcIdx;
    maybeFilename_ = firstLoc.maybeFilename;
}

void encodeDelta(AVec<uint8_t>& idxDeltas, int64_t delta, size_t bitsize) {
    size_t remBits = bitsize;

    size_t const firstWidth = remBits % bytecodeVarIntPayloadWidth;
    if (firstWidth != 0) {
        remBits -= firstWidth;
        auto const firstMask = uint8_t((1 << firstWidth) - 1);
        auto const byte = uint8_t((uint64_t(delta) >> remBits) & firstMask);
        idxDeltas.push(byte);
    }

    while (remBits > bytecodeVarIntPayloadWidth) {
        remBits -= bytecodeVarIntPayloadWidth;
        auto const byte = uint8_t((uint64_t(delta) >> remBits) & bytecodeVarIntPayloadMask);
        idxDeltas.push(byte);
    }

    auto const byte = uint8_t(bytecodeVarIntTerminalBit | (delta & bytecodeVarIntPayloadMask));
    idxDeltas.push(byte);
}

void MethodBuilder::pushMaybeLoc(RT const& state, ORef maybeLoc) {
    auto const loc = MethodBuilderLoc::fromORef(state, maybeLoc);

    filenameCount_ += code_.count() - prevMaybeLocIdx_;
    if (!(eq(loc.maybeFilename, maybeFilename_))) {
        filenameRuns_.push(maybeFilename_);
        filenameRuns_.push(Fixnum{(int64_t)filenameCount_});

        maybeFilename_ = loc.maybeFilename;
        filenameCount_ = 0;
    }

    if (loc.srcIdx != srcIdx_) {
        auto const codeByteIdxDelta = int64_t(code_.count() - prevDeltaCodeByteIdx_);
        encodeDelta(idxDeltas_, codeByteIdxDelta, requiredBitsize(codeByteIdxDelta));
        auto const srcIdxDelta = (int64_t)loc.srcIdx - (int64_t)srcIdx_;
        encodeDelta(idxDeltas_, srcIdxDelta, requiredBitsize(srcIdxDelta));

        srcIdx_ = loc.srcIdx;
        prevDeltaCodeByteIdx_ = code_.count();
    }

    prevMaybeLocIdx_ = code_.count();
}

void MethodBuilder::flushMethodBuilderDeltas() {
    filenameCount_ += code_.count() - prevMaybeLocIdx_;

    filenameRuns_.push(maybeFilename_);
    filenameRuns_.push(Fixnum{(int64_t)filenameCount_});
}

void MethodBuilder::pushOp(RT const& state, Opcode op, ORef maybeLoc) {
    pushCodeByte(uint8_t(op));
    pushMaybeLoc(state, maybeLoc);
}

void MethodBuilder::reserveDisplacement(IRLabel dest) {
    size_t const displacementIdx = code_.count();
    branchTargets_.set(displacementIdx, dest);

    pushCodeByte(0);
    pushCodeByte(0);
}

void MethodBuilder::emitBitSet(BytefulBitSet const& bits) {
    size_t const byteCount = bytefulBitSetByteCount(&bits);

    assert(byteCount < UINT8_MAX);
    pushCodeByte((uint8_t)byteCount);

    for (size_t i = 0; i < byteCount; ++i) {
        pushCodeByte(bytefulBitSetByte(&bits, i));
    }
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

void MethodBuilder::emitAligningClose(std::span<IRName const> names, size_t align) {
    // OPTIMIZE: Use `&compiler.arena`:
    BytefulBitSet bits = newBytefulBitSet(names.size()); // Need at least `count` bits, likely more

    // Set bits for each register:
    for (IRName const name : names) {
        size_t const regIdx = name.index;
        bytefulBitSetSet(&bits, regIdx, true);
    }

    size_t const prePc = code_.count();
    size_t const closeByteCount = bytefulBitSetByteCount(&bits);
    size_t const postClosePc = prePc + 1 + closeByteCount;
    size_t const postPc = alignUp(postClosePc, align);
    size_t const padding = postPc - postClosePc;
    size_t const byteCount = closeByteCount + padding;

    assert(byteCount < UINT8_MAX);
    pushCodeByte((uint8_t)byteCount);

    for (size_t i = 0; i < closeByteCount; ++i) {
        pushCodeByte(bytefulBitSetByte(&bits, i));
    }

    for (size_t i = 0; i < padding; ++i) { pushCodeByte(0); }

    freeBytefulBitSet(&bits);
}

void MethodBuilder::emitCodePtr(MethodCode nativeCode) {
    assert((code_.count() & (alignof(MethodCode) - 1)) == 0);

    size_t const startIdx = code_.count();

    for (size_t i = 0; i < sizeof(MethodCode); ++i) { code_.push(0); } // OPTIMIZE

    *std::bit_cast<MethodCode*>(code_.data() + startIdx) = nativeCode;
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

void MethodBuilder::patchBranches() {
    for (auto const& entry : branchTargets_) {
        size_t const branchIdx = entry.key;
        IRLabel const destLabel = entry.value;

        size_t const destIdx = labelIdxs_.get(destLabel);
        size_t const displacement = destIdx - (branchIdx + sizeof(uint16_t));
        assert(displacement <= UINT16_MAX); // TODO: Enable even bigger displacements?

        code_[branchIdx] = uint8_t((displacement >> UINT8_WIDTH) & UINT8_MAX);
        code_[branchIdx + 1] = uint8_t(displacement & UINT8_MAX);
    }
}

// Emit Bytecode over IR into Builder
// =================================================================================================

void emitConstDef(
    RT const& state, MethodBuilder& builder, IRName name, MethodBuilder::Const c, ORef maybeLoc
) {
    builder.pushOp(state, OP_CONST, maybeLoc);
    builder.pushReg(name);
    builder.emitConstArg(c);
}

HRef<Method> emitMethod(
    RT& state, Compiler& compiler, IRFn& toplevelFn, MethodBuilder* parentBuilder, IRFn const& fn);

void emitStmt(
    RT& state, Compiler& compiler, IRFn& toplevelFn, MethodBuilder& builder, IRStmt const& stmt
) {
    switch (stmt.type) {
    case IRStmt::GLOBAL_DEF: {
        Define const& define = stmt.define;

        builder.pushOp(state, OP_DEFINE, stmt.maybeLoc);
        builder.emitConstArg({define.name, MethodBuilder::Const::GLOBAL_NAME});
        builder.pushReg(define.val);
    }; break;

    case IRStmt::GLOBAL_SET: {
        GlobalSet const& globalSet = stmt.globalSet;

        builder.pushOp(state, OP_GLOBAL_SET, stmt.maybeLoc);
        builder.emitConstArg({globalSet.name, MethodBuilder::Const::GLOBAL_NAME});
        builder.pushReg(globalSet.val);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal const& global = stmt.global;

        builder.pushOp(state, OP_GLOBAL, stmt.maybeLoc);
        builder.pushReg(global.tmpName);
        builder.emitConstArg({global.name, MethodBuilder::Const::GLOBAL_NAME});
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
            builder.pushOp(state, OP_SPECIALIZE, stmt.maybeLoc);
            builder.pushReg(methodDef.name);
            builder.emitConstArg({method, MethodBuilder::Const::VALUE});
            builder.emitRegBits(std::span{fn.domain.vals, fn.domain.count}, true);
        }
    }; break;

    case IRStmt::CLOSURE: {
        IRClosure const& closure = stmt.closure;

        builder.pushOp(state, OP_CLOSURE, stmt.maybeLoc);
        builder.pushReg(closure.name);
        builder.pushReg(closure.method);
        builder.emitClose(*closure.closes);
    }; break;

    case IRStmt::CLOVER: {
        Clover const& clover = stmt.clover;

        builder.pushOp(state, OP_CLOVER, stmt.maybeLoc);
        builder.pushReg(clover.name);
        builder.pushReg(clover.closure);
        builder.pushCodeByte(clover.idx);
    }; break;

    case IRStmt::UNSPILL: {
        Unspill const& unspill = stmt.unspill;

        builder.pushOp(state, OP_UNSPILL, stmt.maybeLoc);
        builder.pushReg(unspill.name);
        builder.pushReg(unspill.closure);
        builder.pushCodeByte(unspill.idx);
    }; break;

    case IRStmt::MOVE: {
        MoveStmt const& mov = stmt.mov;

        builder.pushOp(state, OP_MOVE, stmt.maybeLoc);
        builder.pushReg(mov.dest);
        builder.pushReg(mov.src);
    }; break;

    case IRStmt::SWAP: {
        SwapStmt const& swap = stmt.swap;

        builder.pushOp(state, OP_SWAP, stmt.maybeLoc);
        builder.pushReg(swap.reg1);
        builder.pushReg(swap.reg2);
    }; break;

    case IRStmt::KNOT: {
        KnotStmt const& knot = stmt.knot;

        builder.pushOp(state, OP_KNOT, stmt.maybeLoc);
        builder.pushReg(knot.name);
    }; break;

    case IRStmt::KNOT_INIT: {
        KnotInitStmt const& knotInit = stmt.knotInit;

        builder.pushOp(state, OP_KNOT_INIT, stmt.maybeLoc);
        builder.pushReg(knotInit.knot);
        builder.pushReg(knotInit.v);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt const& knotGet = stmt.knotGet;

        builder.pushOp(state, OP_KNOT_GET, stmt.maybeLoc);
        builder.pushReg(knotGet.name);
        builder.pushReg(knotGet.knot);
    }; break;

    case IRStmt::FFI_CALL: {
        FFICall const& ffiCall = stmt.ffiCall;

        builder.pushOp(state, OP_FFICALL, stmt.maybeLoc);
        builder.pushReg(ffiCall.name);
        builder.pushReg(ffiCall.codomain.name);
        size_t const argc = ffiCall.args.count();
        if (argc >= UINT8_MAX) {
            // TODO: Handle absurd argument count (probably too late here)
            PANIC("Too many (%lu) fficall arguments", argc);
        }
        builder.pushCodeByte(uint8_t(argc));

        {
            // OPTIMIZE: Use `&compiler.arena`:
            BytefulBitSet bits = newBytefulBitSet(1 + argc); // codomain, args

            size_t i = 0;

            bytefulBitSetSet(&bits, i++, ffiCall.codomain.box);

            for (FFICall::Arg const& arg : ffiCall.args) {
                bytefulBitSetSet(&bits, i++, arg.unbox);
            }

            builder.emitBitSet(bits);

            freeBytefulBitSet(&bits);
        }
    }; break;
    }
}

void emitTransfer(
    RT const& state, MethodBuilder& builder, std::optional<IRLabel> fallthrough,
    IRTransfer const& transfer
) {
    switch (transfer.type) {
    case IRTransfer::CALL: {
        Call const& call = transfer.call;

        // Guaranteed not to need an `OP_BR` to return block here.

        builder.pushOp(state, OP_CALL, transfer.maybeLoc);

        builder.emitFreshConstArg({Default, MethodBuilder::Const::INLINE_CACHE});
        builder.freshConstIndex({Default, MethodBuilder::Const::INLINE_CACHE});

        size_t const regCount = firstArgReg + call.args.count();
        if (regCount >= UINT8_MAX) {
            // TODO: Handle absurd argument count (probably too late here)
            PANIC("Too many (%lu) call arguments", call.args.count());
        }
        builder.pushCodeByte((uint8_t)regCount);

        builder.emitAligningClose(call.closes, alignof(MethodCode));
        builder.emitCodePtr(interpret);
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall const& tailcall = transfer.tailcall;

        builder.pushOp(state, OP_TAILCALL, transfer.maybeLoc);

        builder.emitFreshConstArg({Default, MethodBuilder::Const::INLINE_CACHE});
        builder.freshConstIndex({Default, MethodBuilder::Const::INLINE_CACHE});

        size_t const regCount = firstArgReg + tailcall.args.count();
        if (regCount >= UINT8_MAX) {
            // TODO: Handle absurd argument count (probably too late here)
            PANIC("Too many (%lu) tailcall arguments", tailcall.args.count());
        }
        builder.pushCodeByte((uint8_t)regCount);
    }; break;

    case IRTransfer::IF: {
        IRIf const& iff = transfer.iff;

        builder.pushOp(state, OP_BRF, transfer.maybeLoc);
        builder.pushReg(iff.cond);
        builder.reserveDisplacement(iff.alt);
    }; break;

    case IRTransfer::GOTO: {
        IRGoto const& gotoo = transfer.gotoo;

        if (!fallthrough || gotoo.dest != *fallthrough) {
            builder.pushOp(state, OP_BR, transfer.maybeLoc);
            builder.reserveDisplacement(gotoo.dest);
        }
    }; break;

    case IRTransfer::RETURN: {
        builder.pushOp(state, OP_RET, transfer.maybeLoc);
    }; break;
    }
}

void emitBlock(
    RT& state, Compiler& compiler, IRFn& toplevelFn, MethodBuilder& builder,
    std::optional<IRLabel> fallthrough, IRBlock const& block
) {
    builder.setLabelIndex(block.label, builder.codeCount());

    for (IRStmt const& stmt : block.stmts) {
        emitStmt(state, compiler, toplevelFn, builder, stmt);
    }

    emitTransfer(state, builder, fallthrough, block.transfer);
}

HRef<Method> emitMethod(
    RT& state, Compiler& compiler, IRFn& toplevelFn, MethodBuilder* parentBuilder, IRFn const& fn
) {
    auto builder = MethodBuilder{state, &compiler.arena, parentBuilder, fn};

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    {
        auto const end = fn.blocks.end();
        for (auto it = fn.blocks.begin(); it != end;) {
            IRBlock const* const block = *it;
            ++it;
            std::optional<IRLabel> const fallthrough = it != end
                ? std::optional{(*it)->label}
                : std::nullopt;

            emitBlock(state, compiler, toplevelFn, builder, fallthrough, *block);
        }
    }

    builder.flushMethodBuilderDeltas();

    builder.patchBranches();

    return std::move(builder).buildMethod(state, toplevelFn, fn);
}

HRef<Method> emitToplevelMethod(RT& state, Compiler& compiler, IRFn& fn) {
    return emitMethod(state, compiler, fn, nullptr, fn);
}

}
