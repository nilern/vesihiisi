#include "bytecodegen.hpp"

#include <string.h>

#include "../state.hpp"
#include "../bytecode.hpp"
#include "../util/avec.hpp"
#include "../util/bytefulbitset.hpp"

namespace {

// Label to Bytecode Index Mapping
// =================================================================================================

typedef struct LabelIdxs {
    size_t* idxs;
} LabelIdxs;

LabelIdxs createLabelIdxs(Compiler* compiler, size_t blockCount) {
    size_t* const idxs = (size_t*)amalloc(&compiler->arena, blockCount * sizeof *idxs);
    return LabelIdxs{.idxs = idxs};
}

inline size_t getLabelIndex(LabelIdxs const* labelIdxs, IRLabel label) {
    return labelIdxs->idxs[label.blockIndex];
}

inline void setLabelIndex(LabelIdxs* labelIdxs, IRLabel label, size_t index) {
    labelIdxs->idxs[label.blockIndex] = index;
}

// Bytecode Method Builder
// =================================================================================================

struct MethodBuilderLoc {
    ORef maybeFilename;
    size_t srcIdx;

    static MethodBuilderLoc fromORef(State const& state, ORef maybeLoc) {
        if (isa(&state, state.types.loc, maybeLoc)) {
            Loc const* const loc = HRef<Loc>::fromUnchecked(maybeLoc).ptr();
            return MethodBuilderLoc{loc->filename, (uint64_t)loc->byteIdx.val()};
        } else {
            return MethodBuilderLoc{Default, 0};
        }
    }
};

typedef struct MethodBuilder {
    uint8_t* code;
    size_t codeCount;
    size_t codeCap;

    LabelIdxs labelIdxs;

    ORef* consts;
    size_t constCount;
    size_t constCap;

    size_t prevMaybeLocRevIdx;
    size_t srcIdx;
    size_t prevDeltaCodeByteRevIdx;
    ORef maybeFilename;
    size_t filenameCount;
    AVec<uint8_t> revIdxDeltas;
    AVec<ORef> revFilenameRuns;

    struct MethodBuilder* parent;
} MethodBuilder;

void markMethodBuilder(State* state, MethodBuilder* builder) {
    size_t const constCount = builder->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        builder->consts[i] = state->heap.mark(builder->consts[i]);
    }

    builder->maybeFilename = state->heap.mark(builder->maybeFilename);

    size_t const filenameRunCount = builder->revFilenameRuns.count();
    for (size_t i = 1; i < filenameRunCount; i += 2) { // Skip fixnums at 0, 2, 4...
        builder->revFilenameRuns[i] = state->heap.mark(builder->revFilenameRuns[i]);
    }

    if (builder->parent) { markMethodBuilder(state, builder->parent); }
}

[[maybe_unused]]
void assertMethodBuilderInTospace(State const* state,MethodBuilder const* builder) {
    size_t const constCount = builder->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        ORef const v = builder->consts[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    size_t const filenameRunCount = builder->revFilenameRuns.count();
    for (size_t i = 1; i < filenameRunCount; i += 2) { // Skip fixnums at 0, 2, 4...
        ORef const v = builder->revFilenameRuns[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    if (builder->parent) { assertMethodBuilderInTospace(state, builder->parent); }
}

void flushMethodBuilderDeltas(MethodBuilder& builder);

HRef<Method> buildMethod(
    State* state, IRFn* toplevelFn, MethodBuilder&& builder, IRFn const* fn
) {
    flushMethodBuilderDeltas(builder);

    // Allocate method code:
    Fixnum const codeCount = Fixnum((intptr_t)builder.codeCount);
    ByteArray* maybeCode = tryAllocByteArray(state, codeCount);
    if (mustCollect(maybeCode)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeCode = allocByteArrayOrDie(state, codeCount);
    }
    HRef<ByteArray> code = HRef<ByteArray>(maybeCode);
    pushStackRoot(state, (ORef*)&code);

    { // Initialize method:
        uint8_t* codePtr = (uint8_t*)maybeCode->flexData();
        for (size_t i = builder.codeCount; i-- > 0; ++codePtr) {
            *codePtr = builder.code[i];
        }
    }

    // Create method consts:
    Fixnum const constCount = Fixnum((intptr_t)builder.constCount);
    ArrayMut* maybeConsts = tryAllocArrayMut(state, constCount);
    if (mustCollect(maybeConsts)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeConsts = allocArrayMutOrDie(state, constCount);
    }
    HRef<ArrayMut> consts = HRef<ArrayMut>{maybeConsts};
    pushStackRoot(state, (ORef*)&consts);
    memcpy(maybeConsts, builder.consts, builder.constCount * sizeof *builder.consts); // Initialize

    // Copy `revFilenameRuns` to GC heap:
    size_t const filenamesSlotCount = builder.revFilenameRuns.count();
    auto const fxFilenamesSlotCount = Fixnum{(int64_t)filenamesSlotCount};
    Array* maybeFilenames = tryAllocArray(state, fxFilenamesSlotCount);
    if (mustCollect(maybeFilenames)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeFilenames = allocArrayOrDie(state, fxFilenamesSlotCount);
    }
    auto filenames = HRef<Array>{maybeFilenames};
    pushStackRoot(state, &filenames);
    // Part of initialization, so `const_cast`:
    ORef* const filenamesData = const_cast<ORef*>(filenames.ptr()->flexData());
    for (size_t i = 0; i < filenamesSlotCount; ++i) { // Reversing copy
        filenamesData[i] = builder.revFilenameRuns[filenamesSlotCount - 1 - i];
    }

    // Copy initial src byte index and `revIdxDeltas` to GC heap:
    size_t const firstSrcIdx = builder.srcIdx;
    size_t const revIdxDeltasCount = builder.revIdxDeltas.count();
    size_t const firstSrcIdxBitSize = requiredBitsize((int64_t)firstSrcIdx);
    // `firstSrcIdxBitSize` rounded up to nearest mutiple of `bytecodeVarIntPayloadWidth`.
    // `firstSrcIdxBitSize` cannot be 0 because no number can be stored in less than one bit:
    size_t const firstSrcIdxSize = 1 + ((firstSrcIdxBitSize - 1) / bytecodeVarIntPayloadWidth);
    // First code byte index is always zero, so not encoded:
    size_t const srcByteIdxsSize = firstSrcIdxSize + revIdxDeltasCount;
    auto const fxByteIdxsSize = Fixnum{(int64_t)srcByteIdxsSize};
    ByteArray* maybeSrcByteIdxs = tryAllocByteArray(state, fxByteIdxsSize);
    if (mustCollect(maybeSrcByteIdxs)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeSrcByteIdxs = allocByteArrayOrDie(state, fxByteIdxsSize);
    }
    HRef<ByteArray> srcByteIdxs = HRef<ByteArray>(maybeSrcByteIdxs);
    pushStackRoot(state, &srcByteIdxs);
    {
        // Part of initialization, so `const_cast`:
        uint8_t* const srcByteIdxsData = const_cast<uint8_t*>(srcByteIdxs.ptr()->flexData());
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
            srcByteIdxsData[i + j] = builder.revIdxDeltas[revIdxDeltasCount - 1 - j];
        }
    }

    size_t const arity = fn->blocks[0]->paramCount - 2;
    Fixnum const fxArity = Fixnum((intptr_t)arity);
    Bool const hasVarArg = Bool(fn->hasVarArg);
    Slice<uint8_t const> const codeSlice = code.ptr()->flexItems();
    uintptr_t const hash = fnv1aHash_n((char const*)codeSlice.data, codeSlice.count);
    Fixnum const fxHash = Fixnum((intptr_t)hash);
    Method* maybeMethod =
        tryAllocBytecodeMethod(state, code, consts, fxArity, hasVarArg, fxHash, fn->maybeName,
                               filenames, srcByteIdxs);
    if (mustCollect(maybeMethod)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeMethod = allocBytecodeMethodOrDie(state, code, consts, fxArity, hasVarArg, fxHash,
                                               fn->maybeName, filenames, srcByteIdxs);
    }
    if (fn->domain.count == 0) {
        for (size_t i = 0; i < arity; ++i) {
            maybeMethod->domain()[i] = state->types.any.oref();
        }
    } else {
        for (size_t i = 0; i < arity; ++i) {
            IRName const typeName = fn->domain.vals[i];
            if (!irNameIsValid(typeName)) {
                maybeMethod->domain()[i] = state->types.any.oref();
            } // else leave zeroed for specialization to fill in
        }
    }
    HRef<Method> const method = HRef<Method>{maybeMethod};

    popStackRoots(state, 4);
    return method;
}

MethodBuilder createMethodBuilder(
    State const& state, Compiler* compiler, MethodBuilder* parent, IRFn const& fn
) {
    size_t const codeCap = 2;
    uint8_t* const code = (uint8_t*)amalloc(&compiler->arena, codeCap * sizeof *code);

    size_t const constCap = 2;
    ORef* const consts = (ORef*)amalloc(&compiler->arena, constCap * sizeof *consts);

    assert(fn.blockCount >= 1);
    IRBlock const* const lastBlock = fn.blocks[fn.blockCount -  1];
    ORef const lastMaybeLoc = lastBlock->stmts.count > 0
        ? lastBlock->stmts.vals[lastBlock->stmts.count - 1].maybeLoc
        : lastBlock->transfer.maybeLoc;
    MethodBuilderLoc const lastLoc = MethodBuilderLoc::fromORef(state, lastMaybeLoc);

    return MethodBuilder{
        .code = code,
        .codeCount = 0,
        .codeCap = codeCap,

        .labelIdxs = createLabelIdxs(compiler, fn.blockCount),

        .consts = consts,
        .constCount = 0,
        .constCap = constCap,

        .prevMaybeLocRevIdx = 0,
        .srcIdx = lastLoc.srcIdx,
        .prevDeltaCodeByteRevIdx = 0,
        .maybeFilename = lastLoc.maybeFilename,
        .filenameCount = 0,
        .revIdxDeltas = {&compiler->arena},
        .revFilenameRuns = {&compiler->arena},

        .parent = parent
    };
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

void pushMaybeLoc(State const& state, MethodBuilder* builder, ORef maybeLoc) {
    auto const loc = MethodBuilderLoc::fromORef(state, maybeLoc);

    builder->filenameCount += builder->codeCount - builder->prevMaybeLocRevIdx;
    if (!(eq(loc.maybeFilename, builder->maybeFilename))) {
        builder->revFilenameRuns.push(Fixnum{(int64_t)builder->filenameCount});
        builder->revFilenameRuns.push(builder->maybeFilename);

        builder->maybeFilename = loc.maybeFilename;
        builder->filenameCount = 0;
    }

    if (loc.srcIdx != builder->srcIdx) {
        auto const srcIdxDelta = (int64_t)builder->srcIdx - (int64_t)loc.srcIdx;
        encodeRevDelta(builder->revIdxDeltas, srcIdxDelta, requiredBitsize(srcIdxDelta));
        auto const codeByteRevIdxDelta =
            (int64_t)(builder->codeCount - builder->prevDeltaCodeByteRevIdx);
        encodeRevDelta(builder->revIdxDeltas, codeByteRevIdxDelta,
                       requiredBitsize(codeByteRevIdxDelta));

        builder->srcIdx = loc.srcIdx;
        builder->prevDeltaCodeByteRevIdx = builder->codeCount;
    }

    builder->prevMaybeLocRevIdx = builder->codeCount;
}

void flushMethodBuilderDeltas(MethodBuilder& builder) {
    builder.revFilenameRuns.push(Fixnum{(int64_t)builder.filenameCount});
    builder.revFilenameRuns.push(builder.maybeFilename);
}

void pushCodeByte(Compiler* compiler, MethodBuilder* builder, uint8_t byte) {
    if (builder->codeCount == builder->codeCap) {
        size_t const newCap = builder->codeCap + (builder->codeCap >> 1);
        builder->code = (uint8_t*)arealloc(&compiler->arena, builder->code,
                                 builder->codeCap * sizeof *builder->code,
                                 newCap * sizeof *builder->code);
        builder->codeCap = newCap;
    }

    builder->code[builder->codeCount++] = byte;
}

inline void pushOp(
    State const& state, Compiler* compiler, MethodBuilder* builder, Opcode op, ORef maybeLoc
) {
    pushCodeByte(compiler, builder, (uint8_t)op);
    pushMaybeLoc(state, builder, maybeLoc);
}

inline void pushReg(Compiler* compiler, MethodBuilder* builder, IRName name) {
    pushCodeByte(compiler, builder, (uint8_t)(name.index));
}

inline void pushDisplacement(
    Compiler* compiler, MethodBuilder* builder, size_t displacement
) {
    assert(displacement <= UINT16_MAX); // TODO: Enable even bigger displacements
    pushCodeByte(compiler, builder, (uint8_t)(displacement & UINT8_MAX));
    pushCodeByte(compiler, builder, (uint8_t)((displacement >> UINT8_WIDTH) & UINT8_MAX));
}

void emitRegBits(
    Compiler* compiler, MethodBuilder* builder, IRName const* names, size_t count,
    bool specializeHack
) {
    // OPTIMIZE: Use `&compiler->arena`:
    BytefulBitSet bits = newBytefulBitSet(count); // Need at least `count` bits, likely more

    // Set bits for each register:
    for (size_t i = 0; i < count; ++i) {
        size_t const regIdx = names[i].index;
        // FIXME: Hack for `specialize`, assumes that r0 cannot happen:
        if (!specializeHack || regIdx != 0) {
            bytefulBitSetSet(&bits, regIdx);
        }
    }

    // Encode bitset backwards into `builder`:
    size_t const byteCount = bytefulBitSetByteCount(&bits);
    for (size_t i = byteCount; i-- > 0;) {
        pushCodeByte(compiler, builder, bytefulBitSetByte(&bits, i));
    }
    assert(byteCount < UINT8_MAX);
    pushCodeByte(compiler, builder, (uint8_t)byteCount);

    freeBytefulBitSet(&bits);
}

void emitClose(Compiler* compiler, MethodBuilder* builder, Args const* args) {
    emitRegBits(compiler, builder, args->names, args->count, false);
}

uint8_t constIndex(Compiler* compiler, MethodBuilder* builder, ORef c) {
    // Linear search is actually good since there usually aren't that many constants per fn:
    size_t const constCount = builder->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        ORef const ic = builder->consts[i];
        if (eq(ic, c)) {
            assert(i <= UINT8_MAX);
            return (uint8_t)i;
        }
    }

    if (builder->constCount == builder->constCap) {
        size_t const newCap = builder->constCap + builder->constCap / 2;
        builder->consts = (ORef*)arealloc(&compiler->arena, builder->consts,
                                   builder->constCap  * sizeof *builder->consts,
                                   newCap * sizeof *builder->consts);
        builder->constCap = newCap;
    }

    assert(builder->constCount <= UINT8_MAX);
    uint8_t const idx = (uint8_t)builder->constCount;
    builder->consts[builder->constCount++] = c;
    return idx;
}

void emitConstArg(Compiler* compiler, MethodBuilder* builder, ORef c) {
    uint8_t const idx = constIndex(compiler, builder, c);
    pushCodeByte(compiler, builder, idx);
}

void emitConstDef(
    State const& state, Compiler* compiler, MethodBuilder* builder, IRName name, ORef c,
    ORef maybeLoc
) {
    emitConstArg(compiler, builder, c);
    pushReg(compiler, builder, name);
    pushOp(state, compiler, builder, OP_CONST, maybeLoc);
}

// Emit Bytecode over IR into Builder
// =================================================================================================

HRef<Method> emitMethod(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* parentBuilder,
    IRFn const* fn);

void emitStmt(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* builder, IRStmt* stmt
) {
    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: {
        GlobalDef const* const globalDef = &stmt->globalDef;

        pushReg(compiler, builder, globalDef->val);
        emitConstArg(compiler, builder, globalDef->name.oref());
        pushOp(*state, compiler, builder, OP_DEF, stmt->maybeLoc);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal const* const global = &stmt->global;

        emitConstArg(compiler, builder, global->name.oref());
        pushReg(compiler, builder, global->tmpName);
        pushOp(*state, compiler, builder, OP_GLOBAL, stmt->maybeLoc);
    }; break;

    case IRStmt::CONST_DEF: {
        ConstDef const* const constDef = &stmt->constDef;
        emitConstDef(*state, compiler, builder, constDef->name, constDef->v, stmt->maybeLoc);
    }; break;

    case IRStmt::METHOD_DEF: {
        MethodDef const* const methodDef = &stmt->methodDef;
        IRFn const* const fn = &methodDef->fn;

        HRef<Method> const method = emitMethod(state, compiler, toplevelFn, builder, fn);

        if (fn->domain.count == 0) {
            emitConstDef(*state, compiler, builder, methodDef->name, method.oref(), stmt->maybeLoc);
        } else {
            emitRegBits(compiler, builder, fn->domain.vals, fn->domain.count, true);
            emitConstArg(compiler, builder, method.oref());
            pushReg(compiler, builder, methodDef->name);
            pushOp(*state, compiler, builder, OP_SPECIALIZE, stmt->maybeLoc);
        }
    }; break;

    case IRStmt::CLOSURE: {
        IRClosure const* const closure = &stmt->closure;
        emitClose(compiler, builder, closure->closes);
        pushReg(compiler, builder, closure->method);
        pushReg(compiler, builder, closure->name);
        pushOp(*state, compiler, builder, OP_CLOSURE, stmt->maybeLoc);
    }; break;

    case IRStmt::CLOVER: {
        Clover const* const clover = &stmt->clover;
        pushCodeByte(compiler, builder, clover->idx);
        pushReg(compiler, builder, clover->closure);
        pushReg(compiler, builder, clover->name);
        pushOp(*state, compiler, builder, OP_CLOVER, stmt->maybeLoc);
    }; break;

    case IRStmt::MOVE: {
        MoveStmt const* const mov = &stmt->mov;
        pushReg(compiler, builder, mov->src);
        pushReg(compiler, builder, mov->dest);
        pushOp(*state, compiler, builder, OP_MOVE, stmt->maybeLoc);
    }; break;

    case IRStmt::SWAP: {
        SwapStmt const* const swap = &stmt->swap;
        pushReg(compiler, builder, swap->reg2);
        pushReg(compiler, builder, swap->reg1);
        pushOp(*state, compiler, builder, OP_SWAP, stmt->maybeLoc);
    }; break;

    case IRStmt::KNOT: {
        KnotStmt const* const knot = &stmt->knot;
        pushReg(compiler, builder, knot->name);
        pushOp(*state, compiler, builder, OP_KNOT, stmt->maybeLoc);
    }; break;

    case IRStmt::KNOT_INIT: {
        KnotInitStmt const* const knotInit = &stmt->knotInit;
        pushReg(compiler, builder, knotInit->v);
        pushReg(compiler, builder, knotInit->knot);
        pushOp(*state, compiler, builder, OP_KNOT_INIT, stmt->maybeLoc);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt const* const knotGet = &stmt->knotGet;
        pushReg(compiler, builder, knotGet->knot);
        pushReg(compiler, builder, knotGet->name);
        pushOp(*state, compiler, builder, OP_KNOT_GET, stmt->maybeLoc);
    }; break;
    }
}

void emitTransfer(
    State const& state, Compiler* compiler, MethodBuilder* builder, IRTransfer const* transfer
) {
    switch (transfer->type) {
    case IRTransfer::CALL: {
        Call const* const call = &transfer->call;

        // Guaranteed not to need an `OP_BR` to return block here.

        emitClose(compiler, builder, &call->closes);

        size_t const regCount = 2 + call->args.count;
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        pushCodeByte(compiler, builder, (uint8_t)regCount);

        pushOp(state, compiler, builder, OP_CALL, transfer->maybeLoc);
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall const* const tailcall = &transfer->tailcall;

        size_t const regCount = 2 + tailcall->args.count;
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        pushCodeByte(compiler, builder, (uint8_t)regCount);

        pushOp(state, compiler, builder, OP_TAILCALL, transfer->maybeLoc);
    }; break;

    case IRTransfer::IF: {
        IRIf const* const iff = &transfer->iff;

        size_t const postIndex = builder->codeCount - 1;
        size_t const destIndex = getLabelIndex(&builder->labelIdxs, iff->alt);
        size_t const displacement = postIndex - destIndex;

        pushDisplacement(compiler, builder, displacement);
        pushReg(compiler, builder, iff->cond);
        pushOp(state, compiler, builder, OP_BRF, transfer->maybeLoc);
    }; break;

    case IRTransfer::GOTO: {
        IRGoto const* const gotoo = &transfer->gotoo;

        size_t const postIndex = builder->codeCount - 1;
        size_t const destIndex = getLabelIndex(&builder->labelIdxs, gotoo->dest);
        size_t const displacement = postIndex - destIndex;

        if (displacement > 0) { // Only emit branches that actually jump a distance.
            pushDisplacement(compiler, builder, displacement);
            pushOp(state, compiler, builder, OP_BR, transfer->maybeLoc);
        }
    }; break;

    case IRTransfer::RETURN: {
        pushOp(state, compiler, builder, OP_RET, transfer->maybeLoc);
    }; break;
    }
}

void emitBlock(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* builder, IRBlock const* block
) {
    emitTransfer(*state, compiler, builder, &block->transfer);

    for (size_t i = block->stmts.count; i-- > 0;) {
        emitStmt(state, compiler, toplevelFn, builder, &block->stmts.vals[i]);
    }

    setLabelIndex(&builder->labelIdxs, block->label, builder->codeCount - 1);
}

HRef<Method> emitMethod(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* parentBuilder, IRFn const* fn
) {
    MethodBuilder builder = createMethodBuilder(*state, compiler, parentBuilder, *fn);

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    for (size_t i = fn->blockCount; i-- > 0;) {
        emitBlock(state, compiler, toplevelFn, &builder, fn->blocks[i]);
    }

    return buildMethod(state, toplevelFn, std::move(builder), fn);
}

HRef<Method> emitToplevelMethod(State* state, Compiler* compiler, IRFn* fn) {
    return emitMethod(state, compiler, fn, nullptr, fn);
}

}
