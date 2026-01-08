#include "bytecodegen.hpp"

#include <string.h>

#include "../state.hpp"
#include "../bytecode.hpp"
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

typedef struct MethodBuilder {
    uint8_t* code;
    size_t codeCount;
    size_t codeCap;

    LabelIdxs labelIdxs;

    ORef* consts;
    size_t constCount;
    size_t constCap;

    struct MethodBuilder* parent;
} MethodBuilder;

void markMethodBuilder(State* state, MethodBuilder* builder) {
    size_t const constCount = builder->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        builder->consts[i] = state->heap.mark(builder->consts[i]);
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

    if (builder->parent) { assertMethodBuilderInTospace(state, builder->parent); }
}

HRef<Method> buildMethod(
    State* state, IRFn* toplevelFn, MethodBuilder builder, IRFn const* fn
) {
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

    size_t const arity = fn->blocks[0]->paramCount - 2;
    Fixnum const fxArity = Fixnum((intptr_t)arity);
    Bool const hasVarArg = Bool(fn->hasVarArg);
    Slice<uint8_t const> const codeSlice = code.ptr()->flexItems();
    uintptr_t const hash = fnv1aHash_n((char const*)codeSlice.data, codeSlice.count);
    Fixnum const fxHash = Fixnum((intptr_t)hash);
    Method* maybeMethod =
        tryAllocBytecodeMethod(state, code, consts, fxArity, hasVarArg, fxHash, fn->maybeName);
    if (mustCollect(maybeMethod)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeMethod = allocBytecodeMethodOrDie(state, code, consts, fxArity, hasVarArg, fxHash,
                                               fn->maybeName);
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

    popStackRoots(state, 2);
    return method;
}

MethodBuilder createMethodBuilder(
    Compiler* compiler, MethodBuilder* parent, size_t blockCount
) {
    size_t const codeCap = 2;
    uint8_t* const code = (uint8_t*)amalloc(&compiler->arena, codeCap * sizeof *code);

    size_t const constCap = 2;
    ORef* const consts = (ORef*)amalloc(&compiler->arena, constCap * sizeof *consts);

    return MethodBuilder{
        .code = code,
        .codeCount = 0,
        .codeCap = codeCap,

        .labelIdxs = createLabelIdxs(compiler, blockCount),

        .consts = consts,
        .constCount = 0,
        .constCap = constCap,

        .parent = parent
    };
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

inline void pushOp(Compiler* compiler, MethodBuilder* builder, Opcode op) {
    pushCodeByte(compiler, builder, (uint8_t)op);
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

void emitConstDef(Compiler* compiler, MethodBuilder* builder, IRName name, ORef c) {
    emitConstArg(compiler, builder, c);
    pushReg(compiler, builder, name);
    pushOp(compiler, builder, OP_CONST);
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
        pushOp(compiler, builder, OP_DEF);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal const* const global = &stmt->global;

        emitConstArg(compiler, builder, global->name.oref());
        pushReg(compiler, builder, global->tmpName);
        pushOp(compiler, builder, OP_GLOBAL);
    }; break;

    case IRStmt::CONST_DEF: {
        ConstDef const* const constDef = &stmt->constDef;
        emitConstDef(compiler, builder, constDef->name, constDef->v);
    }; break;

    case IRStmt::METHOD_DEF: {
        MethodDef const* const methodDef = &stmt->methodDef;
        IRFn const* const fn = &methodDef->fn;

        HRef<Method> const method = emitMethod(state, compiler, toplevelFn, builder, fn);

        if (fn->domain.count == 0) {
            emitConstDef(compiler, builder, methodDef->name, method.oref());
        } else {
            emitRegBits(compiler, builder, fn->domain.vals, fn->domain.count, true);
            emitConstArg(compiler, builder, method.oref());
            pushReg(compiler, builder, methodDef->name);
            pushOp(compiler, builder, OP_SPECIALIZE);
        }
    }; break;

    case IRStmt::CLOSURE: {
        IRClosure const* const closure = &stmt->closure;
        emitClose(compiler, builder, closure->closes);
        pushReg(compiler, builder, closure->method);
        pushReg(compiler, builder, closure->name);
        pushOp(compiler, builder, OP_CLOSURE);
    }; break;

    case IRStmt::CLOVER: {
        Clover const* const clover = &stmt->clover;
        pushCodeByte(compiler, builder, clover->idx);
        pushReg(compiler, builder, clover->closure);
        pushReg(compiler, builder, clover->name);
        pushOp(compiler, builder, OP_CLOVER);
    }; break;

    case IRStmt::MOVE: {
        MoveStmt const* const mov = &stmt->mov;
        pushReg(compiler, builder, mov->src);
        pushReg(compiler, builder, mov->dest);
        pushOp(compiler, builder, OP_MOVE);
    }; break;

    case IRStmt::SWAP: {
        SwapStmt const* const swap = &stmt->swap;
        pushReg(compiler, builder, swap->reg2);
        pushReg(compiler, builder, swap->reg1);
        pushOp(compiler, builder, OP_SWAP);
    }; break;

    case IRStmt::KNOT: {
        KnotStmt const* const knot = &stmt->knot;
        pushReg(compiler, builder, knot->name);
        pushOp(compiler, builder, OP_KNOT);
    }; break;

    case IRStmt::KNOT_INIT: {
        KnotInitStmt const* const knotInit = &stmt->knotInit;
        pushReg(compiler, builder, knotInit->v);
        pushReg(compiler, builder, knotInit->knot);
        pushOp(compiler, builder, OP_KNOT_INIT);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt const* const knotGet = &stmt->knotGet;
        pushReg(compiler, builder, knotGet->knot);
        pushReg(compiler, builder, knotGet->name);
        pushOp(compiler, builder, OP_KNOT_GET);
    }; break;
    }
}

void emitTransfer(Compiler* compiler, MethodBuilder* builder, IRTransfer const* transfer) {
    switch (transfer->type) {
    case IRTransfer::CALL: {
        Call const* const call = &transfer->call;

        // Guaranteed not to need an `OP_BR` to return block here.

        emitClose(compiler, builder, &call->closes);

        size_t const regCount = 2 + call->args.count;
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        pushCodeByte(compiler, builder, (uint8_t)regCount);

        pushOp(compiler, builder, OP_CALL);
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall const* const tailcall = &transfer->tailcall;

        size_t const regCount = 2 + tailcall->args.count;
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        pushCodeByte(compiler, builder, (uint8_t)regCount);

        pushOp(compiler, builder, OP_TAILCALL);
    }; break;

    case IRTransfer::IF: {
        IRIf const* const iff = &transfer->iff;

        size_t const postIndex = builder->codeCount - 1;
        size_t const destIndex = getLabelIndex(&builder->labelIdxs, iff->alt);
        size_t const displacement = postIndex - destIndex;

        pushDisplacement(compiler, builder, displacement);
        pushReg(compiler, builder, iff->cond);
        pushOp(compiler, builder, OP_BRF);
    }; break;

    case IRTransfer::GOTO: {
        IRGoto const* const gotoo = &transfer->gotoo;

        size_t const postIndex = builder->codeCount - 1;
        size_t const destIndex = getLabelIndex(&builder->labelIdxs, gotoo->dest);
        size_t const displacement = postIndex - destIndex;

        if (displacement > 0) { // Only emit branches that actually jump a distance.
            pushDisplacement(compiler, builder, displacement);
            pushOp(compiler, builder, OP_BR);
        }
    }; break;

    case IRTransfer::RETURN: {
        pushOp(compiler, builder, OP_RET);
    }; break;
    }
}

void emitBlock(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* builder, IRBlock const* block
) {
    emitTransfer(compiler, builder, &block->transfer);

    for (size_t i = block->stmts.count; i-- > 0;) {
        emitStmt(state, compiler, toplevelFn, builder, &block->stmts.vals[i]);
    }

    setLabelIndex(&builder->labelIdxs, block->label, builder->codeCount - 1);
}

HRef<Method> emitMethod(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* parentBuilder, IRFn const* fn
) {
    MethodBuilder builder = createMethodBuilder(compiler, parentBuilder, fn->blockCount);

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    for (size_t i = fn->blockCount; i-- > 0;) {
        emitBlock(state, compiler, toplevelFn, &builder, fn->blocks[i]);
    }

    return buildMethod(state, toplevelFn, builder, fn);
}

HRef<Method> emitToplevelMethod(State* state, Compiler* compiler, IRFn* fn) {
    return emitMethod(state, compiler, fn, nullptr, fn);
}

}
