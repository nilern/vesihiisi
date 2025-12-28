#include <string.h>

#include "compiler.h"
#include "state.h"
#include "bytecode.h"
#include "bytefulbitset.h"

typedef struct LabelIdxs {
    size_t* idxs;
} LabelIdxs;

static LabelIdxs createLabelIdxs(Compiler* compiler, size_t blockCount) {
    size_t* const idxs = amalloc(&compiler->arena, blockCount * sizeof *idxs);
    return (LabelIdxs){.idxs = idxs};
}

inline static size_t getLabelIndex(LabelIdxs const* labelIdxs, IRLabel label) {
    return labelIdxs->idxs[label.blockIndex];
}

inline static void setLabelIndex(LabelIdxs* labelIdxs, IRLabel label, size_t index) {
    labelIdxs->idxs[label.blockIndex] = index;
}

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

static void markMethodBuilder(State* state, MethodBuilder* builder) {
    size_t const constCount = builder->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        builder->consts[i] = mark(&state->heap, builder->consts[i]);
    }

    if (builder->parent) { markMethodBuilder(state, builder->parent); }
}

[[maybe_unused]]
static void assertMethodBuilderInTospace(State const* state,MethodBuilder const* builder) {
    size_t const constCount = builder->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        ORef const v = builder->consts[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    if (builder->parent) { assertMethodBuilderInTospace(state, builder->parent); }
}

static MethodRef buildMethod(
    State* state, IRFn* toplevelFn, MethodBuilder builder, IRFn const* fn
) {
    // Allocate method code:
    Fixnum const codeCount = tagInt((intptr_t)builder.codeCount);
    uint8_t* maybeCode = tryAllocByteArray(state, codeCount);
    if (mustCollect(maybeCode)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeCode = allocByteArrayOrDie(state, codeCount);
    }
    ByteArrayRef code = tagByteArray(maybeCode);
    pushStackRoot(state, (ORef*)&code);

    { // Initialize method:
        uint8_t* codePtr = maybeCode;
        for (size_t i = builder.codeCount; i-- > 0; ++codePtr) {
            *codePtr = builder.code[i];
        }
    }

    // Create method consts:
    Fixnum const constCount = tagInt((intptr_t)builder.constCount);
    ORef* maybeConsts = tryAllocArray(state, constCount);
    if (mustCollect(maybeConsts)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeConsts = allocArrayOrDie(state, constCount);
    }
    ArrayRef consts = tagArray(maybeConsts);
    pushStackRoot(state, (ORef*)&consts);
    memcpy(maybeConsts, builder.consts, builder.constCount * sizeof *builder.consts); // Initialize

    size_t const arity = fn->blocks[0]->paramCount - 2;
    Fixnum const fxArity = tagInt((intptr_t)arity);
    Bool const hasVarArg = tagBool(fn->hasVarArg);
    uintptr_t const hash =
        fnv1aHash_n((char const*)toPtr(code), (uintptr_t)fixnumToInt(flexLength(toORef(code))));
    Fixnum const fxHash = tagInt((intptr_t)hash);
    Method* maybeMethod = tryAllocBytecodeMethod(state, code, consts, fxArity, hasVarArg, fxHash);
    if (mustCollect(maybeMethod)) {
        collectTracingIR(state, toplevelFn, &builder);
        maybeMethod = allocBytecodeMethodOrDie(state, code, consts, fxArity, hasVarArg, fxHash);
    }
    if (fn->domain.count == 0) {
        for (size_t i = 0; i < arity; ++i) {
            maybeMethod->domain[i] = toORef(state->anyType);
        }
    } else {
        for (size_t i = 0; i < arity; ++i) {
            IRName const typeName = fn->domain.vals[i];
            if (!irNameIsValid(typeName)) {
                maybeMethod->domain[i] = toORef(state->anyType);
            } // else leave zeroed for specialization to fill in
        }
    }
    MethodRef const method = tagMethod(maybeMethod);

    popStackRoots(state, 2);
    return method;
}

static MethodBuilder createMethodBuilder(
    Compiler* compiler, MethodBuilder* parent, size_t blockCount
) {
    size_t const codeCap = 2;
    uint8_t* const code = amalloc(&compiler->arena, codeCap * sizeof *code);

    size_t const constCap = 2;
    ORef* const consts = amalloc(&compiler->arena, constCap * sizeof *consts);

    return (MethodBuilder){
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

static void pushCodeByte(Compiler* compiler, MethodBuilder* builder, uint8_t byte) {
    if (builder->codeCount == builder->codeCap) {
        size_t const newCap = builder->codeCap + (builder->codeCap >> 1);
        builder->code = arealloc(&compiler->arena, builder->code,
                                 builder->codeCap * sizeof *builder->code,
                                 newCap * sizeof *builder->code);
        builder->codeCap = newCap;
    }

    builder->code[builder->codeCount++] = byte;
}

inline static void pushOp(Compiler* compiler, MethodBuilder* builder, Opcode op) {
    pushCodeByte(compiler, builder, (uint8_t)op);
}

inline static void pushReg(Compiler* compiler, MethodBuilder* builder, IRName name) {
    pushCodeByte(compiler, builder, (uint8_t)(name.index));
}

inline static void pushDisplacement(Compiler* compiler, MethodBuilder* builder, size_t displacement) {
    pushCodeByte(compiler, builder, (uint8_t)displacement); // FIXME: `displacement` may not fit in one byte
}

static void emitRegBits(
    Compiler* compiler, MethodBuilder* builder, IRName const* names, size_t count, bool specializeHack
) {
    // OPTIMIZE: Use `&compiler->arena`:
    BytefulBitSet bits = newBytefulBitSet(count); // Need at least `count` bits, likely more

    // Set bits for each register:
    for (size_t i = 0; i < count; ++i) {
        size_t const regIdx = names[i].index;
        if (!specializeHack || regIdx != 0) { // FIXME: Hack for `specialize`, assumes that r0 cannot happen
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

static void emitClose(Compiler* compiler, MethodBuilder* builder, Args const* args) {
    emitRegBits(compiler, builder, args->names, args->count, false);
}

static uint8_t constIndex(Compiler* compiler, MethodBuilder* builder, ORef c) {
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
        builder->consts = arealloc(&compiler->arena, builder->consts,
                                   builder->constCap  * sizeof *builder->consts,
                                   newCap * sizeof *builder->consts);
        builder->constCap = newCap;
    }

    assert(builder->constCount <= UINT8_MAX);
    uint8_t const idx = (uint8_t)builder->constCount;
    builder->consts[builder->constCount++] = c;
    return idx;
}

static void emitConstArg(Compiler* compiler, MethodBuilder* builder, ORef c) {
    uint8_t const idx = constIndex(compiler, builder, c);
    pushCodeByte(compiler, builder, idx);
}

static void emitConstDef(Compiler* compiler, MethodBuilder* builder, IRName name, ORef c) {
    emitConstArg(compiler, builder, c);
    pushReg(compiler, builder, name);
    pushOp(compiler, builder, OP_CONST);
}

static MethodRef emitMethod(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* parentBuilder, IRFn* fn);

static void emitStmt(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* builder, IRStmt* stmt
) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: {
        pushReg(compiler, builder, stmt->globalDef.val);
        emitConstArg(compiler, builder, toORef(stmt->globalDef.name));
        pushOp(compiler, builder, OP_DEF);
    }; break;

    case STMT_GLOBAL: {
        emitConstArg(compiler, builder, toORef(stmt->global.name));
        pushReg(compiler, builder, stmt->global.tmpName);
        pushOp(compiler, builder, OP_GLOBAL);
    }; break;

    case STMT_CONST_DEF: {
        emitConstDef(compiler, builder, stmt->constDef.name, stmt->constDef.v);
    }; break;

    case STMT_METHOD_DEF: {
        MethodRef const method =
            emitMethod(state, compiler, toplevelFn, builder, &stmt->methodDef.fn);

        if (stmt->methodDef.fn.domain.count == 0) {
            emitConstDef(compiler, builder, stmt->methodDef.name, toORef(method));
        } else {
            emitRegBits(compiler, builder, stmt->methodDef.fn.domain.vals, stmt->methodDef.fn.domain.count,
                        true);
            emitConstArg(compiler, builder, toORef(method));
            pushReg(compiler, builder, stmt->methodDef.name);
            pushOp(compiler, builder, OP_SPECIALIZE);
        }
    }; break;

    case STMT_CLOSURE: {
        emitClose(compiler, builder, stmt->closure.closes);
        pushReg(compiler, builder, stmt->closure.method);
        pushReg(compiler, builder, stmt->closure.name);
        pushOp(compiler, builder, OP_CLOSURE);
    }; break;

    case STMT_CLOVER: {
        pushCodeByte(compiler, builder, stmt->clover.idx);
        pushReg(compiler, builder, stmt->clover.closure);
        pushReg(compiler, builder, stmt->clover.name);
        pushOp(compiler, builder, OP_CLOVER);
    }; break;

    case STMT_MOVE: {
        pushReg(compiler, builder, stmt->mov.src);
        pushReg(compiler, builder, stmt->mov.dest);
        pushOp(compiler, builder, OP_MOVE);
    }; break;

    case STMT_SWAP: {
        pushReg(compiler, builder, stmt->swap.reg2);
        pushReg(compiler, builder, stmt->swap.reg1);
        pushOp(compiler, builder, OP_SWAP);
    }; break;

    case STMT_KNOT: {
        pushReg(compiler, builder, stmt->knot.name);
        pushOp(compiler, builder, OP_KNOT);
    }; break;

    case STMT_KNOT_INIT: {
        pushReg(compiler, builder, stmt->knotInit.v);
        pushReg(compiler, builder, stmt->knotInit.knot);
        pushOp(compiler, builder, OP_KNOT_INIT);
    }; break;

    case STMT_KNOT_GET: {
        pushReg(compiler, builder, stmt->knotGet.knot);
        pushReg(compiler, builder, stmt->knotGet.name);
        pushOp(compiler, builder, OP_KNOT_GET);
    }; break;
    }
}

static void emitTransfer(Compiler* compiler, MethodBuilder* builder, IRTransfer const* transfer) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        // Guaranteed not to need an `OP_BR` to return block here.

        emitClose(compiler, builder, &transfer->call.closes);

        size_t const regCount = 2 + transfer->call.args.count;
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        pushCodeByte(compiler, builder, (uint8_t)regCount);

        pushOp(compiler, builder, OP_CALL);
    }; break;

    case TRANSFER_TAILCALL: {
        size_t const regCount = 2 + transfer->tailcall.args.count;
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        pushCodeByte(compiler, builder, (uint8_t)regCount);

        pushOp(compiler, builder, OP_TAILCALL);
    }; break;

    case TRANSFER_IF: {
        size_t const postIndex = builder->codeCount - 1;
        size_t const destIndex = getLabelIndex(&builder->labelIdxs, transfer->iff.alt);
        size_t const displacement = postIndex - destIndex;

        pushDisplacement(compiler, builder, displacement);
        pushReg(compiler, builder, transfer->iff.cond);
        pushOp(compiler, builder, OP_BRF);
    }; break;

    case TRANSFER_GOTO: {
        size_t const postIndex = builder->codeCount - 1;
        size_t const destIndex = getLabelIndex(&builder->labelIdxs, transfer->gotoo.dest);
        size_t const displacement = postIndex - destIndex;

        if (displacement > 0) { // Only emit branches that actually jump a distance.
            pushDisplacement(compiler, builder, displacement);
            pushOp(compiler, builder, OP_BR);
        }
    }; break;

    case TRANSFER_RETURN: {
        pushOp(compiler, builder, OP_RET);
    }; break;
    }
}

static void emitBlock(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* builder, IRBlock const* block
) {
    emitTransfer(compiler, builder, &block->transfer);

    for (size_t i = block->stmts.count; i-- > 0;) {
        emitStmt(state, compiler, toplevelFn, builder, &block->stmts.vals[i]);
    }

    // TODO: Handle block params? At least call entry block, for encoding arity (& domain?) into fn.

    setLabelIndex(&builder->labelIdxs, block->label, builder->codeCount - 1);
}

static MethodRef emitMethod(
    State* state, Compiler* compiler, IRFn* toplevelFn, MethodBuilder* parentBuilder, IRFn* fn
) {
    MethodBuilder builder = createMethodBuilder(compiler, parentBuilder, fn->blockCount);

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    for (size_t i = fn->blockCount; i-- > 0;) {
        emitBlock(state, compiler, toplevelFn, &builder, fn->blocks[i]);
    }

    return buildMethod(state, toplevelFn, builder, fn);
}

inline static MethodRef emitToplevelMethod(State* state, Compiler* compiler, IRFn* fn) {
    return emitMethod(state, compiler, fn, nullptr, fn);
}
