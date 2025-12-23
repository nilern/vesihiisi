#include <string.h>

#include "compiler.h"
#include "state.h"
#include "bytecode.h"
#include "bytefulbitset.h"

// TODO: Delay const index allocation to here, especially when we get dead code elimination.

typedef struct LabelIdxs {
    size_t* idxs;
} LabelIdxs;

inline static void freeLabelIdxs(LabelIdxs* labelIdxs) { free(labelIdxs->idxs); }

static LabelIdxs createLabelIdxs(size_t blockCount) {
    size_t* const idxs = malloc(blockCount * sizeof *idxs);
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
} MethodBuilder;

static MethodRef buildMethod(
    State* state, IRFn* toplevelFn, MethodBuilder builder, IRFn const* fn
) {
    // Allocate method code:
    Fixnum const codeCount = tagInt((intptr_t)builder.codeCount);
    uint8_t* maybeCode = tryAllocByteArray(state, codeCount);
    if (mustCollect(maybeCode)) {
        collectTracingIR(state, toplevelFn);
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
    Fixnum const constCount = tagInt((intptr_t)fn->constCount);
    ORef* maybeConsts = tryAllocArray(state, constCount);
    if (mustCollect(maybeConsts)) {
        collectTracingIR(state, toplevelFn);
        maybeConsts = allocArrayOrDie(state, constCount);
    }
    ArrayRef consts = tagArray(maybeConsts);
    pushStackRoot(state, (ORef*)&consts);
    memcpy(maybeConsts, fn->consts, fn->constCount * sizeof *fn->consts); // Initialize

    size_t const arity = fn->blocks[0]->paramCount - 2;
    Fixnum const fxArity = tagInt((intptr_t)arity);
    Bool const hasVarArg = tagBool(fn->hasVarArg);
    uintptr_t const hash =
        fnv1aHash_n((char const*)toPtr(code), (uintptr_t)fixnumToInt(flexLength(toORef(code))));
    Fixnum const fxHash = tagInt((intptr_t)hash);
    Method* maybeMethod = tryAllocBytecodeMethod(state, code, consts, fxArity, hasVarArg, fxHash);
    if (mustCollect(maybeMethod)) {
        collectTracingIR(state, toplevelFn);
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

    free(builder.code);
    freeLabelIdxs(&builder.labelIdxs);

    popStackRoots(state, 2);
    return method;
}

static MethodBuilder createMethodBuilder(size_t blockCount) {
    size_t const codeCap = 2;
    uint8_t* const code = malloc(codeCap * sizeof *code);

    return (MethodBuilder){
        .code = code,
        .codeCount = 0,
        .codeCap = codeCap,
        .labelIdxs = createLabelIdxs(blockCount)
    };
}

static void pushCodeByte(MethodBuilder* builder, uint8_t byte) {
    if (builder->codeCount == builder->codeCap) {
        size_t const newCap = builder->codeCap + (builder->codeCap >> 1);
        builder->code = realloc(builder->code, newCap * sizeof *builder->code);
        builder->codeCap = newCap;
    }

    builder->code[builder->codeCount++] = byte;
}

inline static void pushOp(MethodBuilder* builder, Opcode op) { pushCodeByte(builder, (uint8_t)op); }

inline static void pushReg(MethodBuilder* builder, IRName name) {
    pushCodeByte(builder, (uint8_t)(name.index));
}

inline static void pushDisplacement(MethodBuilder* builder, size_t displacement) {
    pushCodeByte(builder, (uint8_t)displacement); // FIXME: `displacement` may not fit in one byte
}

static void emitRegBits(
    MethodBuilder* builder, IRName const* names, size_t count, bool specializeHack
) {
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
        pushCodeByte(builder, bytefulBitSetByte(&bits, i));
    }
    assert(byteCount < UINT8_MAX);
    pushCodeByte(builder, (uint8_t)byteCount);

    freeBytefulBitSet(&bits);
}

static void emitClose(MethodBuilder* builder, Args const* args) {
    emitRegBits(builder, args->names, args->count, false);
}

static void emitConst(MethodBuilder* builder, IRName name, IRConst c) {
    pushCodeByte(builder, c.index);
    pushReg(builder, name);
    pushOp(builder, OP_CONST);
}

static MethodRef emitMethod(State* state, IRFn* toplevelFn, IRFn* fn);

static void emitStmt(
    State* state, IRFn* toplevelFn, MethodBuilder* builder, IRFn* fn, IRStmt* stmt
) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: {
        pushReg(builder, stmt->globalDef.val);
        pushCodeByte(builder, stmt->globalDef.name.index);
        pushOp(builder, OP_DEF);
    }; break;

    case STMT_GLOBAL: {
        pushCodeByte(builder, stmt->global.name.index);
        pushReg(builder, stmt->global.tmpName);
        pushOp(builder, OP_GLOBAL);
    }; break;

    case STMT_CONST_DEF: emitConst(builder, stmt->constDef.name, stmt->constDef.v); break;

    case STMT_METHOD_DEF: {
        MethodRef const method = emitMethod(state, toplevelFn, &stmt->methodDef.fn);
        setFnConst(fn, stmt->methodDef.v, methodToORef(method));

        if (stmt->methodDef.fn.domain.count == 0) {
            emitConst(builder, stmt->methodDef.name, stmt->methodDef.v);
        } else {
            emitRegBits(builder, stmt->methodDef.fn.domain.vals, stmt->methodDef.fn.domain.count,
                        true);
            pushCodeByte(builder, stmt->methodDef.v.index);
            pushReg(builder, stmt->methodDef.name);
            pushOp(builder, OP_SPECIALIZE);
        }
    }; break;

    case STMT_CLOSURE: {
        emitClose(builder, stmt->closure.closes);
        pushReg(builder, stmt->closure.method);
        pushReg(builder, stmt->closure.name);
        pushOp(builder, OP_CLOSURE);
    }; break;

    case STMT_CLOVER: {
        pushCodeByte(builder, stmt->clover.idx);
        pushReg(builder, stmt->clover.closure);
        pushReg(builder, stmt->clover.name);
        pushOp(builder, OP_CLOVER);
    }; break;

    case STMT_MOVE: {
        pushReg(builder, stmt->mov.src);
        pushReg(builder, stmt->mov.dest);
        pushOp(builder, OP_MOVE);
    }; break;

    case STMT_SWAP: {
        pushReg(builder, stmt->swap.reg2);
        pushReg(builder, stmt->swap.reg1);
        pushOp(builder, OP_SWAP);
    }; break;
    }
}

static void emitTransfer(MethodBuilder* builder, IRTransfer const* transfer) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        // Guaranteed not to need an `OP_BR` to return block here.

        emitClose(builder, &transfer->call.closes);

        size_t const regCount = 2 + transfer->call.args.count;
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        pushCodeByte(builder, (uint8_t)regCount);

        pushOp(builder, OP_CALL);
    }; break;

    case TRANSFER_TAILCALL: {
        size_t const regCount = 2 + transfer->tailcall.args.count;
        assert(regCount < UINT8_MAX); // TODO: Handle absurd argument count (probably too late here)
        pushCodeByte(builder, (uint8_t)regCount);

        pushOp(builder, OP_TAILCALL);
    }; break;

    case TRANSFER_IF: {
        size_t const postIndex = builder->codeCount - 1;
        size_t const destIndex = getLabelIndex(&builder->labelIdxs, transfer->iff.alt);
        size_t const displacement = postIndex - destIndex;

        pushDisplacement(builder, displacement);
        pushReg(builder, transfer->iff.cond);
        pushOp(builder, OP_BRF);
    }; break;

    case TRANSFER_GOTO: {
        size_t const postIndex = builder->codeCount - 1;
        size_t const destIndex = getLabelIndex(&builder->labelIdxs, transfer->gotoo.dest);
        size_t const displacement = postIndex - destIndex;

        if (displacement > 0) { // Only emit branches that actually jump a distance.
            pushDisplacement(builder, displacement);
            pushOp(builder, OP_BR);
        }
    }; break;

    case TRANSFER_RETURN: {
        pushOp(builder, OP_RET);
    }; break;
    }
}

static void emitBlock(
    State* state, IRFn* toplevelFn, MethodBuilder* builder, IRFn* fn, IRBlock const* block
) {
    emitTransfer(builder, &block->transfer);

    for (size_t i = block->stmts.count; i-- > 0;) {
        emitStmt(state, toplevelFn, builder, fn, &block->stmts.vals[i]);
    }

    // TODO: Handle block params? At least call entry block, for encoding arity (& domain?) into fn.

    setLabelIndex(&builder->labelIdxs, block->label, builder->codeCount - 1);
}

static MethodRef emitMethod(State* state, IRFn* toplevelFn, IRFn* fn) {
    MethodBuilder builder = createMethodBuilder(fn->blockCount);

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    for (size_t i = fn->blockCount; i-- > 0;) {
        emitBlock(state, toplevelFn, &builder, fn, fn->blocks[i]);
    }

    return buildMethod(state, toplevelFn, builder, fn);
}

inline static MethodRef emitToplevelMethod(State* state, IRFn* fn) {
    return emitMethod(state, fn, fn);
}
