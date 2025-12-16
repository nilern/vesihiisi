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

// FIXME: Will break horribly when some allocation causes a GC here:
static MethodRef buildMethod(State* state, MethodBuilder builder, IRFn const* fn) {
    ByteArrayRef const code = createByteArray(state, tagInt((intptr_t)builder.codeCount));
    uint8_t* codePtr = byteArrayToPtr(code);
    for (size_t i = builder.codeCount; i-- > 0; ++codePtr) {
        *codePtr = builder.code[i];
    }

    ArrayRef const consts = createArray(state, tagInt((intptr_t)fn->constCount));
    memcpy(arrayToPtr(consts), fn->consts, fn->constCount * sizeof *fn->consts);

    MethodRef const method = createBytecodeMethod(state, code, consts);

    free(builder.code);
    freeLabelIdxs(&builder.labelIdxs);
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

static void emitClose(MethodBuilder* builder, Args const* args) {
    size_t const arity = args->count;
    BytefulBitSet bits = newBytefulBitSet(arity); // Need at least `arity` bits, likely more

    // Set bits for each arg:
    for (size_t i = 0; i < arity; ++i) {
        bytefulBitSetSet(&bits, args->names[i].index);
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

static MethodRef emitMethod(State* state, IRFn* fn);

static void emitStmt(State* state, MethodBuilder* builder, IRFn* fn, IRStmt* stmt) {
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

    case STMT_CONST_DEF: {
        pushCodeByte(builder, stmt->constDef.v.index);
        pushReg(builder, stmt->constDef.name);
        pushOp(builder, OP_CONST);
    }; break;

    case STMT_FN_DEF: {
        // FIXME: Makes GC issues in `buildMethod` even worse (need to treat whole compilation
        // unit as GC roots, not just the fn `buildMethod` is consuming):
        MethodRef const method = emitMethod(state, &stmt->fnDef.fn);
        setFnConst(fn, stmt->fnDef.v, methodToORef(method));

        emitClose(builder, &stmt->fnDef.closes);
        pushCodeByte(builder, stmt->fnDef.v.index);
        pushReg(builder, stmt->fnDef.name);
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

        size_t const regCount = 2 + transfer->tailcall.args.count;
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

static void emitBlock(State* state, MethodBuilder* builder, IRFn* fn, IRBlock const* block) {
    emitTransfer(builder, &block->transfer);

    for (size_t i = block->stmtCount; i-- > 0;) {
        emitStmt(state, builder, fn, &block->stmts[i]);
    }

    // TODO: Handle block params?

    setLabelIndex(&builder->labelIdxs, block->label, builder->codeCount - 1);
}

static MethodRef emitMethod(State* state, IRFn* fn) {
    MethodBuilder builder = createMethodBuilder(fn->blockCount);

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    for (size_t i = fn->blockCount; i-- > 0;) {
        emitBlock(state, &builder, fn, fn->blocks[i]);
    }

    return buildMethod(state, builder, fn);
}
