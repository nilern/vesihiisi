typedef struct MethodBuilder {
    uint8_t* code;
    size_t codeCount;
    size_t codeCap;
} MethodBuilder;

// FIXME: Will break horribly when some allocation causes a GC here:
static MethodRef buildMethod(State* state, MethodBuilder builder, IRFn const* fn) {
    ByteArrayRef const code = createByteArray(state, tagInt((intptr_t)builder.codeCount));
    memcpy(byteArrayToPtr(code), builder.code, builder.codeCount * sizeof *builder.code);

    ArrayRef const consts = createArray(state, tagInt((intptr_t)fn->constCount));
    memcpy(arrayToPtr(consts), fn->consts, fn->constCount * sizeof *fn->consts);

    MethodRef const method = createMethod(state, code, consts);

    free(builder.code);
    return method;
}

static MethodBuilder createMethodBuilder(void) {
    size_t const codeCap = 2;
    uint8_t* const code = malloc(codeCap * sizeof *code);

    return (MethodBuilder){
        .code = code,
        .codeCount = 0,
        .codeCap = codeCap
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

inline static void pushArg(MethodBuilder* builder, IRName name) {
    pushCodeByte(builder, (uint8_t)(name.index - 1)); // HACK; FIXME
}

static void emitStmt(MethodBuilder* builder, IRStmt const* stmt) {
    switch (stmt->type) {
    case STMT_CONST_DEF:
        pushOp(builder, OP_CONST);
        pushArg(builder, stmt->constDef.name);
        pushCodeByte(builder, stmt->constDef.v.index);
        break;
    }
}

static void emitTransfer(MethodBuilder* builder, IRTransfer const* transfer) {
    switch (transfer->type) {
    case TRANSFER_RETURN:
        pushOp(builder, OP_RET);
        pushArg(builder, transfer->ret.callee);
        pushArg(builder, transfer->ret.arg);
        break;
    }
}

static void emitBlock(MethodBuilder* builder, IRBlock const* block) {
    // TODO: Handle block params

    size_t const stmtCount = block->stmtCount;
    for (size_t i = 0; i < stmtCount; ++i) {
        emitStmt(builder, &block->stmts[i]);
    }

    emitTransfer(builder, &block->transfer);
}

static MethodRef emitMethod(State* state, IRFn const* fn) {
    MethodBuilder builder = createMethodBuilder();

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        emitBlock(&builder, fn->blocks[i]);
    }

    return buildMethod(state, builder, fn);
}
