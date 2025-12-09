// OPTIMIZE: keys and vals into same allocation
typedef struct LabelIdxs {
    IRName* keys;
    size_t* vals;
    size_t count;
    size_t cap;
} LabelIdxs;

static void freeLabelIdxs(LabelIdxs* labelIdxs) {
    free(labelIdxs->keys);
    free(labelIdxs->vals);
}

static LabelIdxs createLabelIdxs(void) {
    size_t const cap = 2;
    IRName* const keys = malloc(cap * sizeof *keys);
    memset(keys, 0, cap * sizeof *keys);
    size_t* const vals = malloc(cap * sizeof *vals);

    return (LabelIdxs){
        .keys = keys,
        .vals = vals,
        .count = 0,
        .cap = cap
    };
}

typedef struct FindLabelIndexIndexRes {
    size_t indexIndex;
    bool hasValue;
} FindLabelIndexIndexRes;

static FindLabelIndexIndexRes findLabelIndexIndex(LabelIdxs const* labelIdxs, IRName label) {
    size_t const h = irNameHash(label);

    size_t const maxIdx = labelIdxs->cap - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        IRName const k = labelIdxs->keys[i];

        if (irNameEq(k, label)) {
            return (FindLabelIndexIndexRes){.indexIndex = i, true};
        } else if (k.index == 0) {
            return (FindLabelIndexIndexRes){.indexIndex = i, false};
        }
    }
}

static void rehashLabelIdxs(LabelIdxs* labelIdxs) {
    size_t const oldCap = labelIdxs->cap;
    IRName* const oldKeys = labelIdxs->keys;
    size_t* const oldVals = labelIdxs->vals;

    size_t const newCap = oldCap << 1;
    IRName* const newKeys = malloc(newCap * sizeof *newKeys);
    memset(newKeys, 0, newCap * sizeof *newKeys);
    size_t* const newVals = malloc(newCap * sizeof *newVals);

    for (size_t i = 0; i < oldCap; ++i) {
        IRName const k = oldKeys[i];

        if (k.index != 0) {
            size_t const h = irNameHash(k);

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                IRName* const maybeK = newKeys + j;

                if (maybeK->index == 0) {
                    *maybeK = k;
                    newVals[j] = oldVals[i];
                    break;
                }
            }
        }
    }

    labelIdxs->keys = newKeys;
    labelIdxs->vals = newVals;
    labelIdxs->cap = newCap;

    free(oldKeys);
    free(oldVals);
}

typedef struct GetLabelIndexRes {
    size_t index;
    bool found;

} GetLabelIndexRes;

static GetLabelIndexRes getLabelIndex(LabelIdxs const* labelIdxs, IRName label) {
    FindLabelIndexIndexRes const findRes = findLabelIndexIndex(labelIdxs, label);
    if (!findRes.hasValue) { return (GetLabelIndexRes){.found = false}; }

    return (GetLabelIndexRes){
        .index = labelIdxs->vals[findRes.indexIndex],
        .found = true
    };
}

static void setLabelIndex(LabelIdxs* labelIdxs, IRName label, size_t index) {
    FindLabelIndexIndexRes findRes = findLabelIndexIndex(labelIdxs, label);
    if (findRes.hasValue) {
        labelIdxs->vals[findRes.indexIndex] = index;
    } else {
        size_t indexIndex = findRes.indexIndex;

        size_t const newCount = labelIdxs->count + 1;
        if (newCount > (labelIdxs->cap >> 1)) {
            rehashLabelIdxs(labelIdxs);
            findRes = findLabelIndexIndex(labelIdxs, label);
            assert(!findRes.hasValue);
            indexIndex = findRes.indexIndex;
        }

        labelIdxs->keys[indexIndex] = label;
        labelIdxs->vals[indexIndex] = index;
        labelIdxs->count = newCount;
    }
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

    MethodRef const method = createMethod(state, code, consts);

    free(builder.code);
    freeLabelIdxs(&builder.labelIdxs);
    return method;
}

static MethodBuilder createMethodBuilder(void) {
    size_t const codeCap = 2;
    uint8_t* const code = malloc(codeCap * sizeof *code);

    return (MethodBuilder){
        .code = code,
        .codeCount = 0,
        .codeCap = codeCap,
        .labelIdxs = createLabelIdxs()
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
    pushCodeByte(builder, (uint8_t)(name.index - 2)); // HACK; FIXME
}

inline static void pushDisplacement(MethodBuilder* builder, size_t displacement) {
    pushCodeByte(builder, (uint8_t)displacement); // FIXME: `displacement` may not fit in one byte
}

static void emitStmt(MethodBuilder* builder, IRStmt const* stmt) {
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
        assert(false); // TODO
    }; break;
    }
}

static void emitTransfer(MethodBuilder* builder, IRTransfer const* transfer) {
    switch (transfer->type) {
    case TRANSFER_CALL: // fallthrough
    case TRANSFER_TAILCALL: assert(false); break; // TODO

    case TRANSFER_IF: {
        size_t const postIndex = builder->codeCount - 1;
        GetLabelIndexRes const getRes = getLabelIndex(&builder->labelIdxs, transfer->iff.alt);
        assert(getRes.found);
        size_t const destIndex = getRes.index;
        size_t const displacement = postIndex - destIndex;
        pushDisplacement(builder, displacement);
        pushReg(builder, transfer->iff.cond);
        pushOp(builder, OP_BRF);
    }; break;

    case TRANSFER_GOTO: {
        size_t const postIndex = builder->codeCount - 1;
        GetLabelIndexRes const getRes = getLabelIndex(&builder->labelIdxs, transfer->gotoo.dest);
        assert(getRes.found);
        size_t const destIndex = getRes.index;
        size_t const displacement = postIndex - destIndex;
        if (displacement > 0) {
            pushDisplacement(builder, displacement);
            pushOp(builder, OP_BR);
        }
    }; break;

    case TRANSFER_RETURN: {
        pushReg(builder, transfer->ret.arg);
        pushReg(builder, transfer->ret.callee);
        pushOp(builder, OP_RET);
    }; break;
    }
}

static void emitBlock(MethodBuilder* builder, IRBlock const* block) {
    emitTransfer(builder, &block->transfer);

    for (size_t i = block->stmtCount; i-- > 0;) {
        emitStmt(builder, &block->stmts[i]);
    }

    // TODO: Handle block params?

    setLabelIndex(&builder->labelIdxs, block->label, builder->codeCount - 1);
}

static MethodRef emitMethod(State* state, IRFn const* fn) {
    MethodBuilder builder = createMethodBuilder();

    // Thanks to previous passes, CFG DAG blocks are conveniently in reverse post-order:
    for (size_t i = fn->blockCount; i-- > 0;) {
        emitBlock(&builder, fn->blocks[i]);
    }

    return buildMethod(state, builder, fn);
}
