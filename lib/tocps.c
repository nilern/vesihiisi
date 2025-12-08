typedef struct ToCpsFrame {
    ORef* keys;
    IRName* vals;
    size_t count;
    size_t cap;
} ToCpsFrame;

inline static void freeToCpsFrame(ToCpsFrame* frame) { free(frame->keys); }

static ToCpsFrame createToCpsFrame(void) {
    size_t const cap = 2;
    void* const kvs = malloc(cap * (sizeof *(ToCpsFrame){}.keys + sizeof *(ToCpsFrame){}.vals));
    ORef* const keys = (ORef*)kvs;
    memset(keys, 0, cap * sizeof *keys);
    IRName* const vals = (IRName*)(keys + cap);

    return (ToCpsFrame){
        .keys = keys,
        .vals = vals,
        .count = 0,
        .cap = cap
    };
}

static void rehashToCpsFrame(ToCpsFrame* frame) {
    size_t const oldCap = frame->cap;
    ORef* const oldKeys = frame->keys;
    IRName* const oldVals = frame->vals;

    size_t const newCap = oldCap << 1;
    void* const newKvs =
        malloc(newCap * (sizeof *(ToCpsFrame){}.keys + sizeof *(ToCpsFrame){}.vals));
    ORef* const newKeys = (ORef*)newKvs;
    memset(newKeys, 0, newCap * sizeof *newKeys);
    IRName* const newVals = (IRName*)(newKeys + newCap);

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const k = oldKeys[i];

        if (!eq(k, fixnumToORef(Zero))) {
            size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(uncheckedORefToSymbol(k))->hash);

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const maybeK = newKeys + j;

                if (eq(*maybeK, fixnumToORef(Zero))) {
                    *maybeK = k;
                    newVals[j] = oldVals[i];
                    break;
                }
            }
        }
    }

    frame->keys = newKeys;
    frame->vals = newVals;
    frame->cap = newCap;

    free(oldKeys);
}

static BucketIdx toCpsFrameFindIdx(ToCpsFrame const* frame, SymbolRef sym) {
    size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(sym)->hash);

    size_t const maxIdx = frame->cap - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        ORef const k = frame->keys[i];

        if (eq(k, symbolToORef(sym))) {
            return (BucketIdx){.idx = i, .occupied = true};
        } else if (eq(k, fixnumToORef(Zero))) {
            return (BucketIdx){.idx = i, .occupied = false};
        }
    }
}

static IRName toCpsFrameFind(ToCpsFrame const* frame, SymbolRef sym) {
    BucketIdx const bucketIdx = toCpsFrameFindIdx(frame, sym);
    if (!bucketIdx.occupied) { return invalidIRName; }

    return frame->vals[bucketIdx.idx];
}

static void toCpsFrameSet(ToCpsFrame* frame, SymbolRef sym, IRName name) {
    BucketIdx bucketIdx = toCpsFrameFindIdx(frame, sym);
    if (bucketIdx.occupied) {
        frame->vals[bucketIdx.idx] = name;
    } else {
        size_t idx = bucketIdx.idx;

        size_t const newCount = frame->count + 1;
        if (newCount > (frame->cap >> 1)) {
            rehashToCpsFrame(frame);
            bucketIdx = toCpsFrameFindIdx(frame, sym);
            assert(!bucketIdx.occupied);
            idx = bucketIdx.idx;
        }

        frame->keys[idx] = symbolToORef(sym);
        frame->vals[idx] = name;
        frame->count = newCount;
    }
}

typedef struct ToCpsEnv {
    ToCpsFrame frame;
    struct ToCpsEnv const* parent;
} ToCpsEnv;

inline static void freeToCpsEnv(ToCpsEnv* env) { freeToCpsFrame(&env->frame); }

static ToCpsEnv createToCpsEnv(ToCpsEnv const* parent) {
    return (ToCpsEnv){
        .frame = createToCpsFrame(),
        .parent = parent
    };
}

static IRName useSymbolIRName(ToCpsEnv const* env, SymbolRef sym) {
    for (; env; env = env->parent) {
        IRName const name = toCpsFrameFind(&env->frame, sym);
        if (irNameIsValid(name)) { return name; }
    }

    return invalidIRName;
}

[[maybe_unused]] // FIXME
static IRName defSymbolIRName(Compiler* compiler, ToCpsEnv* env, SymbolRef sym) {
     // FIXME: Proper error (duplicate defs):
    assert(!irNameIsValid(toCpsFrameFind(&env->frame, sym)));

    IRName const name = renameSymbol(compiler, sym);
    toCpsFrameSet(&env->frame, sym, name);

    return name;
}

typedef struct ToCpsContReturn {
    IRName cont;
} ToCpsContReturn;

typedef struct ToCpsCont {
    union {
        ToCpsContReturn ret;
    };
    enum {
        TO_CPS_CONT_VAL,
        TO_CPS_CONT_RETURN
    } type;
} ToCpsCont;

static IRName constToCPS(Compiler* compiler, IRFn* fn, IRBlock* block, ORef expr, ToCpsCont k) {
    IRName const name = freshName(compiler);
    IRConst const c = fnConst(fn, expr);
    pushIRStmt(block, constDefToStmt((ConstDef){name, c}));

    switch (k.type) {
    case TO_CPS_CONT_VAL: break;

    case TO_CPS_CONT_RETURN:
        createIRReturn(block, k.ret.cont, name);
        break;
    }

    return name;
}

static IRName globalToCPS(
    Compiler* compiler, IRFn* fn, IRBlock* block, SymbolRef name, ToCpsCont k
) {
    IRName const tmpName = freshName(compiler);
    IRConst const nameIdx = fnConst(fn, symbolToORef(name));
    pushIRStmt(block, globalToStmt((IRGlobal){tmpName, nameIdx}));

    switch (k.type) {
    case TO_CPS_CONT_VAL: break;

    case TO_CPS_CONT_RETURN:
        createIRReturn(block, k.ret.cont, tmpName);
        break;
    }

    return tmpName;
}

static IRName exprToIR(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef expr, ToCpsCont k
) {
    if (isHeaped(expr)) {
        if (isPair(state, expr)) {
            Pair const* pair = pairToPtr(uncheckedORefToPair(expr));
            ORef const callee = pair->car;
            if (isSymbol(state, callee)) {
                SymbolRef const calleeSym = uncheckedORefToSymbol(callee);
                // OPTIMIZE: Symbol comparisons instead of `strEq`:
                if (strEq(symbolName(calleeSym), (Str){"if", /*HACK:*/2})) {
                    // OPTIMIZE: Avoid creating `goto`s to `goto`s:

                    ORef args = pair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO
                    }
                    Pair const* argsPair = pairToPtr(uncheckedORefToPair(args));

                    ORef const cond = argsPair->car;
                    args = argsPair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO
                    }
                    argsPair = pairToPtr(uncheckedORefToPair(args));

                    ORef const conseq = argsPair->car;
                    args = argsPair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO
                    }
                    argsPair = pairToPtr(uncheckedORefToPair(args));

                    ORef const alt = argsPair->car;
                    if (!isEmptyList(state, argsPair->cdr)) {
                        assert(false); // TODO
                    }

                    ToCpsCont const splitK = (ToCpsCont){{}, TO_CPS_CONT_VAL};
                    IRName const condName = exprToIR(state, compiler, fn, env, block, cond, splitK);
                    IRName const conseqLabel = freshName(compiler);
                    IRName const altLabel = freshName(compiler);
                    createIRIf(*block, condName, conseqLabel, altLabel);

                    IRBlock* conseqBlock = createIRBlock(fn, conseqLabel, 0);
                    IRName const conseqName =
                        exprToIR(state, compiler, fn, env, &conseqBlock, conseq, k);
                    IRBlock* altBlock = createIRBlock(fn, altLabel, 0);
                    IRName const altName = exprToIR(state, compiler, fn, env, &altBlock, alt, k);

                    if (k.type != TO_CPS_CONT_RETURN) {
                        IRName const joinLabel = freshName(compiler);

                        createIRGoto(conseqBlock, joinLabel, conseqName);
                        createIRGoto(altBlock, joinLabel, altName);

                        IRBlock* const joinBlock = createIRBlock(fn, joinLabel, 1);
                        IRName const phi = freshName(compiler);
                        pushIRParam(joinBlock, phi);

                        *block = joinBlock;
                        return phi;
                    } else {
                        return condName; // Arbitrary value, will not be used by callee
                    }
                } else if (strEq(symbolName(calleeSym), (Str){"quote", /*HACK:*/5})) {
                    ORef const args = pair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO
                    }
                    Pair const* const argsPair = pairToPtr(uncheckedORefToPair(args));

                    if (!isEmptyList(state, argsPair->cdr)) {
                        assert(false); // TODO
                    }

                    return constToCPS(compiler, fn, *block, argsPair->car, k);
                } else if (strEq(symbolName(calleeSym), (Str){"def", /*HACK:*/3})) {
                    ORef args = pair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO
                    }
                    Pair const* argsPair = pairToPtr(uncheckedORefToPair(args));

                    ORef const pat = argsPair->car;
                    if (!isSymbol(state, pat)) {
                        assert(false); // TODO
                    }
                    SymbolRef const name = uncheckedORefToSymbol(pat);
                    args = argsPair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO
                    }
                    argsPair = pairToPtr(uncheckedORefToPair(args));

                    ORef const val = argsPair->car;
                    if (!isEmptyList(state, argsPair->cdr)) {
                        assert(false); // TODO
                    }

                    ToCpsCont const defK = (ToCpsCont){{}, TO_CPS_CONT_VAL};
                    IRName const valName = exprToIR(state, compiler, fn, env, block, val, defK);
                    IRConst const nameIdx = fnConst(fn, symbolToORef(name));
                    pushIRStmt(*block, globalDefToStmt((GlobalDef){nameIdx, valName}));
                    // FIXME: Return e.g. nil/undefined/unspecified instead of new val:
                    IRName const resName = valName;
                    switch (k.type) {
                    case TO_CPS_CONT_VAL: break;

                    case TO_CPS_CONT_RETURN: {
                        createIRReturn(*block, k.ret.cont, resName);
                        break;
                    }
                    }

                    return resName;
                }
            }

            assert(false); // TODO
        } else if (isSymbol(state, expr)) {
            SymbolRef const sym = uncheckedORefToSymbol(expr);

            IRName const name = useSymbolIRName(env, sym);
            if (irNameIsValid(name)) {
                // TODO: Local variable
            } else {
                return globalToCPS(compiler, fn, *block, uncheckedORefToSymbol(expr), k);
            }
        }
    }

    // Else a constant:
    return constToCPS(compiler, fn, *block, expr, k);
}

static IRFn topLevelExprToIR(State const* state, Compiler* compiler, ORef expr) {
    IRFn fn = createIRFn();

    IRName const entry = freshName(compiler);
    IRBlock* entryBlock = createIRBlock(&fn, entry, 1);

    ToCpsEnv env = createToCpsEnv(nullptr);
    IRName const self = freshName(compiler);
    pushIRParam(entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(entryBlock, ret);

    ToCpsCont const retK = {{.ret = {.cont = ret}}, TO_CPS_CONT_RETURN};
    exprToIR(state, compiler, &fn, &env, &entryBlock, expr, retK);

    return fn;
}
