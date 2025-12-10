typedef enum BindingsType {
    BINDINGS_PAR,
    BINDINGS_SEQ
} BindingsType;

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

static IRName defSymbolIRName(ToCpsEnv* env, SymbolRef sym, IRName name, BindingsType type) {
     // FIXME: Proper error (duplicate defs):
    if (type == BINDINGS_PAR) {
        assert(!irNameIsValid(toCpsFrameFind(&env->frame, sym)));
    }

    toCpsFrameSet(&env->frame, sym, name);

    return name;
}

typedef struct ToCpsContReturn {
    IRName cont;
} ToCpsContReturn;

typedef struct ToCpsCont {
    union {
        ToCpsContReturn ret;
        IRName name;
    };
    enum {
        TO_CPS_CONT_VAL,
        TO_CPS_CONT_BIND,
        TO_CPS_CONT_RETURN
    } type;
} ToCpsCont;

static IRName toCpsContDestName(Compiler* compiler, ToCpsCont k) {
    switch (k.type) {
    case TO_CPS_CONT_BIND: return k.name;

    case TO_CPS_CONT_VAL: // fallthrough
    case TO_CPS_CONT_RETURN: return freshName(compiler);
    }

    return invalidIRName; // Unreachable
}

static IRName constToCPS(Compiler* compiler, IRFn* fn, IRBlock* block, ORef expr, ToCpsCont k) {
    IRName const name = toCpsContDestName(compiler, k);
    IRConst const c = fnConst(fn, expr);
    pushIRStmt(block, constDefToStmt((ConstDef){name, c}));

    if (k.type == TO_CPS_CONT_RETURN) {
        createIRReturn(block, k.ret.cont, name);
    }

    return name;
}

static IRName globalToCPS(
    Compiler* compiler, IRFn* fn, IRBlock* block, SymbolRef sym, ToCpsCont k
) {
    IRName const name = toCpsContDestName(compiler, k);
    IRConst const symIdx = fnConst(fn, symbolToORef(sym));
    pushIRStmt(block, globalToStmt((IRGlobal){name, symIdx}));

    if (k.type == TO_CPS_CONT_RETURN) {
        createIRReturn(block, k.ret.cont, name);
    }

    return name;
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
                if (strEq(symbolName(calleeSym), (Str){"fn", /*HACK:*/2})) {
                    IRFn innerFn = createIRFn();

                    IRBlock* entryBlock = createIRBlock(&innerFn);

                    ToCpsEnv fnEnv = createToCpsEnv(env);
                    IRName const self = freshName(compiler);
                    pushIRParam(entryBlock, self);
                    IRName const ret = freshName(compiler);
                    pushIRParam(entryBlock, ret);

                    ORef args = pair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO: Proper args error (`(fn)`)
                    }
                    Pair const* argsPair = pairToPtr(uncheckedORefToPair(args));

                    for (ORef params = argsPair->car;;) {
                        if (isPair(state, params)) {
                            Pair const* const paramsPair = pairToPtr(uncheckedORefToPair(params));

                            ORef const param = paramsPair->car;
                            if (!isSymbol(state, param)) {
                                assert(false); // TODO: Proper param error
                            }
                            SymbolRef const paramSym = uncheckedORefToSymbol(param);

                            IRName const paramName = renameSymbol(compiler, paramSym);
                            pushIRParam(entryBlock, paramName);
                            defSymbolIRName(&fnEnv, paramSym, paramName, BINDINGS_PAR);

                            params = paramsPair->cdr;
                        } else if (isEmptyList(state, params)) {
                            break;
                        } else {
                            assert(false); // TODO: Varargs (or error)
                        }
                    }

                    args = argsPair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO: Proper args error (missing body)
                    }
                    argsPair = pairToPtr(uncheckedORefToPair(args));

                    ORef const body = argsPair->car;

                    if (!isEmptyList(state, argsPair->cdr)) {
                        assert(false); // TODO: Proper args error or implement body stmts
                    }

                    ToCpsCont const retK = {{.ret = {.cont = ret}}, TO_CPS_CONT_RETURN};
                    exprToIR(state, compiler, &innerFn, &fnEnv, &entryBlock, body, retK);
                    freeToCpsEnv(&fnEnv);


                    IRName const name = toCpsContDestName(compiler, k);
                    // Placeholder, will be replaced with `Method` in codegen:
                    IRConst const constIdx = fnConst(fn, fixnumToORef(Zero));
                    pushIRStmt(*block, fnDefToStmt((FnDef){name, innerFn, constIdx}));

                    if (k.type == TO_CPS_CONT_RETURN) {
                        createIRReturn(*block, k.ret.cont, name);
                    }

                    return name;
                } else if (strEq(symbolName(calleeSym), (Str){"if", /*HACK:*/2})) {
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

                    IRBlock* conseqBlock = createIRBlock(fn);
                    IRBlock* altBlock = createIRBlock(fn);

                    ToCpsCont const splitK = (ToCpsCont){{}, TO_CPS_CONT_VAL};
                    IRName const condName = exprToIR(state, compiler, fn, env, block, cond, splitK);
                    createIRIf(*block, condName, conseqBlock->label, altBlock->label);

                    IRName const conseqName =
                        exprToIR(state, compiler, fn, env, &conseqBlock, conseq, k);
                    IRName const altName = exprToIR(state, compiler, fn, env, &altBlock, alt, k);

                    if (k.type != TO_CPS_CONT_RETURN) {
                        IRBlock* const joinBlock = createIRBlock(fn);
                        // FIXME: If `k` provides a target name it should be used for `phi`. Now it
                        // gets pushed into the branches and possibly multiply defined:
                        IRName const phi = freshName(compiler);
                        pushIRParam(joinBlock, phi);

                        createIRGoto(conseqBlock, joinBlock->label, conseqName);
                        createIRGoto(altBlock, joinBlock->label, altName);

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
                    if (k.type == TO_CPS_CONT_RETURN) {
                        createIRReturn(*block, k.ret.cont, resName);
                    }

                    return resName;
                } else if (strEq(symbolName(calleeSym), (Str){"let", /*HACK:*/3})) {
                    ToCpsEnv letEnv = createToCpsEnv(env);

                    ORef args = pair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO: Proper invalid args error
                    }
                    Pair const* argsPair = pairToPtr(uncheckedORefToPair(args));

                    for (ORef bindings = argsPair->car;;) {
                        if (isPair(state, bindings)) {
                            Pair const* const bindingsPair =
                                pairToPtr(uncheckedORefToPair(bindings));

                            ORef const binding = bindingsPair->car;
                            if (!isPair(state, binding)) {
                                assert(false); // TODO: Proper invalid binding error
                            }
                            Pair const* const bindingPair =
                                pairToPtr(uncheckedORefToPair(binding));

                            ORef const pat = bindingPair->car;
                            if (!isSymbol(state, pat)) {
                                assert(false); // TODO: Proper invalid binder error
                            }
                            SymbolRef const binder = uncheckedORefToSymbol(pat);

                            ORef const bindingArgs = bindingPair->cdr;
                            if (!isPair(state, bindingArgs)) {
                                assert(false); // TODO: Proper invalid binding error
                            }
                            Pair const* const bindingArgsPair =
                                pairToPtr(uncheckedORefToPair(bindingArgs));

                            ORef const val = bindingArgsPair->car;

                            if (!isEmptyList(state, bindingArgsPair->cdr)) {
                                assert(false); // TODO: Proper invali [[maybe_unused]]d binding error
                            }

                            IRName const binderName = renameSymbol(compiler, binder);
                            ToCpsCont const valK = (ToCpsCont){
                                .name = binderName,
                                .type = TO_CPS_CONT_BIND
                            };
                            IRName const finalName =
                                exprToIR(state, compiler, fn, &letEnv, block, val, valK);
                            // If `finalName != binderName` we have a local copy e.g.
                            // `(let ((x 5) (y x)) ...)` and `exprToIR` emitted nothing. Putting
                            // `finalName` to env implements the rest of copy propagation:
                            defSymbolIRName(&letEnv, binder, finalName, BINDINGS_SEQ);

                            bindings = bindingsPair->cdr;
                        } else if (isEmptyList(state, bindings)) {
                            break;
                        } else {
                            assert(false); // TODO: Proper invalid bindings error
                        }
                    }

                    args = argsPair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO: Proper invalid args error
                    }
                    argsPair = pairToPtr(uncheckedORefToPair(args));

                    ORef const body = argsPair->car;

                    if (!isEmptyList(state, argsPair->cdr)) {
                        assert(false); // TODO: Proper invalid args error
                    }

                    IRName const bodyName = exprToIR(state, compiler, fn, &letEnv, block, body, k);
                    freeToCpsEnv(&letEnv);
                    return bodyName;
                }
            }

            IRName const calleeName =
                exprToIR(state, compiler, fn, env, block, callee,
                         (ToCpsCont){.type = TO_CPS_CONT_VAL});
            Args cpsArgs = createArgs();
            for (ORef args = pair->cdr;;) {
                if (isPair(state, args)) {
                    Pair const* const argsPair = pairToPtr(uncheckedORefToPair(args));

                    ORef const arg = argsPair->car;
                    IRName const argName =
                        exprToIR(state, compiler, fn, env, block, arg,
                                 (ToCpsCont){.type = TO_CPS_CONT_VAL});
                    pushArg(&cpsArgs, argName);

                    args = argsPair->cdr;
                } else if (isEmptyList(state, args)) {
                    break;
                } else {
                    assert(false); // TODO: proper improper args error
                }
            }

            IRName const retValName = toCpsContDestName(compiler, k);

            if (k.type != TO_CPS_CONT_RETURN) {
                IRBlock* const retBlock = createIRBlock(fn);
                IRName const frame = freshName(compiler);
                pushIRParam(retBlock, frame);
                pushIRParam(retBlock, retValName);

                createCall(*block, calleeName, retBlock->label, cpsArgs);

                *block = retBlock;
            } else {
                createTailcall(*block, calleeName, k.ret.cont, cpsArgs);
            }

            return retValName;
        } else if (isSymbol(state, expr)) {
            SymbolRef const sym = uncheckedORefToSymbol(expr);

            IRName const name = useSymbolIRName(env, sym);
            if (irNameIsValid(name)) {
                if (k.type == TO_CPS_CONT_RETURN) {
                    createIRReturn(*block, k.ret.cont, name);
                }

                return name;
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

    IRBlock* entryBlock = createIRBlock(&fn);

    ToCpsEnv env = createToCpsEnv(nullptr);
    IRName const self = freshName(compiler);
    pushIRParam(entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(entryBlock, ret);

    ToCpsCont const retK = {{.ret = {.cont = ret}}, TO_CPS_CONT_RETURN};
    exprToIR(state, compiler, &fn, &env, &entryBlock, expr, retK);

    return fn;
}
