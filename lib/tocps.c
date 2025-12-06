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
    State const* state, Compiler* compiler, IRFn* fn, IRBlock** block, ORef expr, ToCpsCont k
) {
    if (isHeaped(expr)) {
        if (isPair(state, expr)) {
            Pair const* pair = pairToPtr(uncheckedORefToPair(expr));
            ORef const callee = pair->car;
            if (isSymbol(state, callee)) {
                SymbolRef const calleeSym = uncheckedORefToSymbol(callee);
                // OPTIMIZE: Symbol comparisons instead of `strEq`:
                if (strEq(symbolName(calleeSym), (Str){"if", /*HACK:*/2})) {
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
                    IRName const condName = exprToIR(state, compiler, fn, block, cond, splitK);
                    IRName const conseqLabel = freshName(compiler);
                    IRName const altLabel = freshName(compiler);
                    createIRIf(*block, condName, conseqLabel, altLabel);

                    IRBlock* conseqBlock = createIRBlock(fn, conseqLabel, 0);
                    IRName const conseqName =
                        exprToIR(state, compiler, fn, &conseqBlock, conseq, k);
                    IRBlock* altBlock = createIRBlock(fn, altLabel, 0);
                    IRName const altName = exprToIR(state, compiler, fn, &altBlock, alt, k);

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
                    IRName const valName = exprToIR(state, compiler, fn, block, val, defK);
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
            return globalToCPS(compiler, fn, *block, uncheckedORefToSymbol(expr), k);
        }
    }

    // Else a constant:
    return constToCPS(compiler, fn, *block, expr, k);
}

static IRFn topLevelExprToIR(State const* state, Compiler* compiler, ORef expr) {
    IRFn fn = createIRFn();

    IRName const entry = freshName(compiler);
    IRBlock* entryBlock = createIRBlock(&fn, entry, 1);
    IRName const self = freshName(compiler);
    pushIRParam(entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(entryBlock, ret);

    ToCpsCont const retK = {{.ret = {.cont = ret}}, TO_CPS_CONT_RETURN};
    exprToIR(state, compiler, &fn, &entryBlock, expr, retK);

    return fn;
}
