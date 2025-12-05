typedef struct ToCpsContReturn {
    IRName cont;
} ToCpsContReturn;

typedef struct ToCpsCont {
    enum {
        TO_CPS_CONT_RETURN
    } type;
    union {
        ToCpsContReturn ret;
    };
} ToCpsCont;

static IRName constToCPS(Compiler* compiler, IRFn* fn, IRBlock* block, ORef expr, ToCpsCont k) {
    IRName const name = freshName(compiler);
    IRConst const c = fnConst(fn, expr);
    pushIRStmt(block, constDefToStmt((ConstDef){name, c}));

    switch (k.type) {
    case TO_CPS_CONT_RETURN:
        createIRReturn(block, k.ret.cont, name);
        break;
    }

    return name;
}

static IRFn topLevelExprToIR(State const* state, Compiler* compiler, ORef expr) {
    IRFn fn = createIRFn();

    IRName const entry = freshName(compiler);
    IRBlock* const entryBlock = createIRBlock(&fn, entry, 1);
    IRName const self = freshName(compiler);
    pushIRParam(entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(entryBlock, ret);

    ToCpsCont const k = {TO_CPS_CONT_RETURN, {.ret = {.cont = ret}}};

    if (isHeaped(expr)) {
        if (isPair(state, expr)) {
            Pair const* pair = pairToPtr(uncheckedORefToPair(expr));
            ORef const callee = pair->car;
            if (isSymbol(state, callee)) {
                SymbolRef const calleeSym = uncheckedORefToSymbol(callee);
                if (strEq(symbolName(calleeSym), (Str){"quote", /*HACK:*/5})) {
                    ORef const args = pair->cdr;
                    if (!isPair(state, args)) {
                        assert(false); // TODO
                    }

                    Pair const* const argsPair = pairToPtr(uncheckedORefToPair(args));
                    if (!isEmptyList(state, argsPair->cdr)) {
                        assert(false); // TODO
                    }

                    constToCPS(compiler, &fn, entryBlock, argsPair->car, k);
                    return fn;
                }
            }

            assert(false); // TODO
            return fn;
        } else if (isSymbol(state, expr)) {
            assert(false); // TODO
            return fn;
        }
    }

    // Else a constant:
    constToCPS(compiler, &fn, entryBlock, expr, k);
    return fn;
}
