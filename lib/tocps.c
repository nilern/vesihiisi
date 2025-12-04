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
        IRReturn* retTransfer = createIRReturn(block, k.ret.cont, 1);
        irReturnPushArg(retTransfer, name);
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
