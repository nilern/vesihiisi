static IRName constToCPS(Compiler* compiler, IRFn* fn, IRBlock* block, ORef expr) {
    IRName const name = freshName(compiler);
    IRConst const c = fnConst(fn, expr);
    pushIRStmt(block, constDefToStmt((ConstDef){name, c}));
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

    if (isHeaped(expr)) {
        if (isPair(state, expr)) {
            assert(false); // TODO
        } else if (isSymbol(state, expr)) {
            assert(false); // TODO
        }
    }

    // Else a constant:
    IRReturn* retTransfer = createIRReturn(entryBlock, ret, 1);
    IRName const cName = constToCPS(compiler, &fn, entryBlock, expr);
    irReturnPushArg(retTransfer, cName);

    return fn;
}
