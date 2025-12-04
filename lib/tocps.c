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
    IRContinue* continueTransfer = createIRContinue(entryBlock, ret, 1);
    IRAtom const c = fnConst(&fn, expr);
    irContinuePushArg(continueTransfer, c);

    return fn;
}
