inline static void requireLive(BitSet* lives, IRName name) { bitSetSet(lives, name.index); }

inline static void rangeStart(BitSet* lives, IRName name) { bitSetRemove(lives, name.index); }

static void transferLiveOutsInto(IRFn const* fn, BitSet* lives, IRTransfer const* transfer) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        IRBlock const* retBlock = irLabelBlock(fn, transfer->call.retLabel);
        bitSetUnionInto(lives, &retBlock->liveIns);
    }; break;

    case TRANSFER_TAILCALL: break;

    case TRANSFER_IF: {
        IRBlock const* alt = irLabelBlock(fn, transfer->iff.alt);
        bitSetUnionInto(lives, &alt->liveIns);
        IRBlock const* conseq = irLabelBlock(fn, transfer->iff.conseq);
        bitSetUnionInto(lives, &conseq->liveIns);
    }; break;

    case TRANSFER_GOTO: {
        IRBlock const* dest = irLabelBlock(fn, transfer->gotoo.dest);
        bitSetUnionInto(lives, &dest->liveIns);
    }; break;

    case TRANSFER_RETURN: break;
    }
}

static void enlivenTransfer(BitSet* liveOuts, IRTransfer const* transfer) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        for (size_t i = transfer->call.args.count; i-- > 0;) {
            requireLive(liveOuts, transfer->call.args.names[i]);
        }
        requireLive(liveOuts, transfer->call.callee);
    }; break;

    case TRANSFER_TAILCALL: {
        for (size_t i = transfer->tailcall.args.count; i-- > 0;) {
            requireLive(liveOuts, transfer->tailcall.args.names[i]);
        }
        requireLive(liveOuts, transfer->tailcall.retFrame);
        requireLive(liveOuts, transfer->tailcall.callee);
    }; break;

    case TRANSFER_IF: {
        requireLive(liveOuts, transfer->iff.cond);
    }; break;

    case TRANSFER_GOTO: {
        for (size_t i = transfer->gotoo.args.count; i-- > 0;) {
            requireLive(liveOuts, transfer->gotoo.args.names[i]);
        }
    }; break;

    case TRANSFER_RETURN: {
        requireLive(liveOuts, transfer->ret.arg);
        requireLive(liveOuts, transfer->ret.callee);
    }; break;
    }
}

static void enlivenFn(IRFn* fn);

static void enlivenStmt(BitSet* liveOuts, IRStmt* stmt) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: {
        requireLive(liveOuts, stmt->globalDef.val);
    }; break;

    case STMT_GLOBAL: {
        rangeStart(liveOuts, stmt->global.tmpName);
    }; break;

    case STMT_CONST_DEF: {
        rangeStart(liveOuts, stmt->constDef.name);
    }; break;

    case STMT_CLOVER: assert(false); break; // Should not exist yet

    case STMT_FN_DEF: {
        rangeStart(liveOuts, stmt->fnDef.name);

        IRFn* const innerFn = &stmt->fnDef.fn;
        enlivenFn(innerFn);
        bitSetUnionInto(liveOuts, fnFreeVars(innerFn));
    }; break;

    case STMT_MOVE: case STMT_SWAP: assert(false); break; // Should not exist yet
    }
}

static void enlivenParams(BitSet* liveOuts, IRBlock const* block) {
    for (size_t i = block->paramCount; i-- > 0;) {
        rangeStart(liveOuts, block->params[i]);
    }
}

static void enlivenBlock(IRFn const* fn, IRBlock* block) {
    BitSet* lives = &block->liveIns;
    transferLiveOutsInto(fn, lives, &block->transfer);

    enlivenTransfer(lives, &block->transfer);

    for (size_t i = block->stmts.count; i-- > 0;) {
        enlivenStmt(lives, &block->stmts.vals[i]);
    }

    enlivenParams(lives, block);
}

static void enlivenFn(IRFn* fn) {
    for (size_t i = fn->blockCount; i-- > 0;) {
        enlivenBlock(fn, fn->blocks[i]);
    }
}
