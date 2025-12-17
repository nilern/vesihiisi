// OPTIMIZE: At this point bitsets are slow because we are usually iterating over them.

typedef struct MaybeIRName {
    IRName val;
    bool hasVal;
} MaybeIRName;

typedef struct CloverLoc {
    MaybeIRName reg;
} CloverLoc;

typedef struct CloverLocs {
    CloverLoc* vals;
    size_t count;
} CloverLocs;

inline static void freeCloverLocs(CloverLocs* locs) { free(locs->vals); }

static CloverLocs newCloverLocs(BitSet const vars) {
    size_t const count = bitSetLimit(&vars);
    CloverLoc* const vals = malloc(count * sizeof *vals);

    for (size_t i = 0; i < count; ++i) {
        if (bitSetContains(&vars, i)) {
            vals[i] = (CloverLoc){.reg = {}};
        } else {
            vals[i] = (CloverLoc){.reg = {.val = invalidIRName, .hasVal = true}};
        }
    }

    return (CloverLocs){.vals = vals, .count = count};
}

// OPTIMIZE: Make this unnecessary:
static CloverLocs cloneCloverLocs(CloverLocs const locs) {
    size_t const count = locs.count;
    CloverLoc* const vals = malloc(count * sizeof *vals);

    memcpy(vals, locs.vals, count * sizeof *vals);

    return (CloverLocs){.vals = vals, .count = count};
}

typedef struct MaybeCloverLoc {
    CloverLoc val;
    bool hasVal;
} MaybeCloverLoc;

static MaybeCloverLoc getCloverLoc(CloverLocs const locs, IRName name) {
    size_t const idx = name.index;
    if (idx >= locs.count) { return (MaybeCloverLoc){}; }

    CloverLoc const loc = locs.vals[idx];
    if (loc.reg.hasVal && irNameEq(loc.reg.val, invalidIRName)) {
        return (MaybeCloverLoc){};
    } else {
        return (MaybeCloverLoc){.val = loc, .hasVal = true};
    }
}

static void setCloverReg(CloverLocs* locs, IRName name, IRName reg) {
    assert(name.index < locs->count);
    locs->vals[name.index].reg = (MaybeIRName){.val = reg, .hasVal = true};
}

typedef struct PureLoadsEnv {
    IRName closure;
    CloverLocs locs;
} PureLoadsEnv;

inline static void freePureLoadsEnv(PureLoadsEnv* env) { freeCloverLocs(&env->locs); }

inline static PureLoadsEnv newPureLoadsEnv(IRName closure, BitSet const vars) {
    return (PureLoadsEnv){
        .closure = closure,
        .locs = newCloverLocs(vars)
    };
}

inline static PureLoadsEnv clonePureLoadsEnv(PureLoadsEnv const env) {
    return (PureLoadsEnv){
        .closure = env.closure,
        .locs = cloneCloverLocs(env.locs)
    };
}

typedef struct MaybePureLoadsEnv {
    PureLoadsEnv val;
    bool hasVal;
} MaybePureLoadsEnv;

// TODO: `struct SavedPureLoadsEnvs` to avoid passing size separately:
static void freeSavedEnvs(MaybePureLoadsEnv* savedEnvs, size_t blockCount) {
    for (size_t i = 0; i < blockCount; ++i) {
        MaybePureLoadsEnv* const env = &savedEnvs[i];
        if (env->hasVal) {
            freePureLoadsEnv(&env->val);
        }
    }

    free(savedEnvs);
}

static void freeIRFnHusk(IRFn* fn) {
    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        freeStmts(&fn->blocks[i]->stmts);
    }

    free(fn->blocks);
}

static IRName deepLexicalUse(Compiler* compiler, PureLoadsEnv* env, IRBlock* newBlock, IRName use) {
    MaybeCloverLoc const maybeLoc = getCloverLoc(env->locs, use);
    if (!maybeLoc.hasVal) { return use; }
    CloverLoc const loc = maybeLoc.val;

    if (loc.reg.hasVal) { return loc.reg.val; } // Already loaded

    IRName const newReg = renameIRName(compiler, use);
    pushIRStmt(&newBlock->stmts, (IRStmt){
        .clover = {newReg, env->closure, use, 0},
        STMT_CLOVER
    });
    setCloverReg(&env->locs, use, newReg);
    return newReg;
}

typedef struct LiftingAnalysis {
    BitSet liftees;
    IRName closure;
} LiftingAnalysis;

static LiftingAnalysis joinLambdaLiftees(MaybePureLoadsEnv* savedEnvs, IRBlock const* block) {
    size_t const callerCount = block->callers.count;

    IRName closure = invalidIRName;
    for (size_t i = 0; i < callerCount; ++i) {
        IRLabel const callerLabel = block->callers.vals[i];
        assert(savedEnvs[callerLabel.blockIndex].hasVal);
        IRName const callerClosure = savedEnvs[callerLabel.blockIndex].val.closure;
        if (i == 0) {
            closure = callerClosure; // Init to first one
        } else if (!irNameEq(callerClosure, closure)) { // Disagreement on `closure`
            return (LiftingAnalysis){
                .liftees = bitSetClone(&block->liveIns),
                .closure = invalidIRName
            };
        }
    }

    // At this point all callers share the closure so only lift vars preloaded in all callers:

    BitSet liftees = createBitSet(bitSetBitCap(&block->liveIns));
    for (BitSetIter it = newBitSetIter(&block->liveIns);;) {
        MaybeSize const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        bool liftable = true;
        for (size_t i = 0; i < callerCount; ++i) {
            IRLabel const callerLabel = block->callers.vals[i];
            assert(savedEnvs[callerLabel.blockIndex].hasVal);
            PureLoadsEnv const callerEnv = savedEnvs[callerLabel.blockIndex].val;

            MaybeCloverLoc const maybeLoc = getCloverLoc(callerEnv.locs, liftee);
            if (maybeLoc.hasVal && !maybeLoc.val.reg.hasVal) { // In closure & not preloaded
                liftable = false;
                break;
            }
        }

        if (liftable) { bitSetSet(&liftees, liftee.index); }
    }

    return (LiftingAnalysis){.liftees = liftees, .closure = closure};
}

static void liftArgs(
    Compiler* compiler, PureLoadsEnv* env, IRBlock* newBlock, Args* args, BitSet liftees
) {
    for (BitSetIter it = newBitSetIter(&liftees);;) {
        MaybeSize const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        // OPTIMIZE: Does not need to `setCloverReg`, which `deepLexicalUse` will do:
        pushArg(args, deepLexicalUse(compiler, env, newBlock,liftee));
    }
}

static void liftParams(Compiler* compiler, PureLoadsEnv* env, IRBlock* newBlock, BitSet liftees) {
    for (BitSetIter it = newBitSetIter(&liftees);;) {
        MaybeSize const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        IRName const phi = renameIRName(compiler, liftee);
        pushIRParam(newBlock, phi);
        setCloverReg(&env->locs, liftee, phi);
    }
}

static PureLoadsEnv blockPureLoadsEnv(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, IRFn* newFn, IRBlock* newBlock,
    IRBlock const* block
) {
    switch (block->callers.count) {
    case 0: {
        assert(block->paramCount > 0);
        IRName const closure = block->params[0];
        return newPureLoadsEnv(closure, block->liveIns);
    }

    case 1: {
        assert(savedEnvs[block->callers.vals[0].blockIndex].hasVal);
        return clonePureLoadsEnv(savedEnvs[block->callers.vals[0].blockIndex].val);
    }

    default: {
        LiftingAnalysis const lifting = joinLambdaLiftees(savedEnvs, block);

        {
            size_t const callerCount = block->callers.count;
            for (size_t i = 0; i < callerCount; ++i) {
                IRLabel const callerLabel = block->callers.vals[i];

                assert(savedEnvs[callerLabel.blockIndex].hasVal);
                PureLoadsEnv* callerEnv = &savedEnvs[callerLabel.blockIndex].val;
                assert(callerLabel.blockIndex < newFn->blockCount);
                IRBlock* const newCaller = newFn->blocks[callerLabel.blockIndex];
                IRTransfer* const callerTransfer = &newCaller->transfer;
                assert(callerTransfer->type == TRANSFER_GOTO);
                Args* const callerArgs = &callerTransfer->gotoo.args;
                liftArgs(compiler, callerEnv, newCaller, callerArgs, lifting.liftees);
            }
        }

        PureLoadsEnv env = newPureLoadsEnv(lifting.closure, lifting.liftees);
        liftParams(compiler, &env, newBlock, lifting.liftees);

        return env;
    }
    }
}

static IRFn fnWithPureLoads(Compiler* compiler, IRFn fn);

static void linearizeCloses(
    Compiler* compiler, PureLoadsEnv* env, IRBlock* newBlock, Args* dest, BitSet const* closes
) {
    for (BitSetIter it = newBitSetIter(closes);;) {
        MaybeSize const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }

        IRName const closee = deepLexicalUse(compiler, env, newBlock, (IRName){maybeIdx.val});
        pushArg(dest, closee);
    }
}

static IRStmt stmtWithPureLoads(
    Compiler* compiler, PureLoadsEnv* env, IRBlock* newBlock, IRStmt stmt
) {
    switch (stmt.type) {
    case STMT_GLOBAL_DEF: {
        stmt.globalDef.val = deepLexicalUse(compiler, env, newBlock, stmt.globalDef.val);
    }; break;

    case STMT_GLOBAL: case STMT_CONST_DEF: break;

    case STMT_CLOVER: assert(false); break; // Should not exist yet

    case STMT_FN_DEF: {
        stmt.fnDef.fn = fnWithPureLoads(compiler, stmt.fnDef.fn);

        linearizeCloses(
            compiler, env, newBlock, &stmt.fnDef.closes, &stmt.fnDef.fn.blocks[0]->liveIns
        );
    }; break;

    case STMT_MOVE: case STMT_SWAP: assert(false); break; // Should not exist yet
    }

    return stmt;
}

static IRTransfer transferWithPureLoads(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, PureLoadsEnv* env, IRBlock* newBlock,
    IRFn const* fn, IRTransfer transfer
) {
    switch (transfer.type) {
    case TRANSFER_CALL: {
        transfer.call.callee = deepLexicalUse(compiler, env, newBlock, transfer.call.callee);

        size_t const arity = transfer.call.args.count;
        for (size_t i = 0; i < arity; ++i) {
            transfer.call.args.names[i] =
                deepLexicalUse(compiler, env, newBlock, transfer.call.args.names[i]);
        }

        IRBlock const* const retBlock = fn->blocks[transfer.call.retLabel.blockIndex];
        linearizeCloses(compiler, env, newBlock, &transfer.call.closes, &retBlock->liveIns);

        freePureLoadsEnv(env);
    }; break;

    case TRANSFER_TAILCALL: {
        transfer.tailcall.callee =
            deepLexicalUse(compiler, env, newBlock, transfer.tailcall.callee);
        transfer.tailcall.retFrame =
            deepLexicalUse(compiler, env, newBlock, transfer.tailcall.retFrame);

        size_t const arity = transfer.tailcall.args.count;
        for (size_t i = 0; i < arity; ++i) {
            transfer.tailcall.args.names[i] =
                deepLexicalUse(compiler, env, newBlock, transfer.tailcall.args.names[i]);
        }

        freePureLoadsEnv(env);
    }; break;

    case TRANSFER_IF: {
        transfer.iff.cond = deepLexicalUse(compiler, env, newBlock, transfer.iff.cond);

        savedEnvs[newBlock->label.blockIndex] = (MaybePureLoadsEnv){.val = *env, .hasVal = true};
    }; break;

    case TRANSFER_GOTO: {
        size_t const arity = transfer.gotoo.args.count;
        for (size_t i = 0; i < arity; ++i) {
            transfer.gotoo.args.names[i] =
                deepLexicalUse(compiler, env, newBlock, transfer.gotoo.args.names[i]);
        }

        savedEnvs[newBlock->label.blockIndex] = (MaybePureLoadsEnv){.val = *env, .hasVal = true};
    }; break;

    case TRANSFER_RETURN: {
        transfer.ret.callee = deepLexicalUse(compiler, env, newBlock, transfer.ret.callee);
        transfer.ret.arg = deepLexicalUse(compiler, env, newBlock, transfer.ret.arg);

        freePureLoadsEnv(env);
    }; break;
    }

    return transfer;
}

static void blockWithPureLoads(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, IRFn* newFn,
    IRFn const* fn, IRBlock const* const block
) {
    size_t const callerCount = block->callers.count;
    IRBlock* const newBlock = createIRBlock(newFn, callerCount);

    for (size_t i = 0; i < callerCount; ++i) {
        pushCaller(newBlock, block->callers.vals[i]);
    }

    newBlock->liveIns = block->liveIns;

    newBlock->params = block->params;
    newBlock->paramCount = block->paramCount;
    newBlock->paramCap = block->paramCap;

    PureLoadsEnv env = blockPureLoadsEnv(compiler, savedEnvs, newFn, newBlock, block);

    size_t const stmtCount = block->stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        pushIRStmt(&newBlock->stmts,
                   stmtWithPureLoads(compiler, &env, newBlock, block->stmts.vals[i]));
    }

    newBlock->transfer =
        transferWithPureLoads(compiler, savedEnvs, &env, newBlock, fn, block->transfer);
}

static IRFn fnWithPureLoads(Compiler* compiler, IRFn fn) {
    MaybePureLoadsEnv* const savedEnvs = calloc(fn.blockCount, sizeof *savedEnvs);

    IRFn newFn = createIRFnWithConsts(fn.consts, fn.constCount, fn.constCap);

    size_t const blockCount = fn.blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        blockWithPureLoads(compiler, savedEnvs, &newFn, &fn, fn.blocks[i]);
    }

    freeIRFnHusk(&fn);

    freeSavedEnvs(savedEnvs, fn.blockCount);
    return newFn;
}
