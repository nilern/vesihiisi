typedef struct CloverIdxs {
    IRName closure;
    MaybeUInt8* idxs;
    size_t cap;
} CloverIdxs;

static CloverIdxs newCloverIdxs(Compiler* compiler, IRName closure) {
    size_t const cap = compiler->nameCount;
    MaybeUInt8* const idxs = acalloc(&compiler->arena, cap, sizeof *idxs);
    return (CloverIdxs){.closure = closure, .idxs = idxs, .cap = cap};
}

static uint8_t getCloverIdx(CloverIdxs const* env, IRName origName) {
    MaybeUInt8 const maybeIdx = env->idxs[origName.index];
    assert(maybeIdx.hasVal);
    return maybeIdx.val;
}

typedef struct MaybeCloverIdxs {
    CloverIdxs val;
    bool hasVal;
} MaybeCloverIdxs;

typedef struct CloverIndexing {
    MaybeCloverIdxs* savedEnvs;
    size_t blockCount;
} CloverIndexing;

static CloverIndexing newCloverIndexing(Compiler* compiler, IRFn const* fn) {
    size_t const blockCount = fn->blockCount;
    MaybeCloverIdxs* const savedEnvs = acalloc(&compiler->arena, blockCount, sizeof *savedEnvs);
    return (CloverIndexing){.savedEnvs = savedEnvs, .blockCount = blockCount};
}

inline static void saveCloverIdxs(CloverIndexing* pass, IRLabel label, CloverIdxs const* env) {
    pass->savedEnvs[label.blockIndex] = (MaybeCloverIdxs){.val = *env, .hasVal = true};
}

static CloverIdxs const* getCloverIdxs(CloverIndexing const* pass, IRLabel label) {
    MaybeCloverIdxs const* const maybeCloverIdxs = &pass->savedEnvs[label.blockIndex];
    assert(maybeCloverIdxs->hasVal);
    return &maybeCloverIdxs->val;
}

static CloverIdxs closeCloverIdxs(
    Compiler* compiler, BitSet const* clovers, IRName closure, Args const* close
) {
    CloverIdxs env = newCloverIdxs(compiler, closure);

    {
        size_t const cloverCount = close->count;
        BitSetIter it = newBitSetIter(clovers);
        for (size_t i = 0;; ++i) {
            MaybeSize const maybeIdx = bitSetIterNext(&it);
            if (!maybeIdx.hasVal) { break; }
            IRName const clover = {maybeIdx.val};

            assert(i < close->count);
            Reg const reg = (Reg){(uint8_t)close->names[i].index};

            // How many clovers in `close` whose register is lower than `reg`?:
            size_t idx = 0;
            for (size_t j = 0; j < cloverCount; ++j) {
                if (close->names[j].index < reg.index) {
                    ++idx;
                }
            }

            env.idxs[clover.index] = (MaybeUInt8){.val = (uint8_t)idx, .hasVal = true};
        }
    }

    return env;
}

static void indexFnClovers(
    Compiler* compiler, CloverIndexing* pass, CloverIdxs const* fnEnv, IRFn* fn
);

static void indexStmtClovers(
    Compiler* compiler, CloverIndexing* pass, CloverIdxs const* env, IRStmt* stmt
) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: case STMT_GLOBAL: case STMT_CONST_DEF: break;

    case STMT_CLOVER: {
        stmt->clover.idx = getCloverIdx(env, stmt->clover.origName);
    }; break;

    case STMT_FN_DEF: {
        IRBlock const* const entryBlock = stmt->fnDef.fn.blocks[0];
        CloverIdxs innerEnv = closeCloverIdxs(
            compiler, &entryBlock->liveIns, entryBlock->params[0], &stmt->fnDef.closes
        );
        indexFnClovers(compiler, pass, &innerEnv, &stmt->fnDef.fn);
    }; break;

    case STMT_MOVE: case STMT_SWAP: break;
    }
}

static void indexTransferClovers(
    Compiler* compiler, CloverIndexing* pass, IRFn const* fn, CloverIdxs const* env,
    IRTransfer const* transfer
) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        IRLabel const retLabel = transfer->call.retLabel;
        IRBlock const* const retBlock = fn->blocks[retLabel.blockIndex];
        CloverIdxs retEnv = closeCloverIdxs(
            compiler, &retBlock->liveIns, retBlock->params[0], &transfer->call.closes
        );
        saveCloverIdxs(pass, retLabel, &retEnv);
    }; break;

    case TRANSFER_TAILCALL: break;

    case TRANSFER_IF: {
        saveCloverIdxs(pass, transfer->iff.conseq, env);
        saveCloverIdxs(pass, transfer->iff.alt, env);
    }; break;

    case TRANSFER_GOTO: {
        saveCloverIdxs(pass, transfer->gotoo.dest, env);
    }; break;

    case TRANSFER_RETURN: break;
    }
}

static void indexBlockClovers(
    Compiler* compiler, CloverIndexing* pass, IRFn const* fn, CloverIdxs const* fnEnv,
    IRBlock* block
) {
    CloverIdxs const* env = block->label.blockIndex != 0
        ? getCloverIdxs(pass, block->label)
        : fnEnv;

    size_t const stmtCount = block->stmtCount;
    for (size_t i = 0; i < stmtCount; ++i) {
        indexStmtClovers(compiler, pass, env, &block->stmts[i]);
    }

    indexTransferClovers(compiler, pass, fn, env, &block->transfer);
}

static void indexFnClovers(
    Compiler* compiler, CloverIndexing* pass, CloverIdxs const* fnEnv, IRFn* fn
) {
    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        indexBlockClovers(compiler, pass, fn, fnEnv, fn->blocks[i]);
    }
}

static void indexToplevelFnClovers(Compiler* compiler, IRFn* fn) {
    CloverIndexing pass = newCloverIndexing(compiler, fn);
    CloverIdxs const emptyFnEnv = newCloverIdxs(compiler, fn->blocks[0]->params[0]);
    indexFnClovers(compiler, &pass, &emptyFnEnv, fn);
}
