#include "cloverindexing.hpp"

#include "../util/util.hpp"
#include "../util/bitset.hpp"
#include "regalloc.hpp"

namespace {

typedef struct CloverIdxs {
    Maybe<uint8_t>* idxs;
    size_t cap;
} CloverIdxs;

CloverIdxs newCloverIdxs(Compiler* compiler) {
    size_t const cap = compiler->nameCount;
    Maybe<uint8_t>* const idxs = (Maybe<uint8_t>*)acalloc(&compiler->arena, cap, sizeof *idxs);
    return CloverIdxs{.idxs = idxs, .cap = cap};
}

uint8_t getCloverIdx(CloverIdxs const* env, IRName origName) {
    Maybe<uint8_t> const maybeIdx = env->idxs[origName.index];
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

CloverIndexing newCloverIndexing(Compiler* compiler, IRFn const* fn) {
    size_t const blockCount = fn->blockCount;
    MaybeCloverIdxs* const savedEnvs =
        (MaybeCloverIdxs*)acalloc(&compiler->arena, blockCount, sizeof *savedEnvs);
    return CloverIndexing{.savedEnvs = savedEnvs, .blockCount = blockCount};
}

inline void saveCloverIdxs(CloverIndexing* pass, IRLabel label, CloverIdxs const* env) {
    pass->savedEnvs[label.blockIndex] = MaybeCloverIdxs{.val = *env, .hasVal = true};
}

CloverIdxs const* getCloverIdxs(CloverIndexing const* pass, IRLabel label) {
    MaybeCloverIdxs const* const maybeCloverIdxs = &pass->savedEnvs[label.blockIndex];
    assert(maybeCloverIdxs->hasVal);
    return &maybeCloverIdxs->val;
}

CloverIdxs closeCloverIdxs(Compiler* compiler, BitSet const* clovers, Args const* close) {
    CloverIdxs env = newCloverIdxs(compiler);

    {
        size_t const cloverCount = close->count;
        BitSetIter it = newBitSetIter(clovers);
        for (size_t i = 0;; ++i) {
            Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
            if (!maybeIdx.hasVal) { break; }
            IRName const clover = {maybeIdx.val};

            assert(i < close->count);
            Reg const reg = Reg{(uint8_t)close->names[i].index};

            // How many clovers in `close` whose register is lower than `reg`?:
            size_t idx = 0;
            for (size_t j = 0; j < cloverCount; ++j) {
                if (close->names[j].index < reg.index) {
                    ++idx;
                }
            }

            env.idxs[clover.index] = Maybe<uint8_t>{(uint8_t)idx};
        }
    }

    return env;
}

void indexFnClovers(Compiler* compiler, CloverIdxs const* fnEnv, IRFn* fn);

void indexStmtClovers(Compiler* compiler, CloverIdxs const* env, IRStmt* stmt) {
    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: case IRStmt::GLOBAL: case IRStmt::CONST_DEF: break; // Not closure-related

    case IRStmt::CLOVER: {
        Clover* const clover = &stmt->clover;
        clover->idx = getCloverIdx(env, clover->origName);
    }; break;

    case IRStmt::METHOD_DEF: {
        MethodDef* const methodDef = &stmt->methodDef;
        IRFn* const fn = &methodDef->fn;

        CloverIdxs innerEnv = closeCloverIdxs(compiler, fnFreeVars(fn), methodDef->closes);
        indexFnClovers(compiler, &innerEnv, fn);
    }; break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP:
    case IRStmt::KNOT: case IRStmt::KNOT_INIT: case IRStmt::KNOT_GET: break; // Not closure-related
    }
}

void indexTransferClovers(
    Compiler* compiler, CloverIndexing* pass, IRFn const* fn, CloverIdxs const* env,
    IRTransfer const* transfer
) {
    switch (transfer->type) {
    case IRTransfer::CALL: {
        Call const* const call = &transfer->call;

        IRLabel const retLabel = call->retLabel;
        IRBlock const* const retBlock = fn->blocks[retLabel.blockIndex];
        CloverIdxs retEnv = closeCloverIdxs(compiler, &retBlock->liveIns, &call->closes);
        saveCloverIdxs(pass, retLabel, &retEnv);
    }; break;

    case IRTransfer::TAILCALL: break; // Terminator; does not even ned to `saveCloverIdxs`

    case IRTransfer::IF: {
        IRIf const* const iff = &transfer->iff;
        saveCloverIdxs(pass, iff->conseq, env);
        saveCloverIdxs(pass, iff->alt, env);
    }; break;

    case IRTransfer::GOTO: {
        saveCloverIdxs(pass, transfer->gotoo.dest, env);
    }; break;

    case IRTransfer::RETURN: break; // Terminator; does not even ned to `saveCloverIdxs`
    }
}

void indexBlockClovers(
    Compiler* compiler, CloverIndexing* pass, IRFn const* fn, CloverIdxs const* fnEnv,
    IRBlock* block
) {
    CloverIdxs const* env = block->label.blockIndex != 0
        ? getCloverIdxs(pass, block->label)
        : fnEnv;

    size_t const stmtCount = block->stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        indexStmtClovers(compiler, env, &block->stmts.vals[i]);
    }

    indexTransferClovers(compiler, pass, fn, env, &block->transfer);
}

void indexFnClovers(Compiler* compiler, CloverIdxs const* fnEnv, IRFn* fn) {
    CloverIndexing pass = newCloverIndexing(compiler, fn);

    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        indexBlockClovers(compiler, &pass, fn, fnEnv, fn->blocks[i]);
    }
}

void indexToplevelFnClovers(Compiler* compiler, IRFn* fn) {
    CloverIdxs const emptyFnEnv = newCloverIdxs(compiler);
    indexFnClovers(compiler, &emptyFnEnv, fn);
}

} // namespace
