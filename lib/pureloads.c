#include "pureloads.h"

#include <string.h>

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

static IRName deepLexicalUse(Compiler* compiler, PureLoadsEnv* env, Stmts* newStmts, IRName use) {
    MaybeCloverLoc const maybeLoc = getCloverLoc(env->locs, use);
    if (!maybeLoc.hasVal) { return use; }
    CloverLoc const loc = maybeLoc.val;

    if (loc.reg.hasVal) { return loc.reg.val; } // Already loaded

    IRName const newReg = renameIRName(compiler, use);
    pushIRStmt(compiler, newStmts, (IRStmt){
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

static LiftingAnalysis joinLambdaLiftees(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, IRBlock const* block
) {
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
                .liftees = bitSetClone(&compiler->arena, &block->liveIns),
                .closure = invalidIRName
            };
        }
    }

    // At this point all callers share the closure so only lift vars preloaded in all callers:

    BitSet liftees = createBitSet(&compiler->arena, bitSetBitCap(&block->liveIns));
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

        if (liftable) { bitSetSet(&compiler->arena, &liftees, liftee.index); }
    }

    return (LiftingAnalysis){.liftees = liftees, .closure = closure};
}

static void liftArgs(
    Compiler* compiler, PureLoadsEnv* env, Stmts* newStmts, Args* args, BitSet liftees
) {
    for (BitSetIter it = newBitSetIter(&liftees);;) {
        MaybeSize const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        // OPTIMIZE: Does not need to `setCloverReg`, which `deepLexicalUse` will do:
        pushArg(compiler, args, deepLexicalUse(compiler, env, newStmts, liftee));
    }
}

static void liftParams(Compiler* compiler, PureLoadsEnv* env, IRBlock* block, BitSet liftees) {
    for (BitSetIter it = newBitSetIter(&liftees);;) {
        MaybeSize const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        IRName const phi = renameIRName(compiler, liftee);
        pushIRParam(compiler, block, phi);
        setCloverReg(&env->locs, liftee, phi);
    }
}

static PureLoadsEnv blockPureLoadsEnv(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, IRFn* fn, IRBlock* block
) {
    switch (block->callers.count) {
    case 0: { // Escaping block; new env from block live-ins:
        assert(block->paramCount > 0);
        IRName const closure = block->params[0];
        return newPureLoadsEnv(closure, block->liveIns);
    }

    case 1: { // Non-join; env from end of predecessor (live-ins = live-outs of predecessor):
        assert(savedEnvs[block->callers.vals[0].blockIndex].hasVal);
        return clonePureLoadsEnv(savedEnvs[block->callers.vals[0].blockIndex].val);
    }

    default: { // Join: lambda-lift some or all of block live-ins:
        LiftingAnalysis const lifting = joinLambdaLiftees(compiler, savedEnvs, block);

        {
            size_t const callerCount = block->callers.count;
            for (size_t i = 0; i < callerCount; ++i) {
                IRLabel const callerLabel = block->callers.vals[i];

                assert(savedEnvs[callerLabel.blockIndex].hasVal);
                PureLoadsEnv* callerEnv = &savedEnvs[callerLabel.blockIndex].val;
                assert(callerLabel.blockIndex < fn->blockCount);
                IRBlock* const caller = fn->blocks[callerLabel.blockIndex];
                IRTransfer* const callerTransfer = &caller->transfer;
                assert(callerTransfer->type == TRANSFER_GOTO);
                Args* const callerArgs = &callerTransfer->gotoo.args;
                liftArgs(compiler, callerEnv, &caller->stmts, callerArgs, lifting.liftees);
            }
        }

        PureLoadsEnv env = newPureLoadsEnv(lifting.closure, block->liveIns);
        liftParams(compiler, &env, block, lifting.liftees);

        return env;
    }
    }
}

static void linearizeCloses(
    Compiler* compiler, PureLoadsEnv* env, Stmts* newStmts, Args* dest, BitSet const* closes
) {
    for (BitSetIter it = newBitSetIter(closes);;) {
        MaybeSize const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }

        IRName const closee = deepLexicalUse(compiler, env, newStmts, (IRName){maybeIdx.val});
        pushArg(compiler, dest, closee);
    }
}

static IRStmt stmtWithPureLoads(
    Compiler* compiler, PureLoadsEnv* env, Stmts* newStmts, IRStmt stmt
) {
    switch (stmt.type) {
    case STMT_GLOBAL_DEF: {
        GlobalDef* const globalDef = &stmt.globalDef;

        globalDef->val = deepLexicalUse(compiler, env, newStmts, globalDef->val);
    }; break;

    case STMT_GLOBAL: case STMT_CONST_DEF: break; // These do not contain any uses

    case STMT_CLOVER: assert(false); break; // Should not exist yet

    case STMT_METHOD_DEF: {
        MethodDef* const methodDef = &stmt.methodDef;
        IRFn* const fn = &methodDef->fn;

        // Domain:
        size_t const domainCount = fn->domain.count;
        for (size_t i = 0; i < domainCount; ++i) {
            fn->domain.vals[i] = deepLexicalUse(compiler, env, newStmts, fn->domain.vals[i]);
        }

        // Method:
        fnWithPureLoads(compiler, fn);
        IRName const closureName = methodDef->name;
        IRName const methodName = renameIRName(compiler, closureName);
        methodDef->name = methodName;
        pushIRStmt(compiler, newStmts, stmt);

        // Closure:
        IRClosure closure =
            (IRClosure){.name = closureName, .method = methodName, .closes = methodDef->closes};
        linearizeCloses(compiler, env, newStmts, closure.closes, fnFreeVars(fn));
        stmt = (IRStmt){.closure = closure, STMT_CLOSURE};
    }; break;

    case STMT_CLOSURE: case STMT_MOVE: case STMT_SWAP: assert(false); break; // Should not exist yet

    case STMT_KNOT: break; // Does not contain any uses

    case STMT_KNOT_INIT: {
        KnotInitStmt* const knotInit = &stmt.knotInit;
        knotInit->knot = deepLexicalUse(compiler, env, newStmts, knotInit->knot);
        knotInit->v = deepLexicalUse(compiler, env, newStmts, knotInit->v);
    }; break;

    case STMT_KNOT_GET: {
        KnotGetStmt* const knotGet = &stmt.knotGet;
        knotGet->knot = deepLexicalUse(compiler, env, newStmts, knotGet->knot);
    }; break;
    }

    return stmt;
}

static void transferWithPureLoads(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, PureLoadsEnv* env,
    IRFn const* fn, IRBlock const* block, Stmts* newStmts, IRTransfer* transfer
) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        Call* const call = &transfer->call;

        call->callee = deepLexicalUse(compiler, env, newStmts, call->callee);

        size_t const arity = call->args.count;
        for (size_t i = 0; i < arity; ++i) {
            call->args.names[i] =
                deepLexicalUse(compiler, env, newStmts, call->args.names[i]);
        }

        IRBlock const* const retBlock = fn->blocks[call->retLabel.blockIndex];
        linearizeCloses(compiler, env, newStmts, &call->closes, &retBlock->liveIns);

        freePureLoadsEnv(env);
    }; break;

    case TRANSFER_TAILCALL: {
        Tailcall* const tailcall = &transfer->tailcall;

        tailcall->callee = deepLexicalUse(compiler, env, newStmts, tailcall->callee);
        tailcall->retFrame = deepLexicalUse(compiler, env, newStmts, tailcall->retFrame);

        size_t const arity = tailcall->args.count;
        for (size_t i = 0; i < arity; ++i) {
            tailcall->args.names[i] =
                deepLexicalUse(compiler, env, newStmts, tailcall->args.names[i]);
        }

        freePureLoadsEnv(env);
    }; break;

    case TRANSFER_IF: {
        IRIf* const iff = &transfer->iff;
        iff->cond = deepLexicalUse(compiler, env, newStmts, iff->cond);

        savedEnvs[block->label.blockIndex] = (MaybePureLoadsEnv){.val = *env, .hasVal = true};
    }; break;

    case TRANSFER_GOTO: {
        IRGoto* const gotoo = &transfer->gotoo;

        size_t const arity = gotoo->args.count;
        for (size_t i = 0; i < arity; ++i) {
            gotoo->args.names[i] = deepLexicalUse(compiler, env, newStmts, gotoo->args.names[i]);
        }

        savedEnvs[block->label.blockIndex] = (MaybePureLoadsEnv){.val = *env, .hasVal = true};
    }; break;

    case TRANSFER_RETURN: {
        IRReturn* const ret = &transfer->ret;

        ret->callee = deepLexicalUse(compiler, env, newStmts, ret->callee);
        ret->arg = deepLexicalUse(compiler, env, newStmts, ret->arg);

        freePureLoadsEnv(env);
    }; break;
    }
}

static void blockWithPureLoads(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, IRFn* fn, IRBlock* block
) {
    PureLoadsEnv env = blockPureLoadsEnv(compiler, savedEnvs, fn, block);

    Stmts newStmts = newStmtsWithCap(compiler, block->stmts.count);

    size_t const stmtCount = block->stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        pushIRStmt(compiler, &newStmts,
                   stmtWithPureLoads(compiler, &env, &newStmts, block->stmts.vals[i]));
    }

    transferWithPureLoads(compiler, savedEnvs, &env, fn, block, &newStmts, &block->transfer);

    block->stmts = newStmts;
}

static void fnWithPureLoads(Compiler* compiler, IRFn* fn) {
    MaybePureLoadsEnv* const savedEnvs = calloc(fn->blockCount, sizeof *savedEnvs);

    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        blockWithPureLoads(compiler, savedEnvs, fn, fn->blocks[i]);
    }

    freeSavedEnvs(savedEnvs, fn->blockCount);
}
