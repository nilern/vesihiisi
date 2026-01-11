#include "pureloads.hpp"

#include <string.h>

namespace {

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

inline void freeCloverLocs(CloverLocs* locs) { free(locs->vals); }

CloverLocs newCloverLocs(BitSet const vars) {
    size_t const count = bitSetLimit(&vars);
    CloverLoc* const vals = (CloverLoc*)malloc(count * sizeof *vals);

    for (size_t i = 0; i < count; ++i) {
        if (bitSetContains(&vars, i)) {
            vals[i] = CloverLoc{.reg = {}};
        } else {
            vals[i] = CloverLoc{.reg = {.val = invalidIRName, .hasVal = true}};
        }
    }

    return CloverLocs{.vals = vals, .count = count};
}

CloverLocs cloneCloverLocs(CloverLocs const locs) {
    size_t const count = locs.count;
    CloverLoc* const vals = (CloverLoc*)malloc(count * sizeof *vals);

    memcpy(vals, locs.vals, count * sizeof *vals);

    return CloverLocs{.vals = vals, .count = count};
}

typedef struct MaybeCloverLoc {
    CloverLoc val;
    bool hasVal;
} MaybeCloverLoc;

MaybeCloverLoc getCloverLoc(CloverLocs const locs, IRName name) {
    size_t const idx = name.index;
    if (idx >= locs.count) { return MaybeCloverLoc{}; }

    CloverLoc const loc = locs.vals[idx];
    if (loc.reg.hasVal && irNameEq(loc.reg.val, invalidIRName)) {
        return MaybeCloverLoc{};
    } else {
        return MaybeCloverLoc{.val = loc, .hasVal = true};
    }
}

void setCloverReg(CloverLocs* locs, IRName name, IRName reg) {
    assert(name.index < locs->count);
    locs->vals[name.index].reg = MaybeIRName{.val = reg, .hasVal = true};
}

typedef struct PureLoadsEnv {
    IRName closure;
    CloverLocs locs;
} PureLoadsEnv;

inline void freePureLoadsEnv(PureLoadsEnv* env) { freeCloverLocs(&env->locs); }

inline PureLoadsEnv newPureLoadsEnv(IRName closure, BitSet const vars) {
    return PureLoadsEnv{
        .closure = closure,
        .locs = newCloverLocs(vars)
    };
}

inline PureLoadsEnv clonePureLoadsEnv(PureLoadsEnv const env) {
    return PureLoadsEnv{
        .closure = env.closure,
        .locs = cloneCloverLocs(env.locs)
    };
}

typedef struct MaybePureLoadsEnv {
    PureLoadsEnv val;
    bool hasVal;
} MaybePureLoadsEnv;

// TODO: `struct SavedPureLoadsEnvs` to avoid passing size separately:
void freeSavedEnvs(MaybePureLoadsEnv* savedEnvs, size_t blockCount) {
    for (size_t i = 0; i < blockCount; ++i) {
        MaybePureLoadsEnv* const env = &savedEnvs[i];
        if (env->hasVal) {
            freePureLoadsEnv(&env->val);
        }
    }

    free(savedEnvs);
}

IRName deepLexicalUse(
    Compiler* compiler, PureLoadsEnv* env, Stmts* newStmts, IRName use, ORef maybeSrcLoc
) {
    MaybeCloverLoc const maybeLoc = getCloverLoc(env->locs, use);
    if (!maybeLoc.hasVal) { return use; }
    CloverLoc const loc = maybeLoc.val;

    if (loc.reg.hasVal) { return loc.reg.val; } // Already loaded

    IRName const newReg = renameIRName(compiler, use);
    pushIRStmt(compiler, newStmts, IRStmt{
        .maybeLoc = maybeSrcLoc,
        .clover = {newReg, env->closure, use, 0},
        .type = IRStmt::CLOVER
    });
    setCloverReg(&env->locs, use, newReg);
    return newReg;
}

typedef struct LiftingAnalysis {
    BitSet liftees;
    IRName closure;
} LiftingAnalysis;

LiftingAnalysis joinLambdaLiftees(
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
            return LiftingAnalysis{
                .liftees = bitSetClone(&compiler->arena, &block->liveIns),
                .closure = invalidIRName
            };
        }
    }

    // At this point all callers share the closure so only lift vars preloaded in all callers:

    BitSet liftees = createBitSet(&compiler->arena, bitSetBitCap(&block->liveIns));
    for (BitSetIter it = newBitSetIter(&block->liveIns);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
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

    return LiftingAnalysis{.liftees = liftees, .closure = closure};
}

void liftArgs(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, IRFn* fn, IRLabel label, BitSet liftees
) {
    assert(savedEnvs[label.blockIndex].hasVal);
    PureLoadsEnv* env = &savedEnvs[label.blockIndex].val;
    assert(label.blockIndex < fn->blockCount);
    IRBlock* const block = fn->blocks[label.blockIndex];
    IRTransfer* const transfer = &block->transfer;
    assert(transfer->type == IRTransfer::GOTO);
    Args* const args = &transfer->gotoo.args;

    for (BitSetIter it = newBitSetIter(&liftees);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        // OPTIMIZE: Does not need to `setCloverReg`, which `deepLexicalUse` will do:
        pushArg(compiler, args,
                deepLexicalUse(compiler, env, &block->stmts, liftee, transfer->maybeLoc));
    }
}

void liftParams(Compiler* compiler, PureLoadsEnv* env, IRBlock* block, BitSet liftees) {
    for (BitSetIter it = newBitSetIter(&liftees);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        IRName const phi = renameIRName(compiler, liftee);
        pushIRParam(compiler, block, phi);
        setCloverReg(&env->locs, liftee, phi);
    }
}

PureLoadsEnv blockPureLoadsEnv(
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

        { // Lambda-lift caller args:
            size_t const callerCount = block->callers.count;
            for (size_t i = 0; i < callerCount; ++i) {
                liftArgs(compiler, savedEnvs, fn, block->callers.vals[i], lifting.liftees);
            }
        }

        PureLoadsEnv env = newPureLoadsEnv(lifting.closure, block->liveIns);
        liftParams(compiler, &env, block, lifting.liftees);

        return env;
    }
    }
}

void linearizeCloses(
    Compiler* compiler, PureLoadsEnv* env, Stmts* newStmts, Args* dest, ORef maybeLoc,
    BitSet const* closes
) {
    for (BitSetIter it = newBitSetIter(closes);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }

        IRName const closee =
            deepLexicalUse(compiler, env, newStmts, IRName{maybeIdx.val}, maybeLoc);
        pushArg(compiler, dest, closee);
    }
}

IRStmt stmtWithPureLoads(
    Compiler* compiler, PureLoadsEnv* env, Stmts* newStmts, IRStmt stmt
) {
    switch (stmt.type) {
    case IRStmt::GLOBAL_DEF: {
        GlobalDef* const globalDef = &stmt.globalDef;

        globalDef->val = deepLexicalUse(compiler, env, newStmts, globalDef->val, stmt.maybeLoc);
    }; break;

    case IRStmt::GLOBAL: case IRStmt::CONST_DEF: break; // These do not contain any uses

    case IRStmt::CLOVER: assert(false); break; // Should not exist yet

    case IRStmt::METHOD_DEF: {
        MethodDef* const methodDef = &stmt.methodDef;
        IRFn* const fn = &methodDef->fn;

        // Domain:
        size_t const domainCount = fn->domain.count;
        for (size_t i = 0; i < domainCount; ++i) {
            fn->domain.vals[i] =
                deepLexicalUse(compiler, env, newStmts, fn->domain.vals[i], stmt.maybeLoc);
        }

        // Method:
        fnWithPureLoads(compiler, fn);
        IRName const closureName = methodDef->name;
        IRName const methodName = renameIRName(compiler, closureName);
        methodDef->name = methodName;
        pushIRStmt(compiler, newStmts, stmt);

        // Closure:
        IRClosure closure =
            IRClosure{.name = closureName, .method = methodName, .closes = methodDef->closes};
        linearizeCloses(compiler, env, newStmts, closure.closes, stmt.maybeLoc, fnFreeVars(fn));
        stmt = IRStmt{stmt.maybeLoc, {.closure = closure}, IRStmt::CLOSURE};
    }; break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP:
        assert(false); break; // Should not exist yet

    case IRStmt::KNOT: break; // Does not contain any uses

    case IRStmt::KNOT_INIT: {
        KnotInitStmt* const knotInit = &stmt.knotInit;
        knotInit->knot = deepLexicalUse(compiler, env, newStmts, knotInit->knot, stmt.maybeLoc);
        knotInit->v = deepLexicalUse(compiler, env, newStmts, knotInit->v, stmt.maybeLoc);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt* const knotGet = &stmt.knotGet;
        knotGet->knot = deepLexicalUse(compiler, env, newStmts, knotGet->knot, stmt.maybeLoc);
    }; break;
    }

    return stmt;
}

void transferWithPureLoads(
    Compiler* compiler, MaybePureLoadsEnv* savedEnvs, PureLoadsEnv* env,
    IRFn const* fn, IRBlock const* block, Stmts* newStmts, IRTransfer* transfer
) {
    switch (transfer->type) {
    case IRTransfer::CALL: {
        Call* const call = &transfer->call;
        ORef const maybeLoc = transfer->maybeLoc;

        call->callee = deepLexicalUse(compiler, env, newStmts, call->callee, maybeLoc);

        size_t const arity = call->args.count;
        for (size_t i = 0; i < arity; ++i) {
            call->args.names[i] =
                deepLexicalUse(compiler, env, newStmts, call->args.names[i], maybeLoc);
        }

        IRBlock const* const retBlock = fn->blocks[call->retLabel.blockIndex];
        linearizeCloses(compiler, env, newStmts, &call->closes, maybeLoc, &retBlock->liveIns);

        freePureLoadsEnv(env);
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall* const tailcall = &transfer->tailcall;
        ORef const maybeLoc = transfer->maybeLoc;

        tailcall->callee = deepLexicalUse(compiler, env, newStmts, tailcall->callee, maybeLoc);
        tailcall->retFrame = deepLexicalUse(compiler, env, newStmts, tailcall->retFrame, maybeLoc);

        size_t const arity = tailcall->args.count;
        for (size_t i = 0; i < arity; ++i) {
            tailcall->args.names[i] =
                deepLexicalUse(compiler, env, newStmts, tailcall->args.names[i], maybeLoc);
        }

        freePureLoadsEnv(env);
    }; break;

    case IRTransfer::IF: {
        IRIf* const iff = &transfer->iff;
        iff->cond = deepLexicalUse(compiler, env, newStmts, iff->cond, transfer->maybeLoc);

        savedEnvs[block->label.blockIndex] = MaybePureLoadsEnv{.val = *env, .hasVal = true};
    }; break;

    case IRTransfer::GOTO: {
        IRGoto* const gotoo = &transfer->gotoo;
        ORef const maybeLoc = transfer->maybeLoc;

        size_t const arity = gotoo->args.count;
        for (size_t i = 0; i < arity; ++i) {
            gotoo->args.names[i] =
                deepLexicalUse(compiler, env, newStmts, gotoo->args.names[i], maybeLoc);
        }

        savedEnvs[block->label.blockIndex] = MaybePureLoadsEnv{.val = *env, .hasVal = true};
    }; break;

    case IRTransfer::RETURN: {
        IRReturn* const ret = &transfer->ret;
        ORef const maybeLoc = transfer->maybeLoc;

        ret->callee = deepLexicalUse(compiler, env, newStmts, ret->callee, maybeLoc);
        ret->arg = deepLexicalUse(compiler, env, newStmts, ret->arg, maybeLoc);

        freePureLoadsEnv(env);
    }; break;
    }
}

void blockWithPureLoads(
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

void fnWithPureLoads(Compiler* compiler, IRFn* fn) {
    MaybePureLoadsEnv* const savedEnvs =
        (MaybePureLoadsEnv*)calloc(fn->blockCount, sizeof *savedEnvs);

    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        blockWithPureLoads(compiler, savedEnvs, fn, fn->blocks[i]);
    }

    freeSavedEnvs(savedEnvs, fn->blockCount);
}

} // namespace
