#include "pureloads.hpp"

#include <string.h>
#include <optional>

namespace {

// OPTIMIZE: At this point bitsets are slow because we are usually iterating over them.

// Context Data Structures
// =================================================================================================

struct CloverLoc {
    std::optional<IRName> reg;
};

class CloverLocs {
    CloverLoc* vals_;
    size_t count_;
    Arena* arena_;

    CloverLocs(CloverLoc* vals, size_t count, Arena* arena) :
        vals_{vals}, count_{count}, arena_{arena} {}

public:
    CloverLocs(Arena* arena, BitSet const& vars) :
        vals_{static_cast<CloverLoc*>(amalloc(arena, bitSetLimit(&vars) * sizeof *vals_))},
        count_{bitSetLimit(&vars)},
        arena_{arena}
    {
        for (size_t i = 0; i < count_; ++i) {
            if (bitSetContains(&vars, i)) {
                vals_[i] = CloverLoc{.reg = std::nullopt};
            } else {
                vals_[i] = CloverLoc{.reg = std::optional{invalidIRName}};
            }
        }
    }

    CloverLocs clone() const {
        auto const vals = static_cast<CloverLoc*>(amalloc(arena_, count_ * sizeof *vals_));
        memcpy(vals, vals_, count_ * sizeof *vals);
        return CloverLocs{vals, count_, arena_};
    }

    std::optional<CloverLoc> get(IRName name) const {
        size_t const idx = name.index;
        if (idx >= count_) { return std::nullopt; }

        CloverLoc const loc = vals_[idx];
        if (loc.reg && *loc.reg == invalidIRName) {
            return std::nullopt;
        } else {
            return std::optional{loc};
        }
    }

    void set(IRName name, IRName reg) {
        assert(name.index < count_);
        vals_[name.index].reg = std::optional{reg};
    }
};

struct PureLoadsEnv {
    IRName closure;
    CloverLocs locs;

private:
    PureLoadsEnv(IRName t_closure, CloverLocs const& t_locs) : closure{t_closure}, locs{t_locs} {}

public:
    PureLoadsEnv(Arena* arena, IRName t_closure, BitSet const& t_vars) :
        closure{t_closure}, locs{arena, t_vars}
    {}

    PureLoadsEnv clone() const { return PureLoadsEnv{closure, locs.clone()}; }
};

class SavedPureLoadsEnvs {
    AVec<std::optional<PureLoadsEnv>> envs_;

public:
    SavedPureLoadsEnvs(Arena* arena, size_t blockCount) : envs_{arena, blockCount, std::nullopt} {}

    void save(IRLabel label, PureLoadsEnv env) { envs_[label.blockIndex] = std::optional{env}; }

    std::optional<PureLoadsEnv> const& get(IRLabel label) const { return envs_[label.blockIndex]; }
};

// Pass Algorithm
// =================================================================================================

IRName deepLexicalUse(
    Compiler& compiler, PureLoadsEnv& env, Stmts& newStmts, IRName use, ORef maybeSrcLoc
) {
    std::optional<CloverLoc> const optLoc = env.locs.get(use);;
    if (!optLoc) { return use; }
    CloverLoc const loc = *optLoc;

    if (loc.reg) { return *loc.reg; } // Already loaded

    IRName const newReg = renameIRName(&compiler, use);
    pushIRStmt(&compiler, &newStmts, IRStmt{
        .maybeLoc = maybeSrcLoc,
        .clover = {newReg, env.closure, use, 0},
        .type = IRStmt::CLOVER
    });
    env.locs.set(use, newReg);
    return newReg;
}

struct LiftingAnalysis {
    BitSet liftees;
    IRName closure;
};

LiftingAnalysis joinLambdaLiftees(
    Compiler& compiler, SavedPureLoadsEnvs& savedEnvs, IRBlock const& block
) {
    size_t const callerCount = block.callers.count;

    IRName closure = invalidIRName;
    for (size_t i = 0; i < callerCount; ++i) {
        IRLabel const callerLabel = block.callers.vals[i];
        assert(savedEnvs.get(callerLabel));
        IRName const callerClosure = savedEnvs.get(callerLabel)->closure;
        if (i == 0) {
            closure = callerClosure; // Init to first one
        } else if (callerClosure != closure) { // Disagreement on `closure`
            return LiftingAnalysis{
                .liftees = bitSetClone(&compiler.arena, &block.liveIns),
                .closure = invalidIRName
            };
        }
    }

    // At this point all callers share the closure so only lift vars preloaded in all callers:

    BitSet liftees = createBitSet(&compiler.arena, bitSetBitCap(&block.liveIns));
    for (BitSetIter it = newBitSetIter(&block.liveIns);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        bool liftable = true;
        for (size_t i = 0; i < callerCount; ++i) {
            IRLabel const callerLabel = block.callers.vals[i];
            assert(savedEnvs.get(callerLabel));
            PureLoadsEnv const callerEnv = *savedEnvs.get(callerLabel);

            std::optional<CloverLoc> const optLoc = callerEnv.locs.get(liftee);
            if (optLoc && !optLoc->reg) { // In closure & not preloaded
                liftable = false;
                break;
            }
        }

        if (liftable) { bitSetSet(&compiler.arena, &liftees, liftee.index); }
    }

    return LiftingAnalysis{.liftees = liftees, .closure = closure};
}

void liftArgs(
    Compiler& compiler, SavedPureLoadsEnvs& savedEnvs, IRFn& fn, IRLabel label, BitSet liftees
) {
    assert(savedEnvs.get(label));
    PureLoadsEnv env = *savedEnvs.get(label);
    assert(label.blockIndex < fn.blockCount);
    IRBlock& block = *fn.blocks[label.blockIndex];
    IRTransfer& transfer = block.transfer;
    assert(transfer.type == IRTransfer::GOTO);
    Args& args = transfer.gotoo.args;

    for (BitSetIter it = newBitSetIter(&liftees);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        // OPTIMIZE: Does not need to `setCloverReg`, which `deepLexicalUse` will do:
        pushArg(&compiler, &args,
                deepLexicalUse(compiler, env, block.stmts, liftee, transfer.maybeLoc));
    }
}

void liftParams(Compiler& compiler, PureLoadsEnv& env, IRBlock& block, BitSet liftees) {
    for (BitSetIter it = newBitSetIter(&liftees);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        IRName const phi = renameIRName(&compiler, liftee);
        pushIRParam(&compiler, &block, phi);
        env.locs.set(liftee, phi);
    }
}

PureLoadsEnv blockPureLoadsEnv(
    Compiler& compiler, SavedPureLoadsEnvs& savedEnvs, IRFn& fn, IRBlock& block
) {
    switch (block.callers.count) {
    case 0: { // Escaping block; new env from block live-ins:
        assert(block.paramCount > 0);
        IRName const closure = block.params[0];
        return PureLoadsEnv{&compiler.arena, closure, block.liveIns};
    }

    case 1: { // Non-join; env from end of predecessor (live-ins = live-outs of predecessor):
        assert(savedEnvs.get(block.callers.vals[0]));
        return savedEnvs.get(block.callers.vals[0])->clone();
    }

    default: { // Join: lambda-lift some or all of block live-ins:
        LiftingAnalysis const lifting = joinLambdaLiftees(compiler, savedEnvs, block);

        { // Lambda-lift caller args:
            size_t const callerCount = block.callers.count;
            for (size_t i = 0; i < callerCount; ++i) {
                liftArgs(compiler, savedEnvs, fn, block.callers.vals[i], lifting.liftees);
            }
        }

        auto env = PureLoadsEnv{&compiler.arena, lifting.closure, block.liveIns};
        liftParams(compiler, env, block, lifting.liftees);

        return env;
    }
    }
}

void linearizeCloses(
    Compiler& compiler, PureLoadsEnv& env, Stmts& newStmts, Args& dest, ORef maybeLoc,
    BitSet const& closes
) {
    for (BitSetIter it = newBitSetIter(&closes);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }

        IRName const closee =
            deepLexicalUse(compiler, env, newStmts, IRName{maybeIdx.val}, maybeLoc);
        pushArg(&compiler, &dest, closee);
    }
}

IRStmt stmtWithPureLoads(
    Compiler& compiler, PureLoadsEnv& env, Stmts& newStmts, IRStmt stmt
) {
    switch (stmt.type) {
    case IRStmt::GLOBAL_DEF: {
        Define& define = stmt.define;

        define.val = deepLexicalUse(compiler, env, newStmts, define.val, stmt.maybeLoc);
    }; break;

    case IRStmt::GLOBAL_SET: {
        GlobalSet& globalSet = stmt.globalSet;

        globalSet.val = deepLexicalUse(compiler, env, newStmts, globalSet.val, stmt.maybeLoc);
    }; break;

    case IRStmt::GLOBAL: case IRStmt::CONST_DEF: break; // These do not contain any uses

    case IRStmt::CLOVER: assert(false); break; // Should not exist yet

    case IRStmt::METHOD_DEF: {
        MethodDef& methodDef = stmt.methodDef;
        IRFn& fn = methodDef.fn;

        // Domain:
        size_t const domainCount = fn.domain.count;
        for (size_t i = 0; i < domainCount; ++i) {
            fn.domain.vals[i] =
                deepLexicalUse(compiler, env, newStmts, fn.domain.vals[i], stmt.maybeLoc);
        }

        // Method:
        fnWithPureLoads(compiler, fn);
        IRName const closureName = methodDef.name;
        IRName const methodName = renameIRName(&compiler, closureName);
        methodDef.name = methodName;
        pushIRStmt(&compiler, &newStmts, stmt);

        // Closure:
        IRClosure closure =
            IRClosure{.name = closureName, .method = methodName, .closes = methodDef.closes};
        linearizeCloses(compiler, env, newStmts, *closure.closes, stmt.maybeLoc, *fnFreeVars(&fn));
        stmt = IRStmt{stmt.maybeLoc, {.closure = closure}, IRStmt::CLOSURE};
    }; break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP:
        assert(false); break; // Should not exist yet

    case IRStmt::KNOT: break; // Does not contain any uses

    case IRStmt::KNOT_INIT: {
        KnotInitStmt& knotInit = stmt.knotInit;
        knotInit.knot = deepLexicalUse(compiler, env, newStmts, knotInit.knot, stmt.maybeLoc);
        knotInit.v = deepLexicalUse(compiler, env, newStmts, knotInit.v, stmt.maybeLoc);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt& knotGet = stmt.knotGet;
        knotGet.knot = deepLexicalUse(compiler, env, newStmts, knotGet.knot, stmt.maybeLoc);
    }; break;
    }

    return stmt;
}

void transferWithPureLoads(
    Compiler& compiler, SavedPureLoadsEnvs& savedEnvs, PureLoadsEnv& env,
    IRFn const& fn, IRBlock const& block, Stmts& newStmts, IRTransfer& transfer
) {
    switch (transfer.type) {
    case IRTransfer::CALL: {
        Call& call = transfer.call;
        ORef const maybeLoc = transfer.maybeLoc;

        call.callee = deepLexicalUse(compiler, env, newStmts, call.callee, maybeLoc);

        size_t const arity = call.args.count;
        for (size_t i = 0; i < arity; ++i) {
            call.args.names[i] =
                deepLexicalUse(compiler, env, newStmts, call.args.names[i], maybeLoc);
        }

        IRBlock const& retBlock = *fn.blocks[call.retLabel.blockIndex];
        linearizeCloses(compiler, env, newStmts, call.closes, maybeLoc, retBlock.liveIns);
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall& tailcall = transfer.tailcall;
        ORef const maybeLoc = transfer.maybeLoc;

        tailcall.callee = deepLexicalUse(compiler, env, newStmts, tailcall.callee, maybeLoc);
        tailcall.retFrame = deepLexicalUse(compiler, env, newStmts, tailcall.retFrame, maybeLoc);

        size_t const arity = tailcall.args.count;
        for (size_t i = 0; i < arity; ++i) {
            tailcall.args.names[i] =
                deepLexicalUse(compiler, env, newStmts, tailcall.args.names[i], maybeLoc);
        }
    }; break;

    case IRTransfer::IF: {
        IRIf& iff = transfer.iff;
        iff.cond = deepLexicalUse(compiler, env, newStmts, iff.cond, transfer.maybeLoc);

        savedEnvs.save(block.label, env);
    }; break;

    case IRTransfer::GOTO: {
        IRGoto& gotoo = transfer.gotoo;
        ORef const maybeLoc = transfer.maybeLoc;

        size_t const arity = gotoo.args.count;
        for (size_t i = 0; i < arity; ++i) {
            gotoo.args.names[i] =
                deepLexicalUse(compiler, env, newStmts, gotoo.args.names[i], maybeLoc);
        }

        savedEnvs.save(block.label, env);
    }; break;

    case IRTransfer::RETURN: {
        IRReturn& ret = transfer.ret;
        ORef const maybeLoc = transfer.maybeLoc;

        ret.callee = deepLexicalUse(compiler, env, newStmts, ret.callee, maybeLoc);
        ret.arg = deepLexicalUse(compiler, env, newStmts, ret.arg, maybeLoc);
    }; break;
    }
}

void blockWithPureLoads(
    Compiler& compiler, SavedPureLoadsEnvs& savedEnvs, IRFn& fn, IRBlock& block
) {
    PureLoadsEnv env = blockPureLoadsEnv(compiler, savedEnvs, fn, block);

    Stmts newStmts = newStmtsWithCap(&compiler, block.stmts.count);

    size_t const stmtCount = block.stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        pushIRStmt(&compiler, &newStmts,
                   stmtWithPureLoads(compiler, env, newStmts, block.stmts.vals[i]));
    }

    transferWithPureLoads(compiler, savedEnvs, env, fn, block, newStmts, block.transfer);

    block.stmts = newStmts;
}

void fnWithPureLoads(Compiler& compiler, IRFn& fn) {
    auto savedEnvs = SavedPureLoadsEnvs{&compiler.arena, fn.blockCount};

    size_t const blockCount = fn.blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        blockWithPureLoads(compiler, savedEnvs, fn, *fn.blocks[i]);
    }
}

} // namespace
