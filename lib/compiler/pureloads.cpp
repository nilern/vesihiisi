#include "pureloads.hpp"

#include <string.h>
#include <optional>

#include "../util/asmallmap.hpp"

namespace {

// OPTIMIZE: At this point bitsets are slow because we are usually iterating over them.

// Context Data Structures
// =================================================================================================

struct CloverLoc {
    std::optional<IRName> reg;
};

class CloverLocs {
    ASmallMap<IRName, CloverLoc> locs_;

    explicit CloverLocs(ASmallMap<IRName, CloverLoc>&& locs) : locs_{std::move(locs)} {}

public:
    CloverLocs(Arena* arena, BitSet const& vars) : locs_{arena} {
        size_t const count = bitSetLimit(&vars);
        for (size_t i = 0; i < count; ++i) {
            if (bitSetContains(&vars, i)) {
                locs_.set(IRName{i}, CloverLoc{.reg = std::nullopt});
            }
        }
    }

    CloverLocs clone() const { return CloverLocs{locs_.clone()}; }

    std::optional<CloverLoc> get(IRName name) const { return locs_.tryGet(name); }

    void set(IRName name, IRName reg) { locs_.set(name, CloverLoc{std::optional{reg}}); }
};

struct PureLoadsEnv {
    IRName closure;
    CloverLocs locs;

private:
    PureLoadsEnv(IRName t_closure, CloverLocs&& t_locs) :
        closure{t_closure}, locs{std::move(t_locs)}
    {}

public:
    PureLoadsEnv(Arena* arena, IRName t_closure, BitSet const& t_vars) :
        closure{t_closure}, locs{arena, t_vars}
    {}

    PureLoadsEnv clone() const { return PureLoadsEnv{closure, locs.clone()}; }
};

class SavedPureLoadsEnvs {
    std::optional<PureLoadsEnv>* envs_;

public:
    SavedPureLoadsEnvs(Arena* arena, size_t blockCount) :
        envs_{static_cast<decltype(envs_)>(acalloc(arena, blockCount, sizeof *envs_))}
    {}

    void save(IRLabel label, PureLoadsEnv const& env) {
        envs_[label.blockIndex] = std::optional{env.clone()};
    }

    std::optional<PureLoadsEnv> const& get(IRLabel label) const { return envs_[label.blockIndex]; }
};

// Pass Algorithm
// =================================================================================================

IRName deepLexicalUse(
    Compiler& compiler, PureLoadsEnv& env, AVec<IRStmt>& newStmts, IRName use, ORef maybeSrcLoc
) {
    std::optional<CloverLoc> const optLoc = env.locs.get(use);;
    if (!optLoc) { return use; }
    CloverLoc const loc = *optLoc;

    if (loc.reg) { return *loc.reg; } // Already loaded

    IRName const newReg = renameIRName(&compiler, use);
    newStmts.push(IRStmt{Clover{newReg, env.closure, use, 0}, maybeSrcLoc});
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
    size_t const callerCount = block.callers.count();

    IRName closure = invalidIRName;
    for (size_t i = 0; i < callerCount; ++i) {
        IRLabel const callerLabel = block.callers[i];
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
            IRLabel const callerLabel = block.callers[i];
            assert(savedEnvs.get(callerLabel));
            PureLoadsEnv const& callerEnv = *savedEnvs.get(callerLabel);

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
    PureLoadsEnv env = savedEnvs.get(label)->clone();
    assert(label.blockIndex < fn.blocks.count());
    IRBlock& block = *fn.blocks[label.blockIndex];
    IRTransfer& transfer = block.transfer;
    assert(transfer.type == IRTransfer::GOTO);
    AVec<IRName>& args = transfer.gotoo.args;

    for (BitSetIter it = newBitSetIter(&liftees);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        // OPTIMIZE: Does not need to `setCloverReg`, which `deepLexicalUse` will do:
        args.push(deepLexicalUse(compiler, env, block.stmts, liftee, transfer.maybeLoc));
    }
}

void liftParams(Compiler& compiler, PureLoadsEnv& env, IRBlock& block, BitSet liftees) {
    for (BitSetIter it = newBitSetIter(&liftees);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }
        IRName const liftee = {maybeIdx.val};

        IRName const phi = renameIRName(&compiler, liftee);
        block.params.push(phi);
        env.locs.set(liftee, phi);
    }
}

PureLoadsEnv blockPureLoadsEnv(
    Compiler& compiler, SavedPureLoadsEnvs& savedEnvs, IRFn& fn, IRBlock& block
) {
    switch (block.callers.count()) {
    case 0: { // Escaping block; new env from block live-ins:
        assert(block.params.count() > 0);
        IRName const closure = block.params[0];
        return PureLoadsEnv{&compiler.arena, closure, block.liveIns};
    }

    case 1: { // Non-join; env from end of predecessor (live-ins = live-outs of predecessor):
        assert(savedEnvs.get(block.callers[0]));
        return savedEnvs.get(block.callers[0])->clone();
    }

    default: { // Join: lambda-lift some or all of block live-ins:
        LiftingAnalysis const lifting = joinLambdaLiftees(compiler, savedEnvs, block);

        { // Lambda-lift caller args:
            size_t const callerCount = block.callers.count();
            for (size_t i = 0; i < callerCount; ++i) {
                liftArgs(compiler, savedEnvs, fn, block.callers[i], lifting.liftees);
            }
        }

        auto env = PureLoadsEnv{&compiler.arena, lifting.closure, block.liveIns};
        liftParams(compiler, env, block, lifting.liftees);

        return env;
    }
    }
}

void linearizeCloses(
    Compiler& compiler, PureLoadsEnv& env, AVec<IRStmt>& newStmts, AVec<IRName>& dest,
    ORef maybeLoc, BitSet const& closes
) {
    for (BitSetIter it = newBitSetIter(&closes);;) {
        Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
        if (!maybeIdx.hasVal) { break; }

        IRName const closee =
            deepLexicalUse(compiler, env, newStmts, IRName{maybeIdx.val}, maybeLoc);
        dest.push(closee);
    }
}

IRStmt stmtWithPureLoads(
    Compiler& compiler, PureLoadsEnv& env, AVec<IRStmt>& newStmts, IRStmt stmt
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
        ORef const maybeLoc = stmt.maybeLoc;
        MethodDef& methodDef = stmt.methodDef;
        IRFn& fn = methodDef.fn;

        // Domain:
        size_t const domainCount = fn.domain.count;
        for (size_t i = 0; i < domainCount; ++i) {
            fn.domain.vals[i] =
                deepLexicalUse(compiler, env, newStmts, fn.domain.vals[i], maybeLoc);
        }

        // Method:
        fnWithPureLoads(compiler, fn);
        IRName const closureName = methodDef.name;
        IRName const methodName = renameIRName(&compiler, closureName);
        methodDef.name = methodName;
        newStmts.push(std::move(stmt));

        // Closure:
        IRClosure closure =
            IRClosure{.name = closureName, .method = methodName, .closes = methodDef.closes};
        linearizeCloses(compiler, env, newStmts, *closure.closes, maybeLoc, *fn.freeVars());
        stmt = IRStmt{closure, maybeLoc};
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
    IRFn const& fn, IRBlock const& block, AVec<IRStmt>& newStmts, IRTransfer& transfer
) {
    switch (transfer.type) {
    case IRTransfer::CALL: {
        Call& call = transfer.call;
        ORef const maybeLoc = transfer.maybeLoc;

        call.callee = deepLexicalUse(compiler, env, newStmts, call.callee, maybeLoc);

        size_t const arity = call.args.count();
        for (size_t i = 0; i < arity; ++i) {
            call.args[i] = deepLexicalUse(compiler, env, newStmts, call.args[i], maybeLoc);
        }

        IRBlock const& retBlock = *fn.blocks[call.retLabel.blockIndex];
        linearizeCloses(compiler, env, newStmts, call.closes, maybeLoc, retBlock.liveIns);
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall& tailcall = transfer.tailcall;
        ORef const maybeLoc = transfer.maybeLoc;

        tailcall.callee = deepLexicalUse(compiler, env, newStmts, tailcall.callee, maybeLoc);
        tailcall.retFrame = deepLexicalUse(compiler, env, newStmts, tailcall.retFrame, maybeLoc);

        size_t const arity = tailcall.args.count();
        for (size_t i = 0; i < arity; ++i) {
            tailcall.args[i] = deepLexicalUse(compiler, env, newStmts, tailcall.args[i], maybeLoc);
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

        size_t const arity = gotoo.args.count();
        for (size_t i = 0; i < arity; ++i) {
            gotoo.args[i] = deepLexicalUse(compiler, env, newStmts, gotoo.args[i], maybeLoc);
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

    auto newStmts = AVec<IRStmt>{&compiler.arena, block.stmts.count()};

    size_t const stmtCount = block.stmts.count();
    for (size_t i = 0; i < stmtCount; ++i) {
        newStmts.push(stmtWithPureLoads(compiler, env, newStmts, std::move(block.stmts[i])));
    }

    transferWithPureLoads(compiler, savedEnvs, env, fn, block, newStmts, block.transfer);

    block.stmts = std::move(newStmts);
}

void fnWithPureLoads(Compiler& compiler, IRFn& fn) {
    auto savedEnvs = SavedPureLoadsEnvs{&compiler.arena, fn.blocks.count()};

    size_t const blockCount = fn.blocks.count();
    for (size_t i = 0; i < blockCount; ++i) {
        blockWithPureLoads(compiler, savedEnvs, fn, *fn.blocks[i]);
    }
}

} // namespace
