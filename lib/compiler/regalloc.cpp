#include "regalloc.hpp"

#include <string.h>

#include "../util/util.hpp"
#include "../rt.hpp"
#include "../bytecode.hpp"

namespace {

struct Move {
    Reg dest;
    Reg src;
};

// OPTIMIZE: At this point bitsets are slow because we are usually iterating over them.

// Bidirectional Variable-Register Mappings
// =================================================================================================

class RegEnv {
    std::optional<Reg>* varRegs_;
    IRName* regVars_;
    size_t maxVarCount_; // `.index` of max allocated register + 1
public:
    IRName retName;
private:
    size_t varCap_;
    Arena* arena_;

    RegEnv(
        std::optional<Reg>* varRegs, IRName* regVars, size_t maxVarCount, IRName t_retName,
        size_t varCap, Arena* arena
    ) :
        varRegs_{varRegs}, regVars_{regVars}, maxVarCount_{maxVarCount}, retName{t_retName},
        varCap_{varCap}, arena_{arena}
    {}

    void ensureRegEnvMaxCount(size_t count) {
        if (maxVarCount_ < count) { maxVarCount_ = count; }
    }

    void shrinkRegEnvMaxVarCount() {
        size_t max = maxVarCount_;
        for (; max > 0 && isRegFree(Reg{uint8_t(max - 1)}); --max) {}
        maxVarCount_ = max;
    }

public:
    RegEnv(Compiler& compiler, IRName t_retName) :
        varRegs_{static_cast<decltype(varRegs_)>(
            amalloc(&compiler.arena, compiler.nameSyms.count() * sizeof *varRegs_))},
        regVars_{static_cast<decltype(regVars_)>(
            acalloc(&compiler.arena, REG_COUNT, sizeof *regVars_))},
        maxVarCount_{0},
        retName{t_retName},
        varCap_{compiler.nameSyms.count()},
        arena_{&compiler.arena}
    {
        std::fill(varRegs_, varRegs_ + varCap_, std::nullopt);
    }

    RegEnv clone() const {
        decltype(varRegs_) const cloneVarRegs = static_cast<decltype(varRegs_)>(
            amalloc(arena_, varCap_ * sizeof *cloneVarRegs));
        std::copy(varRegs_, varRegs_ + varCap_, cloneVarRegs);
        decltype(regVars_) const cloneRegVars = static_cast<decltype(regVars_)>(
            amalloc(arena_, REG_COUNT * sizeof *cloneRegVars));
        std::copy(regVars_, regVars_ + REG_COUNT, cloneRegVars);

        return RegEnv{cloneVarRegs, cloneRegVars, maxVarCount_, retName, varCap_, arena_};
    }

    size_t maxVarCount() const { return maxVarCount_; }

    std::optional<Reg> tryVarReg(IRName var) const { return varRegs_[var.index]; }

    IRName tryRegVar(Reg reg) const { return regVars_[reg.index]; }

    bool isRegFree(Reg reg) const { return tryRegVar(reg) == invalidIRName; }

    void add(IRName var, Reg reg) {
        assert(!varRegs_[var.index]);
        assert(regVars_[reg.index] == invalidIRName);

        varRegs_[var.index] = std::optional{reg};
        regVars_[reg.index] = var;
        ensureRegEnvMaxCount(reg.index + 1);
    }

    Reg allocVarReg(IRName var) {
        for (size_t i = /*reserve "stack pointer":*/ retContReg + 1; i < REG_COUNT; ++i) {
            Reg const reg = Reg{(uint8_t)i};

            // Will be true when `i == maxVarCount` at the latest:
            if (isRegFree(reg)) {
                varRegs_[var.index] = std::optional{reg};
                regVars_[i] = var;
                ensureRegEnvMaxCount(i + 1);
                return reg;
            }
        }

        // FIXME: Unlikely to happen, but still should fail just compile instead:
        PANIC("Out of registers");
    }

    struct AllocStmtArgRegRes {
        Reg reg;
        std::optional<Move> maybeMove;
    };

    [[nodiscard]]
    AllocStmtArgRegRes allocStmtArgReg(IRName var) {
        std::optional<Reg> const optDest = varRegs_[var.index];

        Reg const reg = allocVarReg(var);

        return AllocStmtArgRegRes{
            reg,
            optDest ? std::optional{Move{.dest = *optDest, .src = reg}} : std::nullopt
        };
    }

    [[nodiscard]]
    std::optional<Move> allocTransferArgReg(IRName var, Reg reg, bool delayDupDeallocs) {
        assert(isRegFree(reg));

        std::optional<Reg> const optDest = varRegs_[var.index];
        if (!delayDupDeallocs && optDest) {
            regVars_[optDest->index] = invalidIRName;
            // No need to `shrinkRegEnvMaxVarCount` since it will get grown to `reg.index + 1`:
            assert(optDest->index < reg.index);
        }

        varRegs_[var.index] = std::optional{reg};
        regVars_[reg.index] = var;
        ensureRegEnvMaxCount(reg.index + 1);

        return optDest
                   ? std::optional{Move{.dest = *optDest, .src = reg}}
                   : std::nullopt;
    }

    void delayedDeallocTransferArgRegs(Slice<Move const> moves) {
        size_t const moveCount = moves.count;
        for (size_t i = 0; i < moveCount; ++i) {
            Reg const dest = moves[i].dest;
            assert(!isRegFree(dest));

            regVars_[dest.index] = invalidIRName;
        }

        shrinkRegEnvMaxVarCount();
    }

    Reg deallocVarReg(IRName var) {
        if (!varRegs_[var.index]) {
            allocVarReg(var); // OPTIMIZE: Will be immediately deallocated:
        }

        Reg const reg = *varRegs_[var.index];
        varRegs_[var.index] = std::nullopt;
        regVars_[reg.index] = invalidIRName;
        shrinkRegEnvMaxVarCount();

        return reg;
    }

    void deallocDupReg(Reg reg) {
        regVars_[reg.index] = invalidIRName;
        shrinkRegEnvMaxVarCount();
    }

    Reg getVarReg(IRName var) {
        std::optional<Reg> const optReg = tryVarReg(var);
        if (optReg) { return *optReg; }

        return allocVarReg(var);
    }

    [[nodiscard]]
    std::optional<Move> regEnvParamToArg(Reg paramReg, IRName arg) {
        assert(regVars_[paramReg.index] != invalidIRName);
        assert(arg != invalidIRName);

        std::optional<Reg> const optDest = varRegs_[arg.index];
        if (optDest) {
            regVars_[optDest->index] = invalidIRName;
            shrinkRegEnvMaxVarCount();
        }

        IRName const param = regVars_[paramReg.index];
        varRegs_[param.index] = std::nullopt;
        varRegs_[arg.index] = std::optional{paramReg};
        regVars_[paramReg.index] = arg;

        return optDest
                   ? std::optional{Move{.dest = *optDest, .src = paramReg}}
                   : std::nullopt;
    }

    void regEnvMove(IRName var, Reg src, Reg dest) {
        assert(regVars_[src.index] == var);
        assert(regVars_[dest.index] == invalidIRName);

        varRegs_[var.index] = std::optional{dest};
        regVars_[dest.index] = var;
        regVars_[src.index] = invalidIRName;
        shrinkRegEnvMaxVarCount();
    }

    void regEnvSwap(IRName var1, Reg reg1, IRName var2, Reg reg2) {
        assert(regVars_[reg1.index] == var1);
        assert(regVars_[reg2.index] == var2);

        varRegs_[var1.index] = std::optional{reg2};
        regVars_[reg2.index] = var1;
        varRegs_[var2.index] = std::optional{reg1};
        regVars_[reg1.index] = var2;
    }
};

class SavedRegEnvs {
    AVec<std::optional<RegEnv>> envs_;

public:
    SavedRegEnvs(Arena* arena, size_t blockCount) : envs_{arena, blockCount, std::nullopt} {}

    void save(IRLabel label, RegEnv env) { envs_[label.blockIndex] = std::optional{env}; }

    std::optional<RegEnv> const& get(IRLabel label) const { return envs_[label.blockIndex]; }
};

// Register Allocation Over IR
// =================================================================================================

void shuffleRegs(
    Compiler& compiler, RegEnv& current, Stmts& outputStmts, RegEnv const& goal, ORef const maybeLoc
) {
    // Iterate until no line ends = `mov` all lines away:
    for (bool foundLineEnd = true; foundLineEnd;) {
        foundLineEnd = false;

        size_t const maxVarCount = current.maxVarCount();
        for (size_t i = 0; i < maxVarCount; ++i) {
            Reg const reg = {(uint8_t)i};
            IRName const var = current.tryRegVar(reg);
            if (var == invalidIRName) { continue; } // Loop artefact: `reg` is free

            std::optional<Reg> const optGoalReg = goal.tryVarReg(var);
            assert(optGoalReg);
            Reg const goalReg = *optGoalReg;
            if (reg != goalReg) { // Needs move or swap
                if (current.isRegFree(goalReg)) { // Can move now
                    current.regEnvMove(var, reg, goalReg);
                    pushIRStmt(&compiler, &outputStmts, moveToStmt(MoveStmt{
                        .dest = IRName{reg.index},
                        .src = IRName{goalReg.index}
                    }, maybeLoc));

                    foundLineEnd = true;
                }
            }
        }
    }

    // Only cycles remain, handle each with a series of swaps:
    size_t const maxVarCount = current.maxVarCount();
    for (size_t i = 0; i < maxVarCount; ++i) {
        Reg const reg = {(uint8_t)i};
        IRName const var = current.tryRegVar(reg);
        if (var == invalidIRName) { continue; } // Loop artefact: `reg` is free

        std::optional<Reg> const optGoalReg = goal.tryVarReg(var);
        assert(optGoalReg);
        Reg const goalReg = *optGoalReg;
        if (reg != goalReg) { // Needs swap
            IRName const trader = current.tryRegVar(goalReg);
            assert(trader != invalidIRName);

            // Loop-breaking swap:
            current.regEnvSwap(var, reg, trader, goalReg);
            pushIRStmt(&compiler, &outputStmts, swapToStmt(SwapStmt{
                .reg1 = IRName{reg.index},
                .reg2 = IRName{goalReg.index}
            }, maybeLoc));

            // Cascading swaps:
            std::optional<Reg> const optTraderGoalReg = goal.tryVarReg(trader);
            assert(optTraderGoalReg);
            Reg const traderGoalReg = *optTraderGoalReg;
            for (Reg traderReg = reg; traderReg != traderGoalReg;) {
                IRName const taker = goal.tryRegVar(traderReg);
                assert(taker != invalidIRName);
                std::optional<Reg> const optTakerReg = current.tryVarReg(taker);
                assert(optTakerReg);
                Reg const takerReg = *optTakerReg;

                current.regEnvSwap(taker, takerReg, trader, traderReg);
                pushIRStmt(&compiler, &outputStmts, swapToStmt(SwapStmt{
                    .reg1 = IRName{takerReg.index},
                    .reg2 = IRName{traderReg.index}
                }, maybeLoc));

                traderReg = takerReg;
            }
        }
    }
}

RegEnv regAllocIfSuccession(
    Compiler& compiler, SavedRegEnvs& savedEnvs, IRFn& fn, IRLabel conseqLabel, IRLabel altLabel
) {
    assert(savedEnvs.get(conseqLabel));
    RegEnv const& conseqEnv = *savedEnvs.get(conseqLabel); // TODO: Why not cloned like `conseqEnv`?
    assert(savedEnvs.get(altLabel));
    RegEnv altEnv = *savedEnvs.get(altLabel);

    RegEnv goal = conseqEnv.clone(); // OPTIMIZE: Is cloning actually necessary?

    {
        size_t const maxVarCount = altEnv.maxVarCount();
        for (size_t i = 0; i < maxVarCount; ++i) {
            IRName const var = altEnv.tryRegVar(Reg{uint8_t(i)});
            if (var == invalidIRName) { continue; } // Loop artefact: `reg` is free

            // OPTIMIZE: Try to use the same reg as in `altEnv`:
            goal.getVarReg(var); // Just ensure that `var` is in `goal`; discard return value
        }
    }

    assert(altLabel.blockIndex < fn.blockCount);
    IRBlock& altBlock = *fn.blocks[altLabel.blockIndex];
    ORef const maybeLoc = altBlock.stmts.count > 0
        ? altBlock.stmts.vals[0].maybeLoc
        : altBlock.transfer.maybeLoc;
    reverse(altBlock.stmts.vals, altBlock.stmts.count, sizeof *altBlock.stmts.vals, swapStmts);
    shuffleRegs(compiler, altEnv, altBlock.stmts, goal, maybeLoc);
    reverse(altBlock.stmts.vals, altBlock.stmts.count, sizeof *altBlock.stmts.vals, swapStmts);

    return goal;
}

void regAllocBlock(
    Compiler& compiler, SavedRegEnvs& savedEnvs, BitSet& visited, IRFn& fn, IRBlock& block
);

IRName regAllocCallee(RegEnv& env, IRName callee) {
    Reg const reg = Reg{calleeReg};

    [[maybe_unused]] std::optional<Move> const optCalleeMove =
        env.allocTransferArgReg(callee, reg, true);
    assert(!optCalleeMove); // Callees are processed in empty env so no move should result

    return IRName{reg.index};
}

[[nodiscard]]
AVec<Move> regAllocCallArgs(Compiler& compiler, RegEnv& env, Args& args) {
    auto moves = AVec<Move>{&compiler.arena};

    size_t const arity = args.count;
    for (size_t i = 0; i < arity; ++i) {
        Reg const reg = Reg{(uint8_t)(firstArgReg + i)};

        std::optional<Move> const optMove = env.allocTransferArgReg(args.names[i], reg, true);
        if (optMove) {
            moves.push(*optMove);
        }

        args.names[i] = IRName{reg.index};
    }

    return moves;
}

void regAllocTailcallArgs(
    Compiler& compiler, RegEnv& env, IRBlock& block, Args& args, ORef maybeLoc
) {
    size_t const arity = args.count;
    for (size_t i = 0; i < arity; ++i) {
        Reg const reg = Reg{(uint8_t)(firstArgReg + i)};

        std::optional<Move> const optMove = env.allocTransferArgReg(args.names[i], reg, false);
        if (optMove) {
            pushIRStmt(&compiler, &block.stmts, moveToStmt(MoveStmt{
                .dest = IRName{optMove->dest.index},
                .src = IRName{optMove->src.index}
            }, maybeLoc));
        }

        args.names[i] = IRName{reg.index};
    }
}

RegEnv regAllocTransfer(
    Compiler& compiler, SavedRegEnvs& savedEnvs, BitSet& visited, IRFn& fn, IRBlock& block,
    IRTransfer& transfer
) {
    // Transfers pass arguments "in parallel" so we can go forwards. This generates any moves in
    // order of lower to higher destination register.

    switch (transfer.type) {
    case IRTransfer::CALL: {
        Call& call = transfer.call;

        IRLabel const retLabel = call.retLabel;
        assert(retLabel.blockIndex < fn.blockCount);
        IRBlock& retBlock = *fn.blocks[retLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, retBlock);

        IRName const succRetName = [&](){
            assert(savedEnvs.get(retLabel));
            RegEnv const& succEnv = *savedEnvs.get(retLabel);
            return succEnv.retName;
        }();
        IRName const retName = [&](){
            // OPTIMIZE:
            size_t i = 0;
            for (BitSetIter it = newBitSetIter(&retBlock.liveIns);; ++i) {
                Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
                if (!maybeIdx.hasVal) { break; }
                IRName const spillName = IRName{maybeIdx.val};

                if (spillName == succRetName) {
                    return call.closes.names[i];
                }
            }

            return invalidIRName; // Unreachable
        }();
        auto env = RegEnv{compiler, retName};

        call.callee = regAllocCallee(env, call.callee);

        env.add(retName, Reg{retContReg});

        AVec<Move> moves = regAllocCallArgs(compiler, env, call.args);

        for (size_t i = call.closes.count; i-- > 0;) {
            call.closes.names[i] = IRName{env.getVarReg(call.closes.names[i]).index};
        }

        // Do the duplicate arg moves that were delayed to avoid clobbering spillees:
        size_t const moveCount = moves.count();
        for (size_t i = 0; i < moveCount; ++i) {
            Move const move = moves[i];

            pushIRStmt(&compiler, &block.stmts, moveToStmt(MoveStmt{
               .dest = IRName{move.dest.index},
               .src = IRName{move.src.index}
            }, transfer.maybeLoc));
        }
        env.delayedDeallocTransferArgRegs(moves.slice());

        return env;
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall& tailcall = transfer.tailcall;

        auto env = RegEnv{compiler, tailcall.retFrame};

        tailcall.callee = regAllocCallee(env, tailcall.callee);

        Reg const contReg = Reg{retContReg};
        [[maybe_unused]] std::optional<Move> const optContMove =
            env.allocTransferArgReg(tailcall.retFrame, contReg, false);
        assert(!optContMove);
        tailcall.retFrame = IRName{contReg.index};

        regAllocTailcallArgs(compiler, env, block, tailcall.args, transfer.maybeLoc);

        return env;
    }; break;

    case IRTransfer::IF: {
        IRIf& iff = transfer.iff;

        IRLabel const conseqLabel = iff.conseq;
        assert(conseqLabel.blockIndex < fn.blockCount);
        IRBlock& conseqBlock = *fn.blocks[conseqLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, conseqBlock);
        IRLabel const altLabel = iff.alt;
        assert(altLabel.blockIndex < fn.blockCount);
        IRBlock& altBlock = *fn.blocks[altLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, altBlock);

        RegEnv env = regAllocIfSuccession(compiler, savedEnvs, fn, iff.conseq, altLabel);

        iff.cond = IRName{env.getVarReg(iff.cond).index};

        return env;
    }; break;

    case IRTransfer::GOTO: {
        IRGoto& gotoo = transfer.gotoo;
        size_t const arity = gotoo.args.count;

        IRLabel const destLabel = gotoo.dest;
        assert(destLabel.blockIndex < fn.blockCount);
        IRBlock& destBlock = *fn.blocks[destLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, destBlock);

        assert(savedEnvs.get(destLabel));
        RegEnv env = savedEnvs.get(destLabel)->clone();
        for (size_t i = 0; i < arity; ++i) {
            Reg const paramReg = Reg{uint8_t(destBlock.params[i].index)}; // HACK?
            if (env.tryRegVar(paramReg) == env.retName) {
                env.retName = gotoo.args.names[i];
                break;
            }
        }

        assert(destBlock.paramCount == arity);
        for (size_t i = 0; i < arity; ++i) {
            Reg const paramReg = Reg{(uint8_t)destBlock.params[i].index};
            IRName& arg = gotoo.args.names[i];

            std::optional<Move> const optMove = env.regEnvParamToArg(paramReg, arg);
            if (optMove) {
                pushIRStmt(&compiler, &block.stmts, moveToStmt(MoveStmt{
                    .dest = IRName{optMove->dest.index},
                    .src = IRName{optMove->src.index}
                }, transfer.maybeLoc));
            }

            arg = IRName{paramReg.index};
        }

        return env;
    }; break;

    case IRTransfer::RETURN: {
        IRReturn& ret = transfer.ret;

        auto env = RegEnv{compiler, ret.callee};

        Reg const calleeReg = Reg{retContReg};
        [[maybe_unused]] std::optional<Move> const optContMove =
            env.allocTransferArgReg(ret.callee, calleeReg, false);
        assert(!optContMove);
        ret.callee = IRName{calleeReg.index};

        Reg const valReg = Reg{retReg};
        [[maybe_unused]] std::optional<Move> const optValMove =
            env.allocTransferArgReg(ret.arg, valReg, false);
        assert(!optValMove);
        ret.arg = IRName{valReg.index};

        return env;
    }; break;

    default: PANIC("Unreachable code reached");
    }
}

void regAllocStmt(Compiler& compiler, RegEnv& env, Stmts& outputStmts, IRStmt& stmt) {
    switch (stmt.type) {
    case IRStmt::GLOBAL_DEF: {
        Define& define = stmt.define;
        define.val = IRName{env.getVarReg(define.val).index};

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;

    case IRStmt::GLOBAL_SET: {
        GlobalSet& globalSet = stmt.globalSet;
        globalSet.val = IRName{env.getVarReg(globalSet.val).index};

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal& global = stmt.global;
        global.tmpName = IRName{env.deallocVarReg(global.tmpName).index};

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;

    case IRStmt::CONST_DEF: {
        ConstDef& constDef = stmt.constDef;
        constDef.name = IRName{env.deallocVarReg(constDef.name).index};

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;

    case IRStmt::CLOVER: {
        Clover& clover = stmt.clover;

        IRName const name = clover.name;
        clover.name = IRName{env.deallocVarReg(clover.name).index};
        clover.closure = IRName{env.getVarReg(clover.closure).index};

        if (name == env.retName) {
            env.retName = clover.origName;
        }

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;

    case IRStmt::METHOD_DEF: {
        MethodDef& methodDef = stmt.methodDef;
        IRFn& fn = methodDef.fn;

        RegEnv outEnv = env.clone(); // After post-stmt shuffle

        // Clean out registers of live-out specializers and collect non-live-out specializers:
        auto dyerTypes = AVec<IRName>{&compiler.arena};
        for (size_t i = fn.domain.count; i-- > 0;) {
            IRName const typeName = fn.domain.vals[i];
            if (!typeName.isValid()) { continue; } // HACK

            if (env.tryVarReg(typeName)) {
                env.deallocVarReg(typeName);
            } else {
                dyerTypes.push(typeName);
            }
        }

        // Target register (of `methodDef.name`) is not freed so that it does not clobber any
        // live-out specializers.

        // Allocate registers for specializers and collect duplicating moves:
        auto dupMoves = AVec<Move>{&compiler.arena};
        {
            size_t const domainCount = fn.domain.count;
            for (size_t i = 0; i < domainCount; ++i) {
                IRName const typeName = fn.domain.vals[i];
                if (!typeName.isValid()) { continue; } // HACK

                RegEnv::AllocStmtArgRegRes const res = env.allocStmtArgReg(typeName);
                fn.domain.vals[i] = IRName{res.reg.index};
                if (res.maybeMove) {
                    dupMoves.push(*res.maybeMove);
                }
            }
        }

        RegEnv goalOutEnv = env.clone(); // Before post-stmt shuffle
        {
            // Remove move srcs so specializers do not exist in multiple registers at once:
            size_t const dupMoveCount = dupMoves.count();
            for (size_t i = 0; i < dupMoveCount; ++i) {
                goalOutEnv.deallocDupReg(dupMoves[i].src);
            }

            // Remove specializers that are not live-outs:
            size_t const dyerTypeCount = dyerTypes.count();
            for (size_t i = 0; i < dyerTypeCount; ++i) {
                goalOutEnv.deallocVarReg(dyerTypes[i]);
            }
        }

        // Emit post-stmt shuffle:
        shuffleRegs(compiler, outEnv, outputStmts, goalOutEnv, stmt.maybeLoc);

        // Deallocate target register and emit stmt itself:
        methodDef.name = IRName{env.deallocVarReg(methodDef.name).index};
        regAllocFn(compiler, fn);
        pushIRStmt(&compiler, &outputStmts, stmt);

        // Emit pre-stmt duplicating moves:
        {
            size_t const dupMoveCount = dupMoves.count();
            for (size_t i = 0; i < dupMoveCount; ++i) {
                Move const move = dupMoves[i];

                pushIRStmt(&compiler, &outputStmts, moveToStmt(MoveStmt{
                    .dest = IRName{move.dest.index},
                    .src = IRName{move.src.index}
                }, stmt.maybeLoc));
            }
            env.delayedDeallocTransferArgRegs(dupMoves.slice());
        }
    }; break;

    case IRStmt::CLOSURE: {
        IRClosure& closure = stmt.closure;

        closure.name = IRName{env.deallocVarReg(closure.name).index};

        for (size_t i = closure.closes->count; i-- > 0;) {
            closure.closes->names[i] = IRName{env.getVarReg(closure.closes->names[i]).index};
        }

        closure.method = IRName{env.getVarReg(closure.method).index};

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;

    // Generated during this pass, so only copy:
    case IRStmt::MOVE: case IRStmt::SWAP: pushIRStmt(&compiler, &outputStmts, stmt); break;

    case IRStmt::KNOT: {
        KnotStmt& knot = stmt.knot;
        knot.name = IRName{env.deallocVarReg(knot.name).index};

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;

    case IRStmt::KNOT_INIT: {
        KnotInitStmt& knotInit = stmt.knotInit;
        knotInit.v = IRName{env.getVarReg(knotInit.v).index};
        knotInit.knot = IRName{env.getVarReg(knotInit.knot).index};

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt& knotGet = stmt.knotGet;
        knotGet.name = IRName{env.deallocVarReg(knotGet.name).index};
        knotGet.knot = IRName{env.getVarReg(knotGet.knot).index};

        pushIRStmt(&compiler, &outputStmts, stmt);
    }; break;
    }
}

void regAllocParams(Compiler& compiler, RegEnv& env, Stmts& outputStmts, IRBlock& block) {
    if (block.callers.count == 0) { // Escaping block:
        auto goal = RegEnv{compiler, env.retName};

        size_t paramIdx = 0;

        if (block.label.blockIndex == 0) { // Call entry block:
            IRName& callee = block.params[0];
            goal.add(callee, Reg{calleeReg});
            callee = IRName{calleeReg};

            IRName& retCont = block.params[1];
            goal.add(retCont, Reg{retContReg});
            retCont = IRName{retContReg};

            paramIdx = 2;
        } else { // Return entry block:
            IRName& retCont = block.params[0];
            goal.add(retCont, Reg{retContReg});
            retCont = IRName{retContReg};

            paramIdx = 1;
        }

        size_t const arity = block.paramCount;
        for (size_t regIdx = 2; paramIdx < arity; ++paramIdx, ++regIdx) {
            IRName& param = block.params[paramIdx];
            Reg const reg = {(uint8_t)regIdx};
            goal.add(param, reg);
            param = IRName{regIdx};
        }

        ORef const maybeLoc = block.stmts.count > 0
            ? block.stmts.vals[0].maybeLoc
            : block.transfer.maybeLoc;
        shuffleRegs(compiler, env, outputStmts, goal, maybeLoc);
    } else { // Non-escaping block:
        size_t const arity = block.paramCount;
        for (size_t i = 0; i < arity; ++i) {
            IRName& param = block.params[i];
            std::optional<Reg> const optReg = env.tryVarReg(param);
            Reg const reg = optReg ? *optReg : env.allocVarReg(param);
            param = IRName{reg.index};
        }
    }
}

void regAllocBlock(
    Compiler& compiler, SavedRegEnvs& savedEnvs, BitSet& visited, IRFn& fn, IRBlock& block
) {
    IRLabel const label = block.label;
    if (bitSetContains(&visited, label.blockIndex)) { return; }
    bitSetSet(&compiler.arena, &visited, label.blockIndex);

    RegEnv env = regAllocTransfer(compiler, savedEnvs, visited, fn, block, block.transfer);

    Stmts outputStmts = newStmtsWithCap(&compiler, block.stmts.count);

    for (size_t i = block.stmts.count; i-- > 0;) {
        regAllocStmt(compiler, env, outputStmts, block.stmts.vals[i]);
    }

    regAllocParams(compiler, env, outputStmts, block);

    reverse(outputStmts.vals, outputStmts.count, sizeof *outputStmts.vals, swapStmts);
    block.stmts = outputStmts;

    savedEnvs.save(label, env);
}

void regAllocFn(Compiler& compiler, IRFn& fn) {
    assert(fn.blockCount > 0);

    size_t const blockCount = fn.blockCount;
    auto savedEnvs = SavedRegEnvs{&compiler.arena, blockCount};
    BitSet visited = createBitSet(&compiler.arena, blockCount);

    regAllocBlock(compiler, savedEnvs, visited, fn, *fn.blocks[0]);
}

} // namespace
