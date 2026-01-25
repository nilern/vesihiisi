#include "regalloc.hpp"

#include <string.h>

#include "../util/util.hpp"
#include "../state.hpp"
#include "../bytecode.hpp"

namespace {

// OPTIMIZE: At this point bitsets are slow because we are usually iterating over them.

inline bool regEq(Reg reg1, Reg reg2) { return reg1.index == reg2.index; }

typedef struct MaybeReg {
    Reg val;
    bool hasVal;
} MaybeReg;

// Bidirectional Variable-Register Mappings
// =================================================================================================

typedef struct RegEnv {
    MaybeReg* varRegs;
    IRName* regVars;
    size_t maxVarCount; // `.index` of max allocated register + 1
    size_t varCap;
} RegEnv;

void freeRegEnv(RegEnv* env) {
    free(env->varRegs);
    free(env->regVars);
}

RegEnv newRegEnv(Compiler const* compiler) {
    size_t const varCap = compiler->nameSyms.count();
    MaybeReg* const varRegs = (MaybeReg*)calloc(varCap, sizeof *varRegs);
    IRName* const regVars = (IRName*)calloc(REG_COUNT, sizeof *regVars);
    return RegEnv{.varRegs = varRegs, .regVars = regVars, .maxVarCount = 0, .varCap = varCap};
}

RegEnv cloneRegEnv(RegEnv const* env) {
    size_t const varCap = env->varCap;
    MaybeReg* const varRegs = (MaybeReg*)malloc(varCap * sizeof *varRegs);
    memcpy(varRegs, env->varRegs, varCap * sizeof *varRegs); // OPTIMIZE

    IRName* const regVars = (IRName*)malloc(REG_COUNT * sizeof *regVars);
    memcpy(regVars, env->regVars, REG_COUNT * sizeof *regVars);

    return RegEnv{
        .varRegs = varRegs,
        .regVars = regVars,
        .maxVarCount = env->maxVarCount,
        .varCap = varCap
    };
}

typedef struct MaybeRegEnv {
    RegEnv val;
    bool hasVal;
} MaybeRegEnv;

typedef struct SavedRegEnvs {
    MaybeRegEnv* vals;
    size_t count;
} SavedRegEnvs;

void freeSavedRegEnvs(SavedRegEnvs* savedEnvs) {
    size_t const count = savedEnvs->count;
    for (size_t i = 0; i < count; ++i) {
        MaybeRegEnv* const maybeEnv = &savedEnvs->vals[i];
        if (maybeEnv->hasVal) { freeRegEnv(&maybeEnv->val); }
    }

    free(savedEnvs->vals);
}

SavedRegEnvs newSavedRegEnvs(size_t blockCount) {
    MaybeRegEnv* const vals = (MaybeRegEnv*)calloc(blockCount, sizeof *vals);
    return SavedRegEnvs{.vals = vals, .count = blockCount};
}

void saveRegEnv(SavedRegEnvs* savedEnvs, IRLabel label, RegEnv env) {
    assert(label.blockIndex < savedEnvs->count);

    savedEnvs->vals[label.blockIndex] = MaybeRegEnv{.val = env, .hasVal = true};
}

inline bool isRegFree(RegEnv const* env, Reg reg) {
    return irNameEq(env->regVars[reg.index], invalidIRName);
}

inline void ensureRegEnvMaxCount(RegEnv* env, size_t count) {
    if (env->maxVarCount < count) { env->maxVarCount = count; }
}

void shrinkRegEnvMaxVarCount(RegEnv* env) {
    size_t max = env->maxVarCount;
    for (; max > 0 && isRegFree(env, Reg{(uint8_t)(max - 1)}); --max) {}
    env->maxVarCount = max;
}

inline MaybeReg tryVarReg(RegEnv const* env, IRName var) { return env->varRegs[var.index]; }

void regEnvAdd(RegEnv* env, IRName var, Reg reg) {
    assert(!env->varRegs[var.index].hasVal);
    assert(irNameEq(env->regVars[reg.index], invalidIRName));

    env->varRegs[var.index] = MaybeReg{.val = reg, .hasVal = true};
    env->regVars[reg.index] = var;
    ensureRegEnvMaxCount(env, reg.index + 1);
}

Reg allocVarReg(RegEnv* env, IRName var) {
    for (size_t i = 0; i < REG_COUNT; ++i) {
        Reg const reg = Reg{(uint8_t)i};

        // Will be true when `i == env->maxVarCount` at the latest:
        if (isRegFree(env, reg)) {
            env->varRegs[var.index] = MaybeReg{.val = reg, .hasVal = true};
            env->regVars[i] = var;
            ensureRegEnvMaxCount(env, i + 1);
            return reg;
        }
    }

    exit(EXIT_FAILURE); // FIXME: Unlikely to happen, but still should fail just compile instead
}

struct Move {
    Reg dest;
    Reg src;
};

struct AllocStmtArgRegRes {
    Reg reg;
    Maybe<Move> maybeMove;
};

[[nodiscard]]
AllocStmtArgRegRes allocStmtArgReg(RegEnv* env, IRName var) {
    MaybeReg const maybeDest = env->varRegs[var.index];

    Reg const reg = allocVarReg(env, var);

    return AllocStmtArgRegRes{
        reg,
        maybeDest.hasVal ? Maybe{Move{.dest = maybeDest.val, .src = reg}} : Maybe<Move>{}
    };
}

[[nodiscard]]
Maybe<Move> allocTransferArgReg(RegEnv* env, IRName var, Reg reg, bool delayDupDeallocs) {
    assert(isRegFree(env, reg));

    MaybeReg const maybeDest = env->varRegs[var.index];
    if (!delayDupDeallocs && maybeDest.hasVal) {
        env->regVars[maybeDest.val.index] = invalidIRName;
        // No need to `shrinkRegEnvMaxVarCount` since it will get grown to `reg.index + 1`:
        assert(maybeDest.val.index < reg.index);
    }

    env->varRegs[var.index] = MaybeReg{.val = reg, .hasVal = true};
    env->regVars[reg.index] = var;
    ensureRegEnvMaxCount(env, reg.index + 1);

    return maybeDest.hasVal
        ? Maybe{Move{.dest = maybeDest.val, .src = reg}}
        : Maybe<Move>{};
}

void delayedDeallocTransferArgRegs(RegEnv* env, Slice<Move const> moves) {
    size_t const moveCount = moves.count;
    for (size_t i = 0; i < moveCount; ++i) {
        Reg const dest = moves[i].dest;
        assert(!isRegFree(env, dest));

        env->regVars[dest.index] = invalidIRName;
    }

    shrinkRegEnvMaxVarCount(env);
}

Reg deallocVarReg(RegEnv* env, IRName var) {
    if (!env->varRegs[var.index].hasVal) {
        allocVarReg(env, var); // OPTIMIZE: Will be immediately deallocated:
    }

    Reg const reg = env->varRegs[var.index].val;
    env->varRegs[var.index] = MaybeReg{};
    env->regVars[reg.index] = invalidIRName;
    shrinkRegEnvMaxVarCount(env);

    return reg;
}

void deallocDupReg(RegEnv* env, Reg reg) {
    env->regVars[reg.index] = invalidIRName;
    shrinkRegEnvMaxVarCount(env);
}

Reg getVarReg(RegEnv* env, IRName var) {
    MaybeReg const maybeReg = tryVarReg(env, var);
    if (maybeReg.hasVal) { return maybeReg.val; }

    return allocVarReg(env, var);
}

[[nodiscard]]
Maybe<Move> regEnvParamToArg(RegEnv* env, Reg paramReg, IRName arg) {
    assert(!irNameEq(env->regVars[paramReg.index], invalidIRName));
    assert(!irNameEq(arg, invalidIRName));

    MaybeReg const maybeDest = env->varRegs[arg.index];
    if (maybeDest.hasVal) {
        env->regVars[maybeDest.val.index] = invalidIRName;
        shrinkRegEnvMaxVarCount(env);
    }

    IRName const param = env->regVars[paramReg.index];
    env->varRegs[param.index] = MaybeReg{};
    env->varRegs[arg.index] = MaybeReg{.val = paramReg, .hasVal = true};
    env->regVars[paramReg.index] = arg;

    return maybeDest.hasVal
               ? Maybe{Move{.dest = maybeDest.val, .src = paramReg}}
               : Maybe<Move>{};
}

void regEnvMove(RegEnv* env, IRName var, Reg src, Reg dest) {
    assert(irNameEq(env->regVars[src.index], var));
    assert(irNameEq(env->regVars[dest.index], invalidIRName));

    env->varRegs[var.index] = MaybeReg{.val = dest, .hasVal = true};
    env->regVars[dest.index] = var;
    env->regVars[src.index] = invalidIRName;
    shrinkRegEnvMaxVarCount(env);
}

void regEnvSwap(RegEnv* env, IRName var1, Reg reg1, IRName var2, Reg reg2) {
    assert(irNameEq(env->regVars[reg1.index], var1));
    assert(irNameEq(env->regVars[reg2.index], var2));

    env->varRegs[var1.index] = MaybeReg{.val = reg2, .hasVal = true};
    env->regVars[reg2.index] = var1;
    env->varRegs[var2.index] = MaybeReg{.val = reg1, .hasVal = true};
    env->regVars[reg1.index] = var2;
}

// Register Allocation Over IR
// =================================================================================================

void shuffleRegs(
    Compiler* compiler, RegEnv* current, Stmts* outputStmts, RegEnv const* goal, ORef const maybeLoc
) {
    // Iterate until no line ends = `mov` all lines away:
    for (bool foundLineEnd = true; foundLineEnd;) {
        foundLineEnd = false;

        size_t const maxVarCount = current->maxVarCount;
        for (size_t i = 0; i < maxVarCount; ++i) {
            Reg const reg = {(uint8_t)i};
            IRName const var = current->regVars[i];
            if (irNameEq(var, invalidIRName)) { continue; } // Loop artefact: `reg` is free

            MaybeReg const maybeGoalReg = goal->varRegs[var.index];
            assert(maybeGoalReg.hasVal);
            Reg const goalReg = maybeGoalReg.val;
            if (!regEq(reg, goalReg)) { // Needs move or swap
                if (isRegFree(current, goalReg)) { // Can move now
                    regEnvMove(current, var, reg, goalReg);
                    pushIRStmt(compiler, outputStmts, moveToStmt(MoveStmt{
                        .dest = IRName{reg.index},
                        .src = IRName{goalReg.index}
                    }, maybeLoc));

                    foundLineEnd = true;
                }
            }
        }
    }

    // Only cycles remain, handle each with a series of swaps:
    size_t const maxVarCount = current->maxVarCount;
    for (size_t i = 0; i < maxVarCount; ++i) {
        Reg const reg = {(uint8_t)i};
        IRName const var = current->regVars[i];
        if (irNameEq(var, invalidIRName)) { continue; } // Loop artefact: `reg` is free

        MaybeReg const maybeGoalReg = goal->varRegs[var.index];
        assert(maybeGoalReg.hasVal);
        Reg const goalReg = maybeGoalReg.val;
        if (!regEq(reg, goalReg)) { // Needs swap
            IRName const trader = current->regVars[goalReg.index];
            assert(!irNameEq(trader, invalidIRName));

            // Loop-breaking swap:
            regEnvSwap(current, var, reg, trader, goalReg);
            pushIRStmt(compiler, outputStmts, swapToStmt(SwapStmt{
                .reg1 = IRName{reg.index},
                .reg2 = IRName{goalReg.index}
            }, maybeLoc));

            // Cascading swaps:
            MaybeReg const maybeTraderGoalReg = goal->varRegs[trader.index];
            assert(maybeTraderGoalReg.hasVal);
            Reg const traderGoalReg = maybeTraderGoalReg.val;
            for (Reg traderReg = reg; !regEq(traderReg, traderGoalReg);) {
                IRName const taker = goal->regVars[traderReg.index];
                assert(!irNameEq(taker, invalidIRName));
                MaybeReg const maybeTakerReg = current->varRegs[taker.index];
                assert(maybeTakerReg.hasVal);
                Reg const takerReg = maybeTakerReg.val;

                regEnvSwap(current, taker, takerReg, trader, traderReg);
                pushIRStmt(compiler, outputStmts, swapToStmt(SwapStmt{
                    .reg1 = IRName{takerReg.index},
                    .reg2 = IRName{traderReg.index}
                }, maybeLoc));

                traderReg = takerReg;
            }
        }
    }
}

RegEnv regAllocIfSuccession(
    Compiler* compiler, SavedRegEnvs* savedEnvs, IRFn* fn, IRLabel conseqLabel, IRLabel altLabel
) {
    assert(savedEnvs->vals[conseqLabel.blockIndex].hasVal);
    RegEnv const* conseqEnv = &savedEnvs->vals[conseqLabel.blockIndex].val;
    assert(savedEnvs->vals[altLabel.blockIndex].hasVal);
    RegEnv* altEnv = &savedEnvs->vals[altLabel.blockIndex].val;

    RegEnv goal = cloneRegEnv(conseqEnv);

    {
        size_t const maxVarCount = altEnv->maxVarCount;
        for (size_t i = 0; i < maxVarCount; ++i) {
            IRName const var = altEnv->regVars[i];
            if (irNameEq(var, invalidIRName)) { continue; } // Loop artefact: `reg` is free

            // OPTIMIZE: Try to use the same reg as in `altEnv`:
            getVarReg(&goal, var); // Just ensure that `var` is in `goal`; discard return value
        }
    }

    assert(altLabel.blockIndex < fn->blockCount);
    IRBlock* altBlock = fn->blocks[altLabel.blockIndex];
    ORef const maybeLoc = altBlock->stmts.count > 0
        ? altBlock->stmts.vals[0].maybeLoc
        : altBlock->transfer.maybeLoc;
    reverse(altBlock->stmts.vals, altBlock->stmts.count, sizeof *altBlock->stmts.vals, swapStmts);
    shuffleRegs(compiler, altEnv, &altBlock->stmts, &goal, maybeLoc);
    reverse(altBlock->stmts.vals, altBlock->stmts.count, sizeof *altBlock->stmts.vals, swapStmts);

    return goal;
}

void regAllocBlock(
    Compiler* compiler, SavedRegEnvs* savedEnvs, BitSet* visited, IRFn* fn, IRBlock* block
);

IRName regAllocCallee(RegEnv* env, IRName callee) {
    Reg const reg = Reg{calleeReg};

    [[maybe_unused]] Maybe<Move> const maybeCalleeMove =
        allocTransferArgReg(env, callee, reg, true);
    assert(!maybeCalleeMove.hasVal); // Callees are processed in empty env so no move should result

    return IRName{reg.index};
}

[[nodiscard]]
AVec<Move> regAllocCallArgs(Compiler* compiler, RegEnv* env, Args* args) {
    auto moves = AVec<Move>{&compiler->arena};

    size_t const arity = args->count;
    for (size_t i = 0; i < arity; ++i) {
        Reg const reg = Reg{(uint8_t)(2 + i)};

        Maybe<Move> const maybeMove = allocTransferArgReg(env, args->names[i], reg, true);
        if (maybeMove.hasVal) {
            moves.push(maybeMove.val);
        }

        args->names[i] = IRName{reg.index};
    }

    return moves;
}

void regAllocTailcallArgs(
    Compiler* compiler, RegEnv* env, IRBlock* block, Args* args, ORef maybeLoc
) {
    size_t const arity = args->count;
    for (size_t i = 0; i < arity; ++i) {
        Reg const reg = Reg{(uint8_t)(2 + i)};

        Maybe<Move> const maybeMove = allocTransferArgReg(env, args->names[i], reg, false);
        if (maybeMove.hasVal) {
            pushIRStmt(compiler, &block->stmts, moveToStmt(MoveStmt{
               .dest = IRName{maybeMove.val.dest.index},
               .src = IRName{maybeMove.val.src.index}
            }, maybeLoc));
        }

        args->names[i] = IRName{reg.index};
    }
}

RegEnv regAllocTransfer(
    Compiler* compiler, SavedRegEnvs* savedEnvs, BitSet* visited, IRFn* fn, IRBlock* block,
    IRTransfer* transfer
) {
    // Transfers pass arguments "in parallel" so we can go forwards. This generates any moves in
    // order of lower to higher destination register.

    switch (transfer->type) {
    case IRTransfer::CALL: {
        Call* const call = &transfer->call;

        IRLabel const retLabel = call->retLabel;
        assert(retLabel.blockIndex < fn->blockCount);
        IRBlock* const retBlock = fn->blocks[retLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, retBlock);

        RegEnv env = newRegEnv(compiler);

        call->callee = regAllocCallee(&env, call->callee);

        AVec<Move> moves = regAllocCallArgs(compiler, &env, &call->args);

        for (size_t i = call->closes.count; i-- > 0;) {
            call->closes.names[i] = IRName{getVarReg(&env, call->closes.names[i]).index};
        }

        // Do the duplicate arg moves that were delayed to avoid clobbering spillees:
        size_t const moveCount = moves.count();
        for (size_t i = 0; i < moveCount; ++i) {
            Move const move = moves[i];

            pushIRStmt(compiler, &block->stmts, moveToStmt(MoveStmt{
               .dest = IRName{move.dest.index},
               .src = IRName{move.src.index}
            }, transfer->maybeLoc));
        }
        delayedDeallocTransferArgRegs(&env, moves.slice());

        return env;
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall* const tailcall = &transfer->tailcall;

        RegEnv env = newRegEnv(compiler);

        tailcall->callee = regAllocCallee(&env, tailcall->callee);

        Reg const contReg = Reg{retContReg};
        [[maybe_unused]] Maybe<Move> const maybeContMove =
            allocTransferArgReg(&env, tailcall->retFrame, contReg, false);
        assert(!maybeContMove.hasVal);
        tailcall->retFrame = IRName{contReg.index};

        regAllocTailcallArgs(compiler, &env, block, &tailcall->args, transfer->maybeLoc);

        return env;
    }; break;

    case IRTransfer::IF: {
        IRIf* const iff = &transfer->iff;

        IRLabel const conseqLabel = iff->conseq;
        assert(conseqLabel.blockIndex < fn->blockCount);
        IRBlock* conseqBlock = fn->blocks[conseqLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, conseqBlock);
        IRLabel const altLabel = iff->alt;
        assert(altLabel.blockIndex < fn->blockCount);
        IRBlock* altBlock = fn->blocks[altLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, altBlock);

        RegEnv env = regAllocIfSuccession(compiler, savedEnvs, fn, iff->conseq, altLabel);

        iff->cond = IRName{getVarReg(&env, iff->cond).index};

        return env;
    }; break;

    case IRTransfer::GOTO: {
        IRGoto* const gotoo = &transfer->gotoo;

        IRLabel const destLabel = gotoo->dest;
        assert(destLabel.blockIndex < fn->blockCount);
        IRBlock* const destBlock = fn->blocks[destLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, destBlock);

        assert(savedEnvs->vals[destLabel.blockIndex].hasVal);
        RegEnv env = cloneRegEnv(&savedEnvs->vals[destLabel.blockIndex].val);

        size_t const arity = gotoo->args.count;
        assert(destBlock->paramCount == arity);
        for (size_t i = 0; i < arity; ++i) {
            Reg const paramReg = Reg{(uint8_t)destBlock->params[i].index};
            IRName* const arg = &gotoo->args.names[i];

            Maybe<Move> const maybeMove = regEnvParamToArg(&env, paramReg, *arg);
            if (maybeMove.hasVal) {
                pushIRStmt(compiler, &block->stmts, moveToStmt(MoveStmt{
                    .dest = IRName{maybeMove.val.dest.index},
                    .src = IRName{maybeMove.val.src.index}
                }, transfer->maybeLoc));
            }

            *arg = IRName{paramReg.index};
        }

        return env;
    }; break;

    case IRTransfer::RETURN: {
        IRReturn* const ret = &transfer->ret;

        RegEnv env = newRegEnv(compiler);

        Reg const calleeReg = Reg{retContReg};
        [[maybe_unused]] Maybe<Move> const maybeContMove =
            allocTransferArgReg(&env, ret->callee, calleeReg, false);
        assert(!maybeContMove.hasVal);
        ret->callee = IRName{calleeReg.index};

        Reg const valReg = Reg{retReg};
        [[maybe_unused]] Maybe<Move> const maybeValMove =
            allocTransferArgReg(&env, ret->arg, valReg, false);
        assert(!maybeValMove.hasVal);
        ret->arg = IRName{valReg.index};

        return env;
    }; break;

    default: exit(EXIT_FAILURE); // Unreachable
    }
}

void regAllocStmt(Compiler* compiler, RegEnv* env, Stmts* outputStmts, IRStmt* stmt) {
    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: {
        Define* const define = &stmt->define;
        define->val = IRName{getVarReg(env, define->val).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;

    case IRStmt::GLOBAL_SET: {
        GlobalSet* const globalSet = &stmt->globalSet;
        globalSet->val = IRName{getVarReg(env, globalSet->val).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal* const global = &stmt->global;
        global->tmpName = IRName{deallocVarReg(env, global->tmpName).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;

    case IRStmt::CONST_DEF: {
        ConstDef* const constDef = &stmt->constDef;
        constDef->name = IRName{deallocVarReg(env, constDef->name).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;

    case IRStmt::CLOVER: {
        Clover* const clover = &stmt->clover;
        clover->name = IRName{deallocVarReg(env, clover->name).index};
        clover->closure = IRName{getVarReg(env, clover->closure).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;

    case IRStmt::METHOD_DEF: {
        MethodDef* const methodDef = &stmt->methodDef;
        IRFn* const fn = &methodDef->fn;

        RegEnv outEnv = cloneRegEnv(env); // After post-stmt shuffle

        // Clean out registers of live-out specializers and collect non-live-out specializers:
        auto dyerTypes = AVec<IRName>{&compiler->arena};
        for (size_t i = fn->domain.count; i-- > 0;) {
            IRName const typeName = fn->domain.vals[i];
            if (!irNameIsValid(typeName)) { continue; } // HACK

            MaybeReg const maybeTypeReg = tryVarReg(env, typeName);
            if (maybeTypeReg.hasVal) {
                deallocVarReg(env, typeName);
            } else {
                dyerTypes.push(typeName);
            }
        }

        // Target register (of `methodDef->name`) is not freed so that it does not clobber any
        // live-out specializers.

        // Allocate registers for specializers and collect duplicating moves:
        auto dupMoves = AVec<Move>{&compiler->arena};
        {
            size_t const domainCount = fn->domain.count;
            for (size_t i = 0; i < domainCount; ++i) {
                IRName const typeName = fn->domain.vals[i];
                if (!irNameIsValid(typeName)) { continue; } // HACK

                AllocStmtArgRegRes const res = allocStmtArgReg(env, typeName);
                fn->domain.vals[i] = IRName{res.reg.index};
                if (res.maybeMove.hasVal) {
                    dupMoves.push(res.maybeMove.val);
                }
            }
        }

        RegEnv goalOutEnv = cloneRegEnv(env); // Before post-stmt shuffle
        {
            // Remove move srcs so specializers do not exist in multiple registers at once:
            size_t const dupMoveCount = dupMoves.count();
            for (size_t i = 0; i < dupMoveCount; ++i) {
                deallocDupReg(&goalOutEnv, dupMoves[i].src);
            }

            // Remove specializers that are not live-outs:
            size_t const dyerTypeCount = dyerTypes.count();
            for (size_t i = 0; i < dyerTypeCount; ++i) {
                deallocVarReg(&goalOutEnv, dyerTypes[i]);
            }
        }

        // Emit post-stmt shuffle:
        shuffleRegs(compiler, &outEnv, outputStmts, &goalOutEnv, stmt->maybeLoc);
        freeRegEnv(&goalOutEnv);
        freeRegEnv(&outEnv);

        // Deallocate target register and emit stmt itself:
        methodDef->name = IRName{deallocVarReg(env, methodDef->name).index};
        regAllocFn(compiler, fn);
        pushIRStmt(compiler, outputStmts, *stmt);

        // Emit pre-stmt duplicating moves:
        {
            size_t const dupMoveCount = dupMoves.count();
            for (size_t i = 0; i < dupMoveCount; ++i) {
                Move const move = dupMoves[i];

                pushIRStmt(compiler, outputStmts, moveToStmt(MoveStmt{
                    .dest = IRName{move.dest.index},
                    .src = IRName{move.src.index}
                }, stmt->maybeLoc));
            }
            delayedDeallocTransferArgRegs(env, dupMoves.slice());
        }
    }; break;

    case IRStmt::CLOSURE: {
        IRClosure* const closure = &stmt->closure;

        closure->name = IRName{deallocVarReg(env, closure->name).index};

        for (size_t i = closure->closes->count; i-- > 0;) {
            closure->closes->names[i] = IRName{getVarReg(env, closure->closes->names[i]).index};
        }

        closure->method = IRName{getVarReg(env, closure->method).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;

    // Generated during this pass, so only copy:
    case IRStmt::MOVE: case IRStmt::SWAP: pushIRStmt(compiler, outputStmts, *stmt); break;

    case IRStmt::KNOT: {
        KnotStmt* const knot = &stmt->knot;
        knot->name = IRName{deallocVarReg(env, knot->name).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;

    case IRStmt::KNOT_INIT: {
        KnotInitStmt* const knotInit = &stmt->knotInit;
        knotInit->v = IRName{getVarReg(env, knotInit->v).index};
        knotInit->knot = IRName{getVarReg(env, knotInit->knot).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt* const knotGet = &stmt->knotGet;
        knotGet->name = IRName{deallocVarReg(env, knotGet->name).index};
        knotGet->knot = IRName{getVarReg(env, knotGet->knot).index};

        pushIRStmt(compiler, outputStmts, *stmt);
    }; break;
    }
}

void regAllocParams(Compiler* compiler, RegEnv* env, Stmts* outputStmts, IRBlock* block) {
    if (block->callers.count == 0) { // Escaping block:
        RegEnv goal = newRegEnv(compiler);

        size_t paramIdx = 0;

        if (block->label.blockIndex == 0) { // Call entry block:
            IRName* const callee = &block->params[0];
            regEnvAdd(&goal, *callee, Reg{calleeReg});
            *callee = IRName{calleeReg};

            IRName* const retCont = &block->params[1];
            regEnvAdd(&goal, *retCont, Reg{retContReg});
            *retCont = IRName{retContReg};

            paramIdx = 2;
        } else { // Return entry block:
            IRName* const retCont = &block->params[0];
            regEnvAdd(&goal, *retCont, Reg{retContReg});
            *retCont = IRName{retContReg};

            paramIdx = 1;
        }

        size_t const arity = block->paramCount;
        for (size_t regIdx = 2; paramIdx < arity; ++paramIdx, ++regIdx) {
            IRName* const param = &block->params[paramIdx];
            Reg const reg = {(uint8_t)regIdx};
            regEnvAdd(&goal, *param, reg);
            *param = IRName{regIdx};
        }

        ORef const maybeLoc = block->stmts.count > 0
            ? block->stmts.vals[0].maybeLoc
            : block->transfer.maybeLoc;
        shuffleRegs(compiler, env, outputStmts, &goal, maybeLoc);

        freeRegEnv(&goal);
    } else { // Non-escaping block:
        size_t const arity = block->paramCount;
        for (size_t i = 0; i < arity; ++i) {
            IRName* const param = &block->params[i];
            MaybeReg const maybeReg = env->varRegs[param->index];
            Reg const reg = maybeReg.hasVal ? maybeReg.val : allocVarReg(env, *param);
            *param = IRName{reg.index};
        }
    }
}

void regAllocBlock(
    Compiler* compiler, SavedRegEnvs* savedEnvs, BitSet* visited, IRFn* fn, IRBlock* block
) {
    IRLabel const label = block->label;
    if (bitSetContains(visited, label.blockIndex)) { return; }
    bitSetSet(&compiler->arena, visited, label.blockIndex);

    RegEnv env = regAllocTransfer(compiler, savedEnvs, visited, fn, block, &block->transfer);

    Stmts outputStmts = newStmtsWithCap(compiler, block->stmts.count);

    for (size_t i = block->stmts.count; i-- > 0;) {
        regAllocStmt(compiler, &env, &outputStmts, &block->stmts.vals[i]);
    }

    regAllocParams(compiler, &env, &outputStmts, block);

    reverse(outputStmts.vals, outputStmts.count, sizeof *outputStmts.vals, swapStmts);
    block->stmts = outputStmts;

    if (block->callers.count == 0) {
        freeRegEnv(&env);
    } else {
        saveRegEnv(savedEnvs, label, env);
    }
}

void regAllocFn(Compiler* compiler, IRFn* fn) {
    assert(fn->blockCount > 0);

    size_t const blockCount = fn->blockCount;
    SavedRegEnvs savedEnvs = newSavedRegEnvs(blockCount);
    BitSet visited = createBitSet(&compiler->arena, blockCount);

    regAllocBlock(compiler, &savedEnvs, &visited, fn, fn->blocks[0]);

    freeSavedRegEnvs(&savedEnvs);
}

} // namespace
