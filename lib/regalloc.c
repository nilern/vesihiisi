#include "regalloc.h"

#include <string.h>

#include "util.h"
#include "compiler.h"
#include "state.h"
#include "bytecode.h"

// OPTIMIZE: At this point bitsets are slow because we are usually iterating over them.

inline static bool regEq(Reg reg1, Reg reg2) { return reg1.index == reg2.index; }

typedef struct MaybeReg {
    Reg val;
    bool hasVal;
} MaybeReg;

typedef struct RegEnv {
    MaybeReg* varRegs;
    IRName* regVars;
    size_t maxVarCount; // `.index` of max allocated register + 1
    size_t varCap;
} RegEnv;

static void freeRegEnv(RegEnv* env) {
    free(env->varRegs);
    free(env->regVars);
}

static RegEnv newRegEnv(Compiler const* compiler) {
    size_t const varCap = compiler->nameCount;
    MaybeReg* const varRegs = calloc(varCap, sizeof *varRegs);
    IRName* const regVars = calloc(REG_COUNT, sizeof *regVars);
    return (RegEnv){.varRegs = varRegs, .regVars = regVars, .maxVarCount = 0, .varCap = varCap};
}

static RegEnv cloneRegEnv(RegEnv const* env) {
    size_t const varCap = env->varCap;
    MaybeReg* const varRegs = malloc(varCap * sizeof *varRegs);
    memcpy(varRegs, env->varRegs, varCap * sizeof *varRegs); // OPTIMIZE

    IRName* const regVars = malloc(REG_COUNT * sizeof *regVars);
    memcpy(regVars, env->regVars, REG_COUNT * sizeof *regVars);

    return (RegEnv){
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

static void freeSavedRegEnvs(SavedRegEnvs* savedEnvs) {
    size_t const count = savedEnvs->count;
    for (size_t i = 0; i < count; ++i) {
        MaybeRegEnv* const maybeEnv = &savedEnvs->vals[i];
        if (maybeEnv->hasVal) { freeRegEnv(&maybeEnv->val); }
    }

    free(savedEnvs->vals);
}

static SavedRegEnvs newSavedRegEnvs(size_t blockCount) {
    MaybeRegEnv* const vals = calloc(blockCount, sizeof *vals);
    return (SavedRegEnvs){.vals = vals, .count = blockCount};
}

static void saveRegEnv(SavedRegEnvs* savedEnvs, IRLabel label, RegEnv env) {
    assert(label.blockIndex < savedEnvs->count);

    savedEnvs->vals[label.blockIndex] = (MaybeRegEnv){.val = env, .hasVal = true};
}

inline static bool isRegFree(RegEnv const* env, Reg reg) {
    return irNameEq(env->regVars[reg.index], invalidIRName);
}

inline static void ensureRegEnvMaxCount(RegEnv* env, size_t count) {
    if (env->maxVarCount < count) { env->maxVarCount = count; }
}

static void shrinkRegEnvMaxVarCount(RegEnv* env) {
    size_t max = env->maxVarCount;
    for (; max > 0 && isRegFree(env, (Reg){(uint8_t)(max - 1)}); --max) {}
    env->maxVarCount = max;
}

inline static MaybeReg tryVarReg(RegEnv const* env, IRName var) { return env->varRegs[var.index]; }

static void regEnvAdd(RegEnv* env, IRName var, Reg reg) {
    assert(!env->varRegs[var.index].hasVal);
    assert(irNameEq(env->regVars[reg.index], invalidIRName));

    env->varRegs[var.index] = (MaybeReg){.val = reg, .hasVal = true};
    env->regVars[reg.index] = var;
    ensureRegEnvMaxCount(env, reg.index + 1);
}

static Reg allocVarReg(RegEnv* env, IRName var) {
    assert(!env->varRegs[var.index].hasVal);

    for (size_t i = 0; i < REG_COUNT; ++i) {
        Reg const reg = (Reg){(uint8_t)i};

        // Will be true when `i == env->maxVarCount` at the latest:
        if (isRegFree(env, reg)) {
            env->varRegs[var.index] = (MaybeReg){.val = reg, .hasVal = true};
            env->regVars[i] = var;
            ensureRegEnvMaxCount(env, i + 1);
            return reg;
        }
    }

    exit(EXIT_FAILURE); // FIXME: Unlikely to happen, but still should fail just compile instead
}

typedef struct MaybeMove {
    Reg dest;
    Reg src;
    bool hasVal;
} MaybeMove;

static MaybeMove allocTransferArgReg(RegEnv* env, IRName var, Reg reg) {
    assert(isRegFree(env, reg));

    MaybeReg const maybeDest = env->varRegs[var.index];
    env->varRegs[var.index] = (MaybeReg){.val = reg, .hasVal = true};
    env->regVars[reg.index] = var;
    ensureRegEnvMaxCount(env, reg.index + 1);

    return maybeDest.hasVal
        ? (MaybeMove){.dest = maybeDest.val, .src = reg, .hasVal = true}
        : (MaybeMove){};
}

static Reg deallocVarReg(RegEnv* env, IRName var) {
    assert(env->varRegs[var.index].hasVal);
    Reg const reg = env->varRegs[var.index].val;
    env->varRegs[var.index] = (MaybeReg){};
    env->regVars[reg.index] = invalidIRName;
    shrinkRegEnvMaxVarCount(env);

    return reg;
}

static Reg getVarReg(RegEnv* env, IRName var) {
    MaybeReg const maybeReg = tryVarReg(env, var);
    if (maybeReg.hasVal) { return maybeReg.val; }

    return allocVarReg(env, var);
}

static void regEnvParamToArg(RegEnv* env, Reg paramReg, IRName arg) {
    assert(!irNameEq(env->regVars[paramReg.index], invalidIRName));
    assert(!irNameEq(arg, invalidIRName));

    IRName const param = env->regVars[paramReg.index];
    env->varRegs[param.index] = (MaybeReg){};
    env->varRegs[arg.index] = (MaybeReg){.val = paramReg, .hasVal = true};
    env->regVars[paramReg.index] = arg;
}

static void regEnvMove(RegEnv* env, IRName var, Reg src, Reg dest) {
    assert(irNameEq(env->regVars[src.index], var));
    assert(irNameEq(env->regVars[dest.index], invalidIRName));

    env->varRegs[var.index] = (MaybeReg){.val = dest, .hasVal = true};
    env->regVars[dest.index] = var;
    env->regVars[src.index] = invalidIRName;
    shrinkRegEnvMaxVarCount(env);
}

static void regEnvSwap(RegEnv* env, IRName var1, Reg reg1, IRName var2, Reg reg2) {
    assert(irNameEq(env->regVars[reg1.index], var1));
    assert(irNameEq(env->regVars[reg2.index], var2));

    env->varRegs[var1.index] = (MaybeReg){.val = reg2, .hasVal = true};
    env->regVars[reg2.index] = var1;
    env->varRegs[var2.index] = (MaybeReg){.val = reg1, .hasVal = true};
    env->regVars[reg1.index] = var2;
}

static void shuffleRegs(Compiler* compiler, RegEnv* current, RegEnv const* goal, IRBlock* block) {
    // Reversing the block retargets `pushIRStmt` to the start of the block:
    reverse(block->stmts.vals, block->stmts.count, sizeof *block->stmts.vals, swapStmts);

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
                    pushIRStmt(compiler, &block->stmts, moveToStmt((MoveStmt){
                        .dest = (IRName){reg.index},
                        .src = (IRName){goalReg.index}
                    }));

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
            pushIRStmt(compiler, &block->stmts, swapToStmt((SwapStmt){
                .reg1 = (IRName){reg.index},
                .reg2 = (IRName){goalReg.index}
            }));

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
                pushIRStmt(compiler, &block->stmts, swapToStmt((SwapStmt){
                    .reg1 = (IRName){takerReg.index},
                    .reg2 = (IRName){traderReg.index}
                }));

                traderReg = takerReg;
            }
        }
    }

    // Undo the block reversal to restore the correct statement order:
    reverse(block->stmts.vals, block->stmts.count, sizeof *block->stmts.vals, swapStmts);
}

static RegEnv regAllocIfSuccession(
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
    shuffleRegs(compiler, altEnv, &goal, altBlock);

    return goal;
}

static void regAllocBlock(
    Compiler* compiler, SavedRegEnvs* savedEnvs, BitSet* visited, IRFn* fn, IRBlock* block
);

static IRName regAllocCallee(Compiler* compiler, RegEnv* env, IRBlock* block, IRName callee) {
    Reg const reg = (Reg){calleeReg};

    MaybeMove const maybeCalleeMove = allocTransferArgReg(env, callee, reg);
    if (maybeCalleeMove.hasVal) {
        pushIRStmt(compiler, &block->stmts, moveToStmt((MoveStmt){
            .dest = (IRName){maybeCalleeMove.dest.index},
            .src = (IRName){maybeCalleeMove.src.index}
        }));
    }

    return (IRName){reg.index};
}

static void regAllocArgs(Compiler* compiler, RegEnv* env, IRBlock* block, Args* args) {
    size_t const arity = args->count;
    for (size_t i = 0; i < arity; ++i) {
        Reg const reg = (Reg){(uint8_t)(2 + i)};

        MaybeMove const maybeMove = allocTransferArgReg(env, args->names[i], reg);
        if (maybeMove.hasVal) {
            pushIRStmt(compiler, &block->stmts, moveToStmt((MoveStmt){
                .dest = (IRName){maybeMove.dest.index},
                .src = (IRName){maybeMove.src.index}
            }));
        }

        args->names[i] = (IRName){reg.index};
    }
}

static RegEnv regAllocTransfer(
    Compiler* compiler, SavedRegEnvs* savedEnvs, BitSet* visited, IRFn* fn, IRBlock* block,
    IRTransfer* transfer
) {
    // Transfers pass arguments "in parallel" so we can go forwards. This generates any moves in
    // order of lower to higher destination register.

    switch (transfer->type) {
    case TRANSFER_CALL: {
        IRLabel const retLabel = transfer->call.retLabel;
        assert(retLabel.blockIndex < fn->blockCount);
        IRBlock* const retBlock = fn->blocks[retLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, retBlock);

        RegEnv env = newRegEnv(compiler);

        transfer->call.callee = regAllocCallee(compiler, &env, block, transfer->call.callee);

        regAllocArgs(compiler, &env, block, &transfer->call.args);

        // FIXME: Clovers should be in register order:
        for (size_t i = transfer->call.closes.count; i-- > 0;) {
            transfer->call.closes.names[i] =
                (IRName){getVarReg(&env, transfer->call.closes.names[i]).index};
        }

        return env;
    }; break;

    case TRANSFER_TAILCALL: {
        RegEnv env = newRegEnv(compiler);

        transfer->tailcall.callee =
            regAllocCallee(compiler, &env, block, transfer->tailcall.callee);

        Reg const contReg = (Reg){retContReg};
        [[maybe_unused]] MaybeMove const maybeContMove =
            allocTransferArgReg(&env, transfer->tailcall.retFrame, contReg);
        assert(!maybeContMove.hasVal);
        transfer->tailcall.retFrame = (IRName){contReg.index};

        regAllocArgs(compiler, &env, block, &transfer->tailcall.args);

        return env;
    }; break;

    case TRANSFER_IF: {
        IRLabel const conseqLabel = transfer->iff.conseq;
        assert(conseqLabel.blockIndex < fn->blockCount);
        IRBlock* conseqBlock = fn->blocks[conseqLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, conseqBlock);
        IRLabel const altLabel = transfer->iff.alt;
        assert(altLabel.blockIndex < fn->blockCount);
        IRBlock* altBlock = fn->blocks[altLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, altBlock);

        RegEnv env = regAllocIfSuccession(compiler, savedEnvs, fn, transfer->iff.conseq, altLabel);

        transfer->iff.cond = (IRName){getVarReg(&env, transfer->iff.cond).index};

        return env;
    }; break;

    case TRANSFER_GOTO: {
        IRLabel const destLabel = transfer->gotoo.dest;
        assert(destLabel.blockIndex < fn->blockCount);
        IRBlock* const destBlock = fn->blocks[destLabel.blockIndex];
        regAllocBlock(compiler, savedEnvs, visited, fn, destBlock);

        assert(savedEnvs->vals[destLabel.blockIndex].hasVal);
        RegEnv env = cloneRegEnv(&savedEnvs->vals[destLabel.blockIndex].val);

        size_t const arity = transfer->gotoo.args.count;
        assert(destBlock->paramCount == arity);
        for (size_t i = 0; i < arity; ++i) {
            Reg const paramReg = (Reg){(uint8_t)destBlock->params[i].index};
            IRName* const arg = &transfer->gotoo.args.names[i];

            regEnvParamToArg(&env, paramReg, *arg);
            *arg = (IRName){paramReg.index};
        }

        return env;
    }; break;

    case TRANSFER_RETURN: {
        RegEnv env = newRegEnv(compiler);

        Reg const calleeReg = (Reg){retContReg};
        [[maybe_unused]] MaybeMove const maybeContMove =
            allocTransferArgReg(&env, transfer->ret.callee, calleeReg);
        assert(!maybeContMove.hasVal);
        transfer->ret.callee = (IRName){calleeReg.index};

        Reg const valReg = (Reg){retReg};
        [[maybe_unused]] MaybeMove const maybeValMove =
            allocTransferArgReg(&env, transfer->ret.arg, valReg);
        assert(!maybeValMove.hasVal);
        transfer->ret.arg = (IRName){valReg.index};

        return env;
    }; break;

    default: exit(EXIT_FAILURE); // Unreachable
    }
}

static void regAllocFn(Compiler* compiler, IRFn* fn);

static void regAllocStmt(Compiler* compiler, RegEnv* env, IRStmt* stmt) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: {
        stmt->globalDef.val = (IRName){getVarReg(env, stmt->globalDef.val).index};
    }; break;

    case STMT_GLOBAL: {
        stmt->global.tmpName = (IRName){deallocVarReg(env, stmt->global.tmpName).index};
    }; break;

    case STMT_CONST_DEF: {
        stmt->constDef.name = (IRName){deallocVarReg(env, stmt->constDef.name).index};
    }; break;

    // FIXME: Clovers should be in register order:
    case STMT_CLOVER: {
        stmt->clover.name = (IRName){deallocVarReg(env, stmt->clover.name).index};
        stmt->clover.closure = (IRName){getVarReg(env, stmt->clover.closure).index};
    }; break;

    case STMT_FN_DEF: {
        stmt->fnDef.name = (IRName){deallocVarReg(env, stmt->fnDef.name).index};

        // FIXME: Clovers should be in register order:
        for (size_t i = stmt->fnDef.closes.count; i-- > 0;) {
            stmt->fnDef.closes.names[i] =
                (IRName){getVarReg(env, stmt->fnDef.closes.names[i]).index};
        }

        regAllocFn(compiler, &stmt->fnDef.fn);
    }; break;

    case STMT_MOVE: case STMT_SWAP: break; // Generated during this pass => no-op
    }
}

static void regAllocParams(Compiler* compiler, RegEnv* env, IRBlock* block) {
    if (block->callers.count == 0) {
        RegEnv goal = newRegEnv(compiler);

        size_t paramIdx = 0;

        if (block->label.blockIndex == 0) { // Call entry block
            IRName* const callee = &block->params[0];
            regEnvAdd(&goal, *callee, (Reg){calleeReg});
            *callee = (IRName){calleeReg};

            IRName* const retCont = &block->params[1];
            regEnvAdd(&goal, *retCont, (Reg){retContReg});
            *retCont = (IRName){retContReg};

            paramIdx = 2;
        } else { // Return entry block
            IRName* const retCont = &block->params[0];
            regEnvAdd(&goal, *retCont, (Reg){retContReg});
            *retCont = (IRName){retContReg};

            paramIdx = 1;
        }

        size_t const arity = block->paramCount;
        for (size_t regIdx = 2; paramIdx < arity; ++paramIdx, ++regIdx) {
            IRName* const param = &block->params[paramIdx];
            Reg const reg = {(uint8_t)regIdx};
            regEnvAdd(&goal, *param, reg);
            *param = (IRName){regIdx};
        }

        shuffleRegs(compiler, env, &goal, block);

        freeRegEnv(&goal);
    } else {
        size_t const arity = block->paramCount;
        for (size_t i = 0; i < arity; ++i) {
            IRName* const param = &block->params[i];
            assert(env->varRegs[param->index].hasVal);
            Reg const reg = env->varRegs[param->index].val;
            *param = (IRName){reg.index};
        }
    }
}

static void regAllocBlock(
    Compiler* compiler, SavedRegEnvs* savedEnvs, BitSet* visited, IRFn* fn, IRBlock* block
) {
    IRLabel const label = block->label;
    if (bitSetContains(visited, label.blockIndex)) { return; }
    bitSetSet(&compiler->arena, visited, label.blockIndex);

    RegEnv env = regAllocTransfer(compiler, savedEnvs, visited, fn, block, &block->transfer);

    for (size_t i = block->stmts.count; i-- > 0;) {
        regAllocStmt(compiler, &env, &block->stmts.vals[i]);
    }

    regAllocParams(compiler, &env, block);

    if (block->callers.count == 0) {
        freeRegEnv(&env);
    } else {
        saveRegEnv(savedEnvs, label, env);
    }
}

static void regAllocFn(Compiler* compiler, IRFn* fn) {
    assert(fn->blockCount > 0);

    size_t const blockCount = fn->blockCount;
    SavedRegEnvs savedEnvs = newSavedRegEnvs(blockCount);
    BitSet visited = createBitSet(&compiler->arena, blockCount);

    regAllocBlock(compiler, &savedEnvs, &visited, fn, fn->blocks[0]);

    freeSavedRegEnvs(&savedEnvs);
}
