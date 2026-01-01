#include "liveness.h"

/// Add `name` to `lives` (at a use).
inline static void requireLive(Compiler* compiler, BitSet* lives, IRName name) {
    bitSetSet(&compiler->arena, lives, name.index);
}

/// Remove `name` from `lives` (at its def).
inline static void rangeStart(BitSet* lives, IRName name) { bitSetRemove(lives, name.index); }

/// Add transfer live-outs (= live-ins of successors) into `lives`.
static void transferLiveOutsInto(
    Compiler* compiler, IRFn const* fn, BitSet* lives, IRTransfer const* transfer
) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        IRBlock const* retBlock = irLabelBlock(fn, transfer->call.retLabel);
        bitSetUnionInto(&compiler->arena, lives, &retBlock->liveIns);
    }; break;

    case TRANSFER_TAILCALL: break;

    case TRANSFER_IF: {
        IRBlock const* alt = irLabelBlock(fn, transfer->iff.alt);
        bitSetUnionInto(&compiler->arena, lives, &alt->liveIns);
        IRBlock const* conseq = irLabelBlock(fn, transfer->iff.conseq);
        bitSetUnionInto(&compiler->arena, lives, &conseq->liveIns);
    }; break;

    case TRANSFER_GOTO: {
        IRBlock const* dest = irLabelBlock(fn, transfer->gotoo.dest);
        bitSetUnionInto(&compiler->arena, lives, &dest->liveIns);
    }; break;

    case TRANSFER_RETURN: break;
    }
}

// Compute transfer liveness (enliven uses in reverse).
static void enlivenTransfer(Compiler* compiler, BitSet* liveOuts, IRTransfer const* transfer) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        Call const* const call = &transfer->call;
        for (size_t i = call->args.count; i-- > 0;) {
            requireLive(compiler, liveOuts, call->args.names[i]);
        }
        requireLive(compiler, liveOuts, call->callee);
    }; break;

    case TRANSFER_TAILCALL: {
        Tailcall const* const tailcall = &transfer->tailcall;
        for (size_t i = tailcall->args.count; i-- > 0;) {
            requireLive(compiler, liveOuts, tailcall->args.names[i]);
        }
        requireLive(compiler, liveOuts, tailcall->retFrame);
        requireLive(compiler, liveOuts, tailcall->callee);
    }; break;

    case TRANSFER_IF: {
        IRIf const* const iff = &transfer->iff;
        requireLive(compiler, liveOuts, iff->cond);
    }; break;

    case TRANSFER_GOTO: {
        IRGoto const* const gotoo = &transfer->gotoo;
        for (size_t i = gotoo->args.count; i-- > 0;) {
            requireLive(compiler, liveOuts, gotoo->args.names[i]);
        }
    }; break;

    case TRANSFER_RETURN: {
        IRReturn const* const ret = &transfer->ret;
        requireLive(compiler, liveOuts, ret->arg);
        requireLive(compiler, liveOuts, ret->callee);
    }; break;
    }
}

// Compute statement liveness (kill def, then enliven uses in reverse).
static void enlivenStmt(Compiler* compiler, BitSet* liveOuts, IRStmt* stmt) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: {
        GlobalDef const* const globalDef = &stmt->globalDef;
        requireLive(compiler, liveOuts, globalDef->val);
    }; break;

    case STMT_GLOBAL: {
        IRGlobal const* const global = &stmt->global;
        rangeStart(liveOuts, global->tmpName);
    }; break;

    case STMT_CONST_DEF: {
        ConstDef const* const constDef = &stmt->constDef;
        rangeStart(liveOuts, constDef->name);
    }; break;

    case STMT_CLOVER: assert(false); break; // Should not exist yet

    case STMT_METHOD_DEF: {
        MethodDef* const methodDef = &stmt->methodDef;

        rangeStart(liveOuts, methodDef->name);

        // Clovers:
        IRFn* const innerFn = &methodDef->fn;
        enlivenFn(compiler, innerFn);
        bitSetUnionInto(&compiler->arena, liveOuts, fnFreeVars(innerFn));

        // Param types:
        for (size_t i = innerFn->domain.count; i-- > 0;) {
            IRName const type = innerFn->domain.vals[i];
            if (irNameIsValid(type)) {
                requireLive(compiler, liveOuts, type);
            }
        }
    }; break;

    case STMT_CLOSURE: case STMT_MOVE: case STMT_SWAP: assert(false); break; // Should not exist yet

    case STMT_KNOT: {
        KnotStmt const* const knot = &stmt->knot;
        rangeStart(liveOuts, knot->name);
    }; break;

    case STMT_KNOT_INIT: {
        KnotInitStmt const* const knotInit = &stmt->knotInit;
        requireLive(compiler, liveOuts, knotInit->v);
        requireLive(compiler, liveOuts, knotInit->knot);
    }; break;

    case STMT_KNOT_GET: {
        KnotGetStmt const* const knotGet = &stmt->knotGet;
        rangeStart(liveOuts, knotGet->name);
        requireLive(compiler, liveOuts, knotGet->knot);
    }
    }
}

/// Compute block liveness, backwards (first transfer, then statements in reverse, then params).
static void enlivenBlock(Compiler* compiler, IRFn const* fn, IRBlock* block) {
    BitSet* lives = &block->liveIns;
    transferLiveOutsInto(compiler, fn, lives, &block->transfer);

    enlivenTransfer(compiler, lives, &block->transfer);

    for (size_t i = block->stmts.count; i-- > 0;) {
        enlivenStmt(compiler, lives, &block->stmts.vals[i]);
    }

    for (size_t i = block->paramCount; i-- > 0;) {
        rangeStart(lives, block->params[i]);
    }
}

static void enlivenFn(Compiler* compiler, IRFn* fn) {
    for (size_t i = fn->blockCount; i-- > 0;) {
        enlivenBlock(compiler, fn, fn->blocks[i]);
    }
}
