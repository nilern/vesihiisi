#include "compiler.h"

inline static void requireLive(Compiler* compiler, BitSet* lives, IRName name) {
    bitSetSet(&compiler->arena, lives, name.index);
}

inline static void rangeStart(BitSet* lives, IRName name) { bitSetRemove(lives, name.index); }

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

static void enlivenTransfer(Compiler* compiler, BitSet* liveOuts, IRTransfer const* transfer) {
    switch (transfer->type) {
    case TRANSFER_CALL: {
        for (size_t i = transfer->call.args.count; i-- > 0;) {
            requireLive(compiler, liveOuts, transfer->call.args.names[i]);
        }
        requireLive(compiler, liveOuts, transfer->call.callee);
    }; break;

    case TRANSFER_TAILCALL: {
        for (size_t i = transfer->tailcall.args.count; i-- > 0;) {
            requireLive(compiler, liveOuts, transfer->tailcall.args.names[i]);
        }
        requireLive(compiler, liveOuts, transfer->tailcall.retFrame);
        requireLive(compiler, liveOuts, transfer->tailcall.callee);
    }; break;

    case TRANSFER_IF: {
        requireLive(compiler, liveOuts, transfer->iff.cond);
    }; break;

    case TRANSFER_GOTO: {
        for (size_t i = transfer->gotoo.args.count; i-- > 0;) {
            requireLive(compiler, liveOuts, transfer->gotoo.args.names[i]);
        }
    }; break;

    case TRANSFER_RETURN: {
        requireLive(compiler, liveOuts, transfer->ret.arg);
        requireLive(compiler, liveOuts, transfer->ret.callee);
    }; break;
    }
}

static void enlivenFn(Compiler* compiler, IRFn* fn);

static void enlivenStmt(Compiler* compiler, BitSet* liveOuts, IRStmt* stmt) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: {
        requireLive(compiler, liveOuts, stmt->globalDef.val);
    }; break;

    case STMT_GLOBAL: {
        rangeStart(liveOuts, stmt->global.tmpName);
    }; break;

    case STMT_CONST_DEF: {
        rangeStart(liveOuts, stmt->constDef.name);
    }; break;

    case STMT_CLOVER: assert(false); break; // Should not exist yet

    case STMT_METHOD_DEF: {
        rangeStart(liveOuts, stmt->methodDef.name);

        IRFn* const innerFn = &stmt->methodDef.fn;
        enlivenFn(compiler, innerFn);
        bitSetUnionInto(&compiler->arena, liveOuts, fnFreeVars(innerFn));

        for (size_t i = innerFn->domain.count; i-- > 0;) {
            IRName const type = innerFn->domain.vals[i];
            if (irNameIsValid(type)) {
                requireLive(compiler, liveOuts, type);
            }
        }
    }; break;

    case STMT_CLOSURE: case STMT_MOVE: case STMT_SWAP: assert(false); break; // Should not exist yet
    }
}

static void enlivenParams(BitSet* liveOuts, IRBlock const* block) {
    for (size_t i = block->paramCount; i-- > 0;) {
        rangeStart(liveOuts, block->params[i]);
    }
}

static void enlivenBlock(Compiler* compiler, IRFn const* fn, IRBlock* block) {
    BitSet* lives = &block->liveIns;
    transferLiveOutsInto(compiler, fn, lives, &block->transfer);

    enlivenTransfer(compiler, lives, &block->transfer);

    for (size_t i = block->stmts.count; i-- > 0;) {
        enlivenStmt(compiler, lives, &block->stmts.vals[i]);
    }

    enlivenParams(lives, block);
}

static void enlivenFn(Compiler* compiler, IRFn* fn) {
    for (size_t i = fn->blockCount; i-- > 0;) {
        enlivenBlock(compiler, fn, fn->blocks[i]);
    }
}
