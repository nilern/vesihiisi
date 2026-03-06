#include "liveness.hpp"

namespace {

/// Add `name` to `lives` (at a use).
inline void requireLive(Compiler& compiler, BitSet& lives, IRName name) {
    bitSetSet(&compiler.arena, &lives, name.index);
}

/// Remove `name` from `lives` (at its def).
inline void rangeStart(BitSet& lives, IRName name) { bitSetRemove(&lives, name.index); }

/// Add transfer live-outs (= live-ins of successors) into `lives`.
void transferLiveOutsInto(
    Compiler& compiler, IRFn const& fn, BitSet& lives, IRTransfer const& transfer
) {
    switch (transfer.type) {
    case IRTransfer::CALL: {
        IRBlock const* retBlock = fn.labelBlock(transfer.call.retLabel);
        bitSetUnionInto(&compiler.arena, &lives, &retBlock->liveIns);
    }; break;

    case IRTransfer::TAILCALL: break;

    case IRTransfer::IF: {
        IRBlock const* alt = fn.labelBlock(transfer.iff.alt);
        bitSetUnionInto(&compiler.arena, &lives, &alt->liveIns);
        IRBlock const* conseq = fn.labelBlock(transfer.iff.conseq);
        bitSetUnionInto(&compiler.arena, &lives, &conseq->liveIns);
    }; break;

    case IRTransfer::GOTO: {
        IRBlock const* dest = fn.labelBlock(transfer.gotoo.dest);
        bitSetUnionInto(&compiler.arena, &lives, &dest->liveIns);
    }; break;

    case IRTransfer::RETURN: break;
    }
}

// Compute transfer liveness (enliven uses in reverse).
void enlivenTransfer(Compiler& compiler, BitSet& liveOuts, IRTransfer const& transfer) {
    switch (transfer.type) {
    case IRTransfer::CALL: {
        Call const& call = transfer.call;
        for (size_t i = call.args.count(); i-- > 0;) {
            requireLive(compiler, liveOuts, call.args[i]);
        }
        requireLive(compiler, liveOuts, call.callee);
    }; break;

    case IRTransfer::TAILCALL: {
        Tailcall const& tailcall = transfer.tailcall;
        for (size_t i = tailcall.args.count(); i-- > 0;) {
            requireLive(compiler, liveOuts, tailcall.args[i]);
        }
        requireLive(compiler, liveOuts, tailcall.retFrame);
        requireLive(compiler, liveOuts, tailcall.callee);
    }; break;

    case IRTransfer::IF: {
        IRIf const& iff = transfer.iff;
        requireLive(compiler, liveOuts, iff.cond);
    }; break;

    case IRTransfer::GOTO: {
        IRGoto const& gotoo = transfer.gotoo;
        for (size_t i = gotoo.args.count(); i-- > 0;) {
            requireLive(compiler, liveOuts, gotoo.args[i]);
        }
    }; break;

    case IRTransfer::RETURN: {
        IRReturn const& ret = transfer.ret;
        requireLive(compiler, liveOuts, ret.arg);
        requireLive(compiler, liveOuts, ret.callee);
    }; break;
    }
}

// Compute statement liveness (kill def, then enliven uses in reverse).
void enlivenStmt(Compiler& compiler, BitSet& liveOuts, IRStmt& stmt) {
    switch (stmt.type) {
    case IRStmt::GLOBAL_DEF: {
        Define const& define = stmt.define;
        requireLive(compiler, liveOuts, define.val);
    }; break;

    case IRStmt::GLOBAL_SET: {
        GlobalSet const& globalSet = stmt.globalSet;
        requireLive(compiler, liveOuts, globalSet.val);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal const& global = stmt.global;
        rangeStart(liveOuts, global.tmpName);
    }; break;

    case IRStmt::CONST_DEF: {
        ConstDef const& constDef = stmt.constDef;
        rangeStart(liveOuts, constDef.name);
    }; break;

    case IRStmt::CLOVER: case IRStmt::UNSPILL: assert(false); break; // Should not exist yet

    case IRStmt::METHOD_DEF: {
        MethodDef& methodDef = stmt.methodDef;

        rangeStart(liveOuts, methodDef.name);

        // Clovers:
        IRFn& innerFn = methodDef.fn;
        enlivenFn(compiler, innerFn);
        bitSetUnionInto(&compiler.arena, &liveOuts, innerFn.freeVars());

        // Param types:
        for (size_t i = innerFn.domain.count; i-- > 0;) {
            IRName const type = innerFn.domain.vals[i];
            if (type.isValid()) {
                requireLive(compiler, liveOuts, type);
            }
        }
    }; break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP: assert(false); break; // Should not exist yet

    case IRStmt::KNOT: {
        KnotStmt const& knot = stmt.knot;
        rangeStart(liveOuts, knot.name);
    }; break;

    case IRStmt::KNOT_INIT: {
        KnotInitStmt const& knotInit = stmt.knotInit;
        requireLive(compiler, liveOuts, knotInit.v);
        requireLive(compiler, liveOuts, knotInit.knot);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt const& knotGet = stmt.knotGet;
        rangeStart(liveOuts, knotGet.name);
        requireLive(compiler, liveOuts, knotGet.knot);
    }; break;

    case IRStmt::FFI_CALL: {
        FFICall const& ffiCall = stmt.ffiCall;

        rangeStart(liveOuts, ffiCall.name);
        requireLive(compiler, liveOuts, ffiCall.codomain.name);
        requireLive(compiler, liveOuts, ffiCall.callee);

        for (FFICall::Arg const& arg : ffiCall.args) {
            requireLive(compiler, liveOuts, arg.name);
        }
    }; break;
    }
}

/// Compute block liveness, backwards (first transfer, then statements in reverse, then params).
void enlivenBlock(Compiler& compiler, IRFn const& fn, IRBlock& block) {
    BitSet* lives = &block.liveIns;
    transferLiveOutsInto(compiler, fn, *lives, block.transfer);

    enlivenTransfer(compiler, *lives, block.transfer);

    for (size_t i = block.stmts.count(); i-- > 0;) {
        enlivenStmt(compiler, *lives, block.stmts[i]);
    }

    for (size_t i = block.params.count(); i-- > 0;) {
        rangeStart(*lives, block.params[i]);
    }
}

void enlivenFn(Compiler& compiler, IRFn& fn) {
    for (size_t i = fn.blocks.count(); i-- > 0;) {
        enlivenBlock(compiler, fn, *fn.blocks[i]);
    }
}

} // namespace
