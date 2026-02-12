#include "cloverindexing.hpp"

#include <optional>

#include "../util/util.hpp"
#include "../util/bitset.hpp"
#include "../util/asmallmap.hpp"
#include "regalloc.hpp"

namespace {

class CloverIdxs {
    ASmallMap<IRName, uint8_t> idxs_;

    explicit CloverIdxs(ASmallMap<IRName, uint8_t>&& idxs) : idxs_{std::move(idxs)} {}

public:
    explicit CloverIdxs(Arena* arena) : idxs_{arena} {}

    CloverIdxs clone() const { return CloverIdxs{idxs_.clone()}; }

    void set(IRName name, uint8_t idx) { idxs_.set(name, idx); }

    uint8_t get(IRName origName) const {
        std::optional<uint8_t> const optIdx = idxs_.tryGet(origName);
        assert(optIdx);
        return *optIdx;
    }
};

class CloverIndexing {
    std::optional<CloverIdxs>* savedEnvs;

public:
    CloverIndexing(Compiler& compiler, IRFn const& fn) :
        savedEnvs{static_cast<std::optional<CloverIdxs>*>(acalloc(
              &compiler.arena, fn.blocks.count(), sizeof *savedEnvs))}
    {}

    void saveBlockEnv(IRLabel label, CloverIdxs const& env) {
        savedEnvs[label.blockIndex] = std::optional{env.clone()};
    }

    CloverIdxs const& getBlockEnv(IRLabel label) const {
        assert(savedEnvs[label.blockIndex].has_value());
        return *savedEnvs[label.blockIndex];
    }
};

CloverIdxs closeCloverIdxs(
    Compiler& compiler, BitSet const& clovers, std::span<IRName const> close
) {
    auto env = CloverIdxs{&compiler.arena};

    {
        size_t const cloverCount = close.size();
        BitSetIter it = newBitSetIter(&clovers);
        for (size_t i = 0;; ++i) {
            Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
            if (!maybeIdx.hasVal) { break; }
            IRName const clover = {maybeIdx.val};

            assert(i < close.size());
            Reg const reg = Reg{(uint8_t)close[i].index};

            // How many clovers in `close` whose register is lower than `reg`?:
            size_t idx = 0;
            for (size_t j = 0; j < cloverCount; ++j) {
                if (close[j].index < reg.index) {
                    ++idx;
                }
            }

            env.set(clover, uint8_t(idx));
        }
    }

    return env;
}

void indexFnClovers(Compiler& compiler, CloverIdxs const& fnEnv, IRFn& fn);

void indexStmtClovers(Compiler& compiler, CloverIdxs const& env, IRStmt& stmt) {
    switch (stmt.type) {
    case IRStmt::GLOBAL_DEF: case IRStmt::GLOBAL_SET: case IRStmt::GLOBAL:
    case IRStmt::CONST_DEF: break; // Not closure-related

    case IRStmt::CLOVER: {
        Clover& clover = stmt.clover;
        clover.idx = env.get(clover.origName);
    }; break;

    case IRStmt::METHOD_DEF: {
        MethodDef& methodDef = stmt.methodDef;
        IRFn& fn = methodDef.fn;

        CloverIdxs innerEnv = closeCloverIdxs(compiler, *fn.freeVars(), *methodDef.closes);
        indexFnClovers(compiler, innerEnv, fn);
    }; break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP:
    case IRStmt::KNOT: case IRStmt::KNOT_INIT: case IRStmt::KNOT_GET:
    case IRStmt::FFI_CALL: break; // Not closure-related
    }
}

void indexTransferClovers(
    Compiler& compiler, CloverIndexing& pass, IRFn const& fn, CloverIdxs const& env,
    IRTransfer const& transfer
) {
    switch (transfer.type) {
    case IRTransfer::CALL: {
        Call const& call = transfer.call;

        IRLabel const retLabel = call.retLabel;
        IRBlock const& retBlock = *fn.blocks[retLabel.blockIndex];
        CloverIdxs retEnv = closeCloverIdxs(compiler, retBlock.liveIns, call.closes);
        pass.saveBlockEnv(retLabel, retEnv);
    }; break;

    case IRTransfer::TAILCALL: break; // Terminator; does not even ned to `saveCloverIdxs`

    case IRTransfer::IF: {
        IRIf const& iff = transfer.iff;
        pass.saveBlockEnv(iff.conseq, env);
        pass.saveBlockEnv(iff.alt, env);
    }; break;

    case IRTransfer::GOTO: {
        pass.saveBlockEnv(transfer.gotoo.dest, env);
    }; break;

    case IRTransfer::RETURN: break; // Terminator; does not even ned to `saveCloverIdxs`
    }
}

void indexBlockClovers(
    Compiler& compiler, CloverIndexing& pass, IRFn const& fn, CloverIdxs const& fnEnv,
    IRBlock& block
) {
    CloverIdxs const& env = block.label.blockIndex != 0 ? pass.getBlockEnv(block.label) : fnEnv;

    size_t const stmtCount = block.stmts.count();
    for (size_t i = 0; i < stmtCount; ++i) {
        indexStmtClovers(compiler, env, block.stmts[i]);
    }

    indexTransferClovers(compiler, pass, fn, env, block.transfer);
}

void indexFnClovers(Compiler& compiler, CloverIdxs const& fnEnv, IRFn& fn) {
    auto pass = CloverIndexing{compiler, fn};

    size_t const blockCount = fn.blocks.count();
    for (size_t i = 0; i < blockCount; ++i) {
        indexBlockClovers(compiler, pass, fn, fnEnv, *fn.blocks[i]);
    }
}

void indexToplevelFnClovers(Compiler& compiler, IRFn& fn) {
    auto const emptyFnEnv = CloverIdxs{&compiler.arena};
    indexFnClovers(compiler, emptyFnEnv, fn);
}

} // namespace
