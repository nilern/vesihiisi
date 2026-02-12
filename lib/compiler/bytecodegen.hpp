#pragma once

#include "ir.hpp"
#include "../bytecode.hpp"

namespace {

// Bytecode Method Builder
// =================================================================================================

class MethodBuilder {
    class LabelIdxs {
        size_t* idxs_;

    public:
        LabelIdxs(Arena* arena,  size_t blockCount) :
            idxs_{static_cast<decltype(idxs_)>(amalloc(arena, blockCount * sizeof *idxs_))}
        {}

        size_t get(IRLabel label) const { return idxs_[label.blockIndex]; }

        void set(IRLabel label, size_t index) { idxs_[label.blockIndex] = index; }
    };

public:
    struct Const {
        ORef val;
        enum { VALUE, GLOBAL_NAME, INLINE_CACHE } type;

        bool operator==(Const const& that) const { return eq(val, that.val) && type == that.type; }
    };

private:
    AVec<uint8_t> code_;

    LabelIdxs labelIdxs_;

    AVec<Const> consts_;

    size_t prevMaybeLocRevIdx_;
    size_t srcIdx_;
    size_t prevDeltaCodeByteRevIdx_;
    ORef maybeFilename_;
    size_t filenameCount_;
    AVec<uint8_t> revIdxDeltas_;
    AVec<ORef> revFilenameRuns_;

    MethodBuilder* parent_;

    void pushMaybeLoc(RT const& state, ORef maybeLoc);

    uint8_t constIndex(MethodBuilder::Const c);

    void flushMethodBuilderDeltas();

public:
    MethodBuilder(RT const& state, Arena* arena, MethodBuilder* parent, IRFn const& fn);

    [[nodiscard]]
    bool mark(RT& state);
    [[maybe_unused]]
    void assertInTospace(RT const& state) const;

    size_t codeCount() const { return code_.count(); }

    void pushCodeByte(uint8_t byte) { code_.push(byte); }

    void pushOp(RT const& state, Opcode op, ORef maybeLoc);

    void pushReg(IRName name) { pushCodeByte((uint8_t)(name.index)); }

    void pushDisplacement(size_t displacement);

    void emitRegBits(std::span<IRName const> names, bool specializeHack);

    void emitClose(std::span<IRName const> args) { emitRegBits(args, false); }

    uint8_t freshConstIndex(MethodBuilder::Const c);

    void emitFreshConstArg(MethodBuilder::Const c) { pushCodeByte(freshConstIndex(c)); }

    void emitConstArg(MethodBuilder::Const c) { pushCodeByte(constIndex(c)); }

    void setLabelIndex(IRLabel label, size_t index) { labelIdxs_.set(label, index); }
    size_t getLabelIndex(IRLabel label) const { return labelIdxs_.get(label); }

    HRef<Method> buildMethod(RT& state, IRFn& toplevelFn, IRFn const& fn) &&;
};

// Bytecode Emission
// =================================================================================================

HRef<Method> emitToplevelMethod(RT& state, Compiler& compiler, IRFn& fn);

} // namespace
