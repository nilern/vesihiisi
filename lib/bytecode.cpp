#include "bytecode.hpp"

#include <stddef.h>

namespace {

int64_t decodeVarInt(size_t* i, Slice<uint8_t const> src) {
    assert(i);

    uint8_t byte = src[(*i)++];
    // The first payload needs to be sign-extended from `bytecodeVarIntPayloadWidth` to 64 bits:
    int64_t res = int8_t(byte << (UINT8_WIDTH - bytecodeVarIntPayloadWidth))
        >> (INT8_WIDTH - bytecodeVarIntPayloadWidth);

    while (!(byte & bytecodeVarIntTerminalBit)) {
        byte = src[(*i)++];
        res = (res << bytecodeVarIntPayloadWidth) | (byte & bytecodeVarIntPayloadMask);
    }

    return res;
}

void ZLoc::print(State const& state, FILE *dest) const {
    bool posPrinted = false;
    if (isString(&state, maybeFilename)) {
        auto const filename = HRef<String>::fromUnchecked(maybeFilename);

        printFilename(dest, filename.ptr()->str());

        Maybe<Coord> const maybePos = fileByteIdxToCoord(filename.ptr()->str(), srcByteIdx);
        if (maybePos.hasVal) {
            putc(':', dest);
            maybePos.val.print(dest);

            posPrinted = true;
        }
    }

    if (!posPrinted) { fprintf(dest, " at byte %lu", srcByteIdx); }
}

class Disassembler {
protected:
    State const* state;
    Method const* method;
    Slice<uint8_t const> codeSlice;
    size_t pc;

public:
    struct MaybeLocatedCodeByte {
        uint8_t codeByte;
        Maybe<ZLoc> loc;
    };

    Disassembler(State const* t_state, Method const* t_method, size_t t_pc) :
        state{t_state}, method{t_method}, pc{t_pc}
    {
        assert(isHeaped(t_method->code));
        codeSlice = HRef<ByteArray>::fromUnchecked(t_method->code).ptr()->items();
    }

    Disassembler(State const* t_state, Method const* t_method)
        : Disassembler{t_state, t_method, 0} {}

    static Disassembler* create(State const* t_state, Method const* t_method);

    virtual ~Disassembler() = default;

    virtual Maybe<MaybeLocatedCodeByte> next() {
        if (pc < codeSlice.count) {
            return Maybe{MaybeLocatedCodeByte{codeSlice[pc++], {}}};
        } else {
            return Maybe<MaybeLocatedCodeByte>{};
        }
    }

    void disassembleReg(FILE* dest, uint8_t reg);

    void disassembleDisplacement(FILE* dest);

    void disassembleRegBits(FILE* dest);

    void disassembleNestedInstr(FILE* dest, size_t nesting, uint8_t codeByte);
};

class LocDisassembler : public Disassembler {
    ORef maybeFilename;
    size_t bytesToNextMaybeFilename;
    int64_t srcByteIdx;
    int64_t bytesToNextSrcByteIdx;
    Slice<ORef const> filenames;
    size_t filenameIdx;
    Slice<uint8_t const> srcByteIdxs;
    size_t srcByteIdxsIdx;

private:
    friend Disassembler;

    LocDisassembler(State const* t_state, Method const* t_method) :
        Disassembler{t_state, t_method}, srcByteIdx{0}, filenameIdx{0}, srcByteIdxsIdx{0}
    {
        assert(isa(state, state->types.array, method->maybeFilenames));
        filenames = HRef<Array>::fromUnchecked(method->maybeFilenames).ptr()->items();
        assert(isa(state, state->types.byteArray, method->maybeSrcByteIdxs));
        srcByteIdxs = HRef<ByteArray>::fromUnchecked(method->maybeSrcByteIdxs).ptr()->items();

        maybeFilename = filenames[filenameIdx++];
        assert(isa(state, state->types.fixnum, filenames[filenameIdx]));
        bytesToNextMaybeFilename = (uint64_t)Fixnum::fromUnchecked(filenames[filenameIdx++]).val();

        srcByteIdx += decodeVarInt(&srcByteIdxsIdx, srcByteIdxs);
        if (srcByteIdxsIdx < srcByteIdxs.count) {
            bytesToNextSrcByteIdx = decodeVarInt(&srcByteIdxsIdx, srcByteIdxs);
        } else { // This is the last one:
            bytesToNextSrcByteIdx = int64_t(codeSlice.count - pc);
        }
    }

public:
    virtual Maybe<MaybeLocatedCodeByte> next() override {
        Maybe<MaybeLocatedCodeByte> const byteRes = Disassembler::next();

        if (byteRes.hasVal) {
            if (bytesToNextMaybeFilename == 0) {
                maybeFilename = filenames[filenameIdx++];
                assert(isa(state, state->types.fixnum, filenames[filenameIdx]));
                bytesToNextMaybeFilename =
                    (uint64_t)Fixnum::fromUnchecked(filenames[filenameIdx++]).val();
            }
            --bytesToNextMaybeFilename;

            if (bytesToNextSrcByteIdx == 0) {
                srcByteIdx += decodeVarInt(&srcByteIdxsIdx, srcByteIdxs);
                if (srcByteIdxsIdx < srcByteIdxs.count) {
                    bytesToNextSrcByteIdx = decodeVarInt(&srcByteIdxsIdx, srcByteIdxs);
                } else { // This is the last one:
                    bytesToNextSrcByteIdx = int64_t(codeSlice.count - pc + 1);
                }
            }
            --bytesToNextSrcByteIdx;

            auto const loc = ZLoc{maybeFilename, size_t(srcByteIdx)};
            return Maybe{MaybeLocatedCodeByte{byteRes.val.codeByte, Maybe{loc}}};
        } else {
            return byteRes;
        }
    }
};

Disassembler* Disassembler::create(State const* state, Method const* method) {
    if (isa(state, state->types.array, method->maybeFilenames)
        && isa(state, state->types.byteArray, method->maybeSrcByteIdxs)
        ) {
        return new LocDisassembler{state, method};
    } else {
        return new Disassembler{state, method};
    }
}

void Disassembler::disassembleReg(FILE* dest, uint8_t reg) { fprintf(dest, "r%u", reg); }

void Disassembler::disassembleDisplacement(FILE* dest) {
    uint16_t displacement = next().val.codeByte;
    displacement = (uint16_t)(displacement << UINT8_WIDTH) | next().val.codeByte;

    fprintf(dest, "%u", displacement);
}

void Disassembler::disassembleRegBits(FILE* dest) {
    uint8_t const byteCount = next().val.codeByte;
    if (byteCount > 0) { fprintf(dest, "#b"); }
    for (size_t j = 0; j < byteCount; ++j) {
        uint8_t const byte = next().val.codeByte;

        for (size_t k = 0; k < UINT8_WIDTH; ++k) {
            uint8_t const bit = (byte >> (UINT8_WIDTH - 1 - k)) & 1;

            fprintf(dest, "%u", bit);
        }
    }
}

void disassembleNested(State const* state, FILE* dest, HRef<Method> methodRef, size_t nesting);

void Disassembler::disassembleNestedInstr(FILE* dest, size_t nesting, uint8_t codeByte) {
    ORef const* const consts = HRef<ArrayMut>::fromUnchecked(method->consts).ptr()->flexData();

    for (size_t j = 0; j < nesting; ++j) { fputc('\t', dest); }
    fprintf(dest, "[%lu]:\t", pc - 1);

    switch ((Opcode)codeByte) { // FIXME: Handle invalid instruction
    case OP_MOVE: {
        disassembleReg(dest, next().val.codeByte);
        fprintf(dest, " = mov ");
        disassembleReg(dest, next().val.codeByte);
    }; break;

    case OP_SWAP: {
        fprintf(dest, "swap ");
        disassembleReg(dest, next().val.codeByte);
        fputc(' ', dest);
        disassembleReg(dest, next().val.codeByte);
    }; break;

    case OP_DEF: {
        fprintf(dest, "def ");
        uint8_t const constIdx = next().val.codeByte;
        fprintf(dest, "%u ", constIdx);
        disassembleReg(dest, next().val.codeByte);
        fprintf(dest, "\t; ");
        print(state, dest, consts[constIdx]); // FIXME: Bounds check
    }; break;

    case OP_GLOBAL: {
        disassembleReg(dest, next().val.codeByte);
        fprintf(dest, " = global ");
        uint8_t const constIdx = next().val.codeByte;
        fprintf(dest, "%u\t; ", constIdx);
        print(state, dest, consts[constIdx]); // FIXME: Bounds check
    }; break;

    case OP_CONST: {
        disassembleReg(dest, next().val.codeByte);
        fprintf(dest, " = const ");
        uint8_t const constIdx = next().val.codeByte;
        fprintf(dest, "%u", constIdx);
        ORef const c = consts[constIdx]; // FIXME: Bounds check

        if (!isMethod(state, c)) {
            fprintf(dest, "\t; ");
            ORef const c = consts[constIdx]; // FIXME: Bounds check
            print(state, dest, c);
        } else {
            HRef<Method> const innerMethod = HRef<Method>::fromUnchecked(c);

            fputc('\n', dest);
            disassembleNested(state, dest, innerMethod, nesting + 1);
        }
    }; break;

    case OP_SPECIALIZE: {
        disassembleReg(dest, next().val.codeByte);
        fprintf(dest, " = specialize ");
        uint8_t const constIdx = next().val.codeByte;
        fprintf(dest, "%u ", constIdx);
        ORef const c = consts[constIdx]; // FIXME: Bounds check

        disassembleRegBits(dest);

        fputc('\n', dest);

        assert(isMethod(state, c));
        HRef<Method> const innerMethod = HRef<Method>::fromUnchecked(c);
        disassembleNested(state, dest, innerMethod, nesting + 1);
    }; break;

    case OP_KNOT: {
        disassembleReg(dest, next().val.codeByte);
        fputs(" = knot ()", dest);
    }; break;

    case OP_KNOT_INIT: {
        fputs("knot-init! ", dest);
        disassembleReg(dest, next().val.codeByte);
        putc(' ', dest);
        disassembleReg(dest, next().val.codeByte);
    }; break;

    case OP_KNOT_GET: {
        disassembleReg(dest, next().val.codeByte);
        fputs(" = knot-get ", dest);
        disassembleReg(dest, next().val.codeByte);
    }; break;

    case OP_BRF: {
        fprintf(dest, "brf ");
        disassembleReg(dest, next().val.codeByte);
        fputc(' ', dest);
        disassembleDisplacement(dest);
    }; break;

    case OP_BR: {
        fprintf(dest, "br ");
        disassembleDisplacement(dest);
    }; break;

    case OP_RET: {
        fprintf(dest, "ret");
    }; break;

    case OP_CLOSURE: {
        disassembleReg(dest, next().val.codeByte);
        fprintf(dest, " = closure ");
        disassembleReg(dest, next().val.codeByte);
        putc(' ', dest);
        disassembleRegBits(dest);
    }; break;

    case OP_CLOVER: {
        disassembleReg(dest, next().val.codeByte);
        fprintf(dest, " = clover ");
        disassembleReg(dest, next().val.codeByte);
        fprintf(dest, " %u", next().val.codeByte);
    }; break;

    case OP_CALL: {
        fprintf(dest, "call %u ", next().val.codeByte);
        disassembleRegBits(dest);
    }; break;

    case OP_TAILCALL: {
        fprintf(dest, "tailcall %u", next().val.codeByte);
    }; break;
    }
}

// TODO: Print labels for BR(F) and their targets:
void disassembleNested(State const* state, FILE* dest, HRef<Method> methodRef, size_t nesting) {
    for (size_t j = 0; j < nesting; ++j) { putc('\t', dest); }
    putc('(', dest);

    ORef const maybeName = methodRef.ptr()->maybeName;
    if (isa(state, state->types.symbol, maybeName)) {
        print(state, dest, maybeName);
    } else {
        putc('_', dest);
    }

    size_t const arity = (uint64_t)methodRef.ptr()->flexCount().val();
    for (size_t i = 0; i < arity; ++i) {
        putc(' ', dest);
        if (i == arity - 1 && methodRef.ptr()->hasVarArg.val()) {
            fputs(". ", dest);
        }

        print(state, dest, methodRef.ptr()->domain()[i]);
    }

    fputs(")\n", dest); // TODO: Source location for function (not the same as for instr #1!)

    if (isHeaped(methodRef.ptr()->code)) {
        Disassembler* const dis = Disassembler::create(state, methodRef.ptr());

        {
            ORef maybeFilename = Default;
            for (Maybe<Disassembler::MaybeLocatedCodeByte> res; (res = dis->next()).hasVal;) {
                auto const mlocOp = res.val;

                // If filename changed, print it:
                if (mlocOp.loc.hasVal) {
                    auto const loc = mlocOp.loc.val;

                    if (!eq(loc.maybeFilename, maybeFilename)) {
                        maybeFilename = loc.maybeFilename;

                        for (size_t j = 0; j < nesting; ++j) { putc('\t', dest); }
                        fputs(";; In ", dest);

                        if (isString(state, maybeFilename)) {
                            auto const filename = HRef<String>::fromUnchecked(maybeFilename);

                            printFilename(dest, filename.ptr()->str());
                        } else {
                            fputs("???", dest);
                        }

                        putc('\n', dest);
                    }
                }

                dis->disassembleNestedInstr(dest, nesting, mlocOp.codeByte);

                if (mlocOp.loc.hasVal) {
                    auto const loc = mlocOp.loc.val;

                    // TODO: line:col (when possible):
                    fprintf(dest, "\t; at byte %lu", loc.srcByteIdx);
                }

                fputc('\n', dest);
            }
        }

        delete dis;
    } else {
        fprintf(dest, "#primop\n");
    }
}

void disassemble(State const* state, FILE* dest, HRef<Method> methodRef) {
    disassembleNested(state, dest, methodRef, 0);
}

void disassembleInstrAt(State const* state, FILE* dest, HRef<Method> methodRef, size_t pc) {
    assert(isHeaped(methodRef.ptr()->code));

    Method const* const method =methodRef.ptr();
    auto dis = Disassembler{state, method, pc + 1};
    uint8_t const codeByte = HRef<ByteArray>::fromUnchecked(method->code).ptr()->items()[pc];
    dis.disassembleNestedInstr(dest, 0, codeByte);
}

Maybe<ZLoc> locatePc(HRef<Method> methodRef, size_t pc) {
    Method const* const method = methodRef.ptr();

    if (!isHeaped(method->maybeFilenames) || !isHeaped(method->maybeSrcByteIdxs)) {
        return Maybe<ZLoc>{};
    }
    Slice<ORef const> const filenames =
        HRef<Array>::fromUnchecked(method->maybeFilenames).ptr()->items();
    Slice<uint8_t const> const srcByteIdxs =
        HRef<ByteArray>::fromUnchecked(method->maybeSrcByteIdxs).ptr()->items();

    ORef maybeFilename = Default;
    for (size_t i = 1, filenameStartPc = 0; i < filenames.count; i += 2) {
        size_t const filenameCount = uint64_t(Fixnum::fromUnchecked(filenames[i]).val());
        size_t const filenameEndPc = filenameStartPc + filenameCount;

        if (filenameStartPc <= pc && pc < filenameEndPc) {
            maybeFilename = filenames[i - 1];
            break;
        }
    }

    int64_t srcByteIdx = 0;
    for (size_t i = 0, codeByteIdx = 0; i < srcByteIdxs.count;) {
        srcByteIdx += decodeVarInt(&i, srcByteIdxs);
        size_t const newCodeByteIdx = size_t(int64_t(codeByteIdx) + decodeVarInt(&i, srcByteIdxs));

        if (codeByteIdx <= pc && pc < newCodeByteIdx) { break; }

        codeByteIdx = newCodeByteIdx;
    }

    return Maybe{ZLoc{maybeFilename, size_t(srcByteIdx)}};
}

} // namespace
