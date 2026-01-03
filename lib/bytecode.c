#include "bytecode.h"

#include <stddef.h>

static const uint8_t calleeReg = 1;
static const uint8_t retContReg = 0;
static const uint8_t firstArgReg = 2;
static const uint8_t retReg = firstArgReg;

inline static void disassembleReg(FILE* dest, uint8_t reg) { fprintf(dest, "r%u", reg); }

[[nodiscard]]
inline static size_t disassembleDisplacement(FILE* dest, uint8_t const* code, size_t i) {
    uint16_t displacement = code[i++];
    displacement = (uint16_t)(displacement << UINT8_WIDTH) | code[i++];

    fprintf(dest, "%u", displacement);

    return i;
}

[[nodiscard]]
static size_t disassembleRegBits(FILE* dest, uint8_t const* code, size_t i) {
    uint8_t const byteCount = code[i++];
    if (byteCount > 0) { fprintf(dest, "#b"); }
    for (size_t j = 0; j < byteCount; ++j) {
        uint8_t const byte = code[i++];

        for (size_t k = 0; k < UINT8_WIDTH; ++k) {
            uint8_t const bit = (byte >> (UINT8_WIDTH - 1 - k)) & 1;

            fprintf(dest, "%u", bit);
        }
    }

    return i;
}

static void disassembleNested(State const* state, FILE* dest, MethodRef methodRef, size_t nesting);

[[nodiscard]]
static size_t disassembleNestedInstr(
    State const* state, FILE* dest, MethodRef methodRef, size_t pc, size_t nesting
) {
    Method const* const method = methodToPtr(methodRef);
    assert(isHeaped(method->code));
    uint8_t const* const code = byteArrayToPtr(uncheckedORefToByteArray(method->code));
    ORef const* const consts = arrayToPtr(uncheckedORefToArray(method->consts));

    for (size_t j = 0; j < nesting; ++j) { fputc('\t', dest); }
    fprintf(dest, "[%lu]:\t", pc);

    switch ((Opcode)code[pc++]) { // FIXME: Handle invalid instruction
    case OP_MOVE: {
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " = mov ");
        disassembleReg(dest, code[pc++]);
    }; break;

    case OP_SWAP: {
        fprintf(dest, "swap ");
        disassembleReg(dest, code[pc++]);
        fputc(' ', dest);
        disassembleReg(dest, code[pc++]);
    }; break;

    case OP_DEF: {
        fprintf(dest, "def ");
        uint8_t const constIdx = code[pc++];
        fprintf(dest, "%u ", constIdx);
        disassembleReg(dest, code[pc++]);
        fprintf(dest, "\t; ");
        print(state, dest, consts[constIdx]); // FIXME: Bounds check
    }; break;

    case OP_GLOBAL: {
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " = global ");
        uint8_t const constIdx = code[pc++];
        fprintf(dest, "%u\t; ", constIdx);
        print(state, dest, consts[constIdx]); // FIXME: Bounds check
    }; break;

    case OP_CONST: {
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " = const ");
        uint8_t const constIdx = code[pc++];
        fprintf(dest, "%u", constIdx);
        ORef const c = consts[constIdx]; // FIXME: Bounds check

        if (!isMethod(state, c)) {
            fprintf(dest, "\t; ");
            ORef const c = consts[constIdx]; // FIXME: Bounds check
            print(state, dest, c);
        } else {
            MethodRef const innerMethod = uncheckedORefToMethod(c);

            fputc('\n', dest);
            disassembleNested(state, dest, innerMethod, nesting + 1);
        }
    }; break;

    case OP_SPECIALIZE: {
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " = specialize ");
        uint8_t const constIdx = code[pc++];
        fprintf(dest, "%u ", constIdx);
        ORef const c = consts[constIdx]; // FIXME: Bounds check

        pc = disassembleRegBits(dest, code, pc);

        fputc('\n', dest);

        assert(isMethod(state, c));
        MethodRef const innerMethod = uncheckedORefToMethod(c);
        disassembleNested(state, dest, innerMethod, nesting + 1);
    }; break;

    case OP_KNOT: {
        disassembleReg(dest, code[pc++]);
        fputs(" = knot ()", dest);
    }; break;

    case OP_KNOT_INIT: {
        fputs("knot-init! ", dest);
        disassembleReg(dest, code[pc++]);
        putc(' ', dest);
        disassembleReg(dest, code[pc++]);
    }; break;

    case OP_KNOT_GET: {
        disassembleReg(dest, code[pc++]);
        fputs(" = knot-get ", dest);
        disassembleReg(dest, code[pc++]);
    }; break;

    case OP_BRF: {
        fprintf(dest, "brf ");
        disassembleReg(dest, code[pc++]);
        fputc(' ', dest);
        pc = disassembleDisplacement(dest, code, pc);
    }; break;

    case OP_BR: {
        fprintf(dest, "br ");
        pc = disassembleDisplacement(dest, code, pc);
    }; break;

    case OP_RET: {
        fprintf(dest, "ret");
    }; break;

    case OP_CLOSURE: {
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " = closure ");
        disassembleReg(dest, code[pc++]);
        putc(' ', dest);
        pc = disassembleRegBits(dest, code, pc);
    }; break;

    case OP_CLOVER: {
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " = clover ");
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " %u", code[pc++]);
    }; break;

    case OP_CALL: {
        fprintf(dest, "call %u ", code[pc++]);
        pc = disassembleRegBits(dest, code, pc);
    }; break;

    case OP_TAILCALL: {
        fprintf(dest, "tailcall %u", code[pc++]);
    }; break;
    }

    return pc;
}

[[nodiscard, maybe_unused]]
static size_t disassembleInstr(State const* state, FILE* dest, MethodRef methodRef, size_t pc) {
    return disassembleNestedInstr(state, dest, methodRef, pc, 0);
}

// TODO: Print labels for BR(F) and their targets:
static void disassembleNested(State const* state, FILE* dest, MethodRef methodRef, size_t nesting) {
    Method const* const method = methodToPtr(methodRef);

    for (size_t j = 0; j < nesting; ++j) { putc('\t', dest); }
    putc('(', dest);

    ORef const maybeName = method->maybeName;
    if (isa(state, state->symbolType, maybeName)) {
        print(state, dest, maybeName);
    } else {
        putc('_', dest);
    }

    size_t const arity = (uintptr_t)fixnumToInt(flexLength(methodToORef(methodRef)));
    for (size_t i = 0; i < arity; ++i) {
        putc(' ', dest);
        if (i == arity - 1 && eq(boolToORef(method->hasVarArg), boolToORef(True))) {
            fputs(". ", dest);
        }

        print(state, dest, method->domain[i]);
    }

    fputs(")\n", dest);

    if (isHeaped(method->code)) {
        size_t const codeCount =
            (uintptr_t)fixnumToInt(byteArrayCount(uncheckedORefToByteArray(method->code)));
        for (size_t pc = 0; pc < codeCount;) {
            pc = disassembleNestedInstr(state, dest, methodRef, pc, nesting);
            fputc('\n', dest);
        }
    } else {
        fprintf(dest, "#primop\n");
    }
}

static void disassemble(State const* state, FILE* dest, MethodRef methodRef) {
    disassembleNested(state, dest, methodRef, 0);
}
