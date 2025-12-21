// TODO: Bigger (only when necessary?) displacements for BR(F)?

// OPTIMIZE: 4-bit opcode with one 4-bit argument when possible?

typedef enum Opcode : uint8_t {
    OP_MOVE,
    OP_SWAP,
    OP_DEF,
    // TODO: OP_GLOBAL_SET
    OP_GLOBAL,
    OP_CONST,
    // TODO: OP_BOX
    // TODO: OP_BOX_SET
    // TODO: OP_BOX_GET
    OP_BRF,
    OP_BR,
    OP_RET,
    OP_CLOSURE,
    OP_CLOVER,
    // TODO: OP_CONT_CLOVER / OP_RESTORE
    OP_CALL,
    OP_TAILCALL // TODO: Encode as call with 0 closes (nontail call always saves at least ret cont)?
} Opcode;

// Calling convention:
static const uint8_t calleeReg = 1;
static const uint8_t retContReg = 0;
static const uint8_t firstArgReg = 2;
static const uint8_t retReg = firstArgReg;

inline static void disassembleReg(FILE* dest, uint8_t reg) { fprintf(dest, "r%u", reg); }

inline static void disassembleDisplacement(FILE* dest, uint8_t len) { fprintf(dest, "%u", len); }

[[nodiscard]]
static size_t disassembleCloses(FILE* dest, uint8_t const* code, size_t i) {
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
    assert(method->nativeCode == callBytecode);
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
        fprintf(dest, "%u\t; ", constIdx);
        print(state, dest, consts[constIdx]); // FIXME: Bounds check
    }; break;

    case OP_BRF: {
        fprintf(dest, "brf ");
        disassembleReg(dest, code[pc++]);
        fputc(' ', dest);
        disassembleDisplacement(dest, code[pc++]);
    }; break;

    case OP_BR: {
        fprintf(dest, "br ");
        disassembleDisplacement(dest, code[pc++]);
    }; break;

    case OP_RET: {
        fprintf(dest, "ret");
    }; break;

    case OP_CLOSURE: {
        disassembleReg(dest, code[pc++]);
        uint8_t const methodConstIdx = code[pc++];
        fprintf(dest, " = closure %u ", methodConstIdx);

        pc = disassembleCloses(dest, code, pc);
        fputc('\n', dest);

        // TODO: Bounds & type check:
        MethodRef const innerMethod = uncheckedORefToMethod(consts[methodConstIdx]);
        disassembleNested(state, dest, innerMethod, nesting + 1);
    }; break;

    case OP_CLOVER: {
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " = clover ");
        disassembleReg(dest, code[pc++]);
        fprintf(dest, " %u", code[pc++]);
    }; break;

    case OP_CALL: {
        fprintf(dest, "call %u ", code[pc++]);
        pc = disassembleCloses(dest, code, pc);
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
    size_t const arity = (uintptr_t)fixnumToInt(flexLength(methodToORef(methodRef)));
    for (size_t i = 0; i < arity; ++i) {
        if (i > 0) { putc(' ', dest); }
        print(state, dest, typeToORef(method->domain[i]));
    }
    fputs(")\n", dest);

    if (method->nativeCode == callBytecode) {
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
