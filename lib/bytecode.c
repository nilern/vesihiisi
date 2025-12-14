// TODO: Bigger (only when necessary?) displacements for BR(F)?

typedef enum Opcode : uint8_t {
    OP_MOVE,
    OP_SWAP,
    OP_DEF,
    OP_GLOBAL,
    OP_CONST,
    OP_BRF,
    OP_BR,
    OP_RET,
    OP_CLOSURE,
    OP_CLOVER,
    OP_TAILCALL
} Opcode;

inline static void disassembleReg(FILE* dest, uint8_t reg) { fprintf(dest, "r%u", reg); }

inline static void disassembleDisplacement(FILE* dest, uint8_t len) { fprintf(dest, "%u", len); }

// TODO: Print labels for BR(F) and their targets:
static void disassembleNested(State const* state, FILE* dest, MethodRef methodRef, size_t nesting) {
    Method const* const method = methodToPtr(methodRef);
    uint8_t const* const code = byteArrayToPtr(method->code);
    uintptr_t const codeCount = (uintptr_t)fixnumToInt(byteArrayCount(method->code));
    ORef const* const consts = arrayToPtr(method->consts);

    for (size_t i = 0; i < codeCount;) {
        for (size_t j = 0; j < nesting; ++j) { fputc('\t', dest); }
        fprintf(dest, "[%lu]:\t", i);

        switch ((Opcode)code[i++]) { // FIXME: Handle invalid instruction
        case OP_MOVE: {
            fprintf(dest, "mov ");
            disassembleReg(dest, code[i++]);
            fputc(' ', dest);
            disassembleReg(dest, code[i++]);
        }; break;

        case OP_SWAP: {
            fprintf(dest, "swap ");
            disassembleReg(dest, code[i++]);
            fputc(' ', dest);
            disassembleReg(dest, code[i++]);
        }; break;

        case OP_DEF: {
            fprintf(dest, "def ");
            uint8_t const constIdx = code[i++];
            fprintf(dest, "%u ", constIdx);
            disassembleReg(dest, code[i++]);
            fprintf(dest, "\t; ");
            print(state, dest, consts[constIdx]); // FIXME: Bounds check
        }; break;

        case OP_GLOBAL: {
            disassembleReg(dest, code[i++]);
            fprintf(dest, " = global ");
            uint8_t const constIdx = code[i++];
            fprintf(dest, "%u\t; ", constIdx);
            print(state, dest, consts[constIdx]); // FIXME: Bounds check
        }; break;

        case OP_CONST: {
            disassembleReg(dest, code[i++]);
            fprintf(dest, " = const ");
            uint8_t const constIdx = code[i++];
            fprintf(dest, "%u\t; ", constIdx);
            print(state, dest, consts[constIdx]); // FIXME: Bounds check
        }; break;

        case OP_BRF: {
            fprintf(dest, "brf ");
            disassembleReg(dest, code[i++]);
            fputc(' ', dest);
            disassembleDisplacement(dest, code[i++]);
        }; break;

        case OP_BR: {
            fprintf(dest, "br ");
            disassembleDisplacement(dest, code[i++]);
        }; break;

        case OP_RET: {
            fprintf(dest, "ret");
        }; break;

        case OP_CLOSURE: {
            disassembleReg(dest, code[i++]);
            uint8_t const methodConstIdx = code[i++];
            fprintf(dest, " = closure %u", methodConstIdx);

            uint8_t const byteCount = code[i++];
            if (byteCount > 0) { fprintf(dest, " #b"); }
            for (size_t j = 0; j < byteCount; ++j) {
                uint8_t const byte = code[i++];

                for (size_t k = 0; k < UINT8_WIDTH; ++k) {
                    uint8_t const bit = (byte >> (UINT8_WIDTH - 1 - k)) & 1;

                    fprintf(dest, "%u", bit);
                }
            }
            fputc('\n', dest);

            // TODO: Bounds & type check:
            MethodRef const innerMethod = uncheckedORefToMethod(consts[methodConstIdx]);
            disassembleNested(state, dest, innerMethod, nesting + 1);
        }; break;

        case OP_CLOVER: {
            disassembleReg(dest, code[i++]);
            fprintf(dest, " = clover ");
            disassembleReg(dest, code[i++]);
            fprintf(dest, " %u", code[i++]);
        }; break;

        case OP_TAILCALL: {
            fprintf(dest, "tailcall %u", code[i++]);
        }; break;
        }

        fputc('\n', dest);
    }
}

static void disassemble(State const* state, FILE* dest, MethodRef methodRef) {
    disassembleNested(state, dest, methodRef, 0);
}
