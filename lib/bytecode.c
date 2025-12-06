// TODO: Bigger (only when necessary?) displacements for BR(F)?

typedef enum Opcode : uint8_t {
    OP_DEF,
    OP_GLOBAL,
    OP_CONST,
    OP_BRF,
    OP_BR,
    OP_RET
} Opcode;

inline static void disassembleReg(FILE* dest, uint8_t reg) { fprintf(dest, "r%u", reg); }

inline static void disassembleDisplacement(FILE* dest, uint8_t len) { fprintf(dest, "%u", len); }

// TODO: Print labels for BR(F) and their targets:
static void disassemble(State const* state, FILE* dest, MethodRef methodRef) {
    Method const* const method = methodToPtr(methodRef);
    uint8_t const* const code = byteArrayToPtr(method->code);
    uintptr_t const codeCount = (uintptr_t)fixnumToInt(byteArrayCount(method->code));
    ORef const* const consts = arrayToPtr(method->consts);

    for (size_t i = 0; i < codeCount;) {
        fprintf(dest, "[%lu]:\t", i);

        switch ((Opcode)code[i++]) { // FIXME: Handle invalid instruction
        case OP_DEF: {
            fprintf(dest, "def ");
            uint8_t const constIdx = code[i++];
            fprintf(dest, "%u ", constIdx);
            disassembleReg(dest, code[i++]);
            fprintf(dest, "\t; ");
            print(state, dest, consts[constIdx]); // FIXME: Bounds check
            break;
        }

        case OP_GLOBAL: {
            disassembleReg(dest, code[i++]);
            fprintf(dest, " = global ");
            uint8_t const constIdx = code[i++];
            fprintf(dest, "%u\t; ", constIdx);
            print(state, dest, consts[constIdx]); // FIXME: Bounds check
            break;
        }

        case OP_CONST: {
            disassembleReg(dest, code[i++]);
            fprintf(dest, " = const ");
            uint8_t const constIdx = code[i++];
            fprintf(dest, "%u\t; ", constIdx);
            print(state, dest, consts[constIdx]); // FIXME: Bounds check
            break;
        }

        case OP_BRF: {
            fprintf(dest, "brf ");
            disassembleReg(dest, code[i++]);
            fputc(' ', dest);
            disassembleDisplacement(dest, code[i++]);
            break;
        }

        case OP_BR: {
            fprintf(dest, "br ");
            disassembleDisplacement(dest, code[i++]);
            break;
        }

        case OP_RET: {
            fprintf(dest, "ret ");
            disassembleReg(dest, code[i++]);
            fputc(' ', dest);
            disassembleReg(dest, code[i++]);
            break;
        }
        }

        fputc('\n', dest);
    }
}
