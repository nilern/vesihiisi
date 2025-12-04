typedef enum Opcode : uint8_t {
    OP_CONST,
    OP_RET
} Opcode;

inline static void disassembleReg(FILE* dest, uint8_t reg) { fprintf(dest, "r%u", reg); }

static void disassemble(State const* state, FILE* dest, MethodRef methodRef) {
    Method const* const method = methodToPtr(methodRef);
    uint8_t const* const code = byteArrayToPtr(method->code);
    uintptr_t const codeCount = (uintptr_t)fixnumToInt(byteArrayCount(method->code));
    ORef const* const consts = arrayToPtr(method->consts);

    for (size_t i = 0; i < codeCount;) {
        switch ((Opcode)code[i++]) { // FIXME: Handle invalid instruction
        case OP_CONST:
            disassembleReg(dest, code[i++]);
            fprintf(dest, " = const ");
            uint8_t const constIndex = code[i++];
            fprintf(dest, "%u ; ", constIndex);
            print(state, dest, consts[constIndex]); // FIXME: Bounds check
            break;

        case OP_RET:
            fprintf(dest, "ret ");
            disassembleReg(dest, code[i++]);
            fputc(' ', dest);
            disassembleReg(dest, code[i++]);
            break;
        }

        fputc('\n', dest);
    }
}
