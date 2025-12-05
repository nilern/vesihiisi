static ORef run(State* state, ClosureRef selfRef) {
    Closure const* const self = closureToPtr(selfRef);
    ORef const anyMethod = self->method;
    // TODO: Debug type check:
    Method const* const method = methodToPtr(uncheckedORefToMethod(anyMethod));
    state->code = byteArrayToPtr(method->code);
    state->pc = 0;
    state->consts = arrayToPtr(method->consts);
    state->regs[0] = closureToORef(selfRef);
    state->regs[1] = closureToORef(state->exit); // Return continuation

    for (;/*ever*/;) {
        switch ((Opcode)state->code[state->pc++]) {
        case OP_CONST:
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            state->regs[destReg] = state->consts[constIdx];
            break;

        case OP_RET:
            uint8_t const retReg = state->code[state->pc++];
            uint8_t const vReg = state->code[state->pc++];

            ClosureRef const retRef = uncheckedORefToClosure(state->regs[retReg]);
            Closure const* const ret = closureToPtr(retRef);
            ORef const anyMethod = ret->method;
            if (!eq(anyMethod, fixnumToORef(Zero))) {
                assert(false); // TODO
            } else { // Exit
                return state->regs[vReg];
            }
        }
    }
}
