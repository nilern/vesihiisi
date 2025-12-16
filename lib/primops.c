static PrimopRes callBytecode(State* /*state*/) { return PRIMOP_RES_TAILCALL; }

static PrimopRes primopIdentical(State* state) {
    ORef const x = state->regs[firstArgReg];
    ORef const y = state->regs[firstArgReg + 1];

    state->regs[retReg] = boolToORef(tagBool(eq(x, y)));

    return PRIMOP_RES_CONTINUE;
}
