static PrimopRes callBytecode(State* /*state*/) { return PRIMOP_RES_TAILCALL; }

static PrimopRes primopIdentical(State* state) {
    ORef const x = state->regs[firstArgReg];
    ORef const y = state->regs[firstArgReg + 1];

    state->regs[retReg] = boolToORef(tagBool(eq(x, y)));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxAdd(State* state) {
    ORef const xRef = state->regs[firstArgReg];
    ORef const yRef = state->regs[firstArgReg + 1];

    if (!isFixnum(xRef)) { assert(false); } // TODO: Type error
    intptr_t const x = uncheckedFixnumToInt(xRef);
    if (!isFixnum(yRef)) { assert(false); } // TODO: Type error
    intptr_t const y = uncheckedFixnumToInt(yRef);

    state->regs[retReg] = fixnumToORef(tagInt(x + y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxSub(State* state) {
    ORef const xRef = state->regs[firstArgReg];
    ORef const yRef = state->regs[firstArgReg + 1];

    if (!isFixnum(xRef)) { assert(false); } // TODO: Type error
    intptr_t const x = uncheckedFixnumToInt(xRef);
    if (!isFixnum(yRef)) { assert(false); } // TODO: Type error
    intptr_t const y = uncheckedFixnumToInt(yRef);

    state->regs[retReg] = fixnumToORef(tagInt(x - y)); // TODO: Underflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxMul(State* state) {
    ORef const xRef = state->regs[firstArgReg];
    ORef const yRef = state->regs[firstArgReg + 1];

    if (!isFixnum(xRef)) { assert(false); } // TODO: Type error
    intptr_t const x = uncheckedFixnumToInt(xRef);
    if (!isFixnum(yRef)) { assert(false); } // TODO: Type error
    intptr_t const y = uncheckedFixnumToInt(yRef);

    state->regs[retReg] = fixnumToORef(tagInt(x * y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}
