static ORef getErrorHandler(State const* state) {
    ORef const v = varToPtr(state->errorHandler)->val;
    if (eq(v, unboundToORef(state->unbound))) {
        exit(EXIT_FAILURE); // FIXME
    }

    return v;
}

[[nodiscard]]
static PrimopRes primopTypeError(State* state, TypeRef type, ORef v) {
    state->regs[calleeReg] = getErrorHandler(state);
    state->regs[firstArgReg] = typeErrorToORef(createTypeError(state, type, v));
    return PRIMOP_RES_TAILCALL;
}

static PrimopRes callBytecode(State* /*state*/) { return PRIMOP_RES_TAILCALL; }

static PrimopRes primopAbort(State* state) {
    ORef const error = state->regs[firstArgReg];

    fputs("Runtime error: ", stderr);
    print(state, stderr, error);
    putc('\n', stderr);

    return PRIMOP_RES_ABORT;
}

static PrimopRes primopIdentical(State* state) {
    ORef const x = state->regs[firstArgReg];
    ORef const y = state->regs[firstArgReg + 1];

    state->regs[retReg] = boolToORef(tagBool(eq(x, y)));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxAdd(State* state) {
    ORef const xRef = state->regs[firstArgReg];
    ORef const yRef = state->regs[firstArgReg + 1];

    if (!isFixnum(xRef)) { return primopTypeError(state, state->fixnumType, xRef); }
    intptr_t const x = uncheckedFixnumToInt(xRef);
    if (!isFixnum(yRef)) { return primopTypeError(state, state->fixnumType, yRef); }
    intptr_t const y = uncheckedFixnumToInt(yRef);

    state->regs[retReg] = fixnumToORef(tagInt(x + y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxSub(State* state) {
    ORef const xRef = state->regs[firstArgReg];
    ORef const yRef = state->regs[firstArgReg + 1];

    if (!isFixnum(xRef)) { return primopTypeError(state, state->fixnumType, xRef); }
    intptr_t const x = uncheckedFixnumToInt(xRef);
    if (!isFixnum(yRef)) { return primopTypeError(state, state->fixnumType, yRef); }
    intptr_t const y = uncheckedFixnumToInt(yRef);

    state->regs[retReg] = fixnumToORef(tagInt(x - y)); // TODO: Underflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxMul(State* state) {
    ORef const xRef = state->regs[firstArgReg];
    ORef const yRef = state->regs[firstArgReg + 1];

    if (!isFixnum(xRef)) { return primopTypeError(state, state->fixnumType, xRef); }
    intptr_t const x = uncheckedFixnumToInt(xRef);
    if (!isFixnum(yRef)) { return primopTypeError(state, state->fixnumType, yRef); }
    intptr_t const y = uncheckedFixnumToInt(yRef);

    state->regs[retReg] = fixnumToORef(tagInt(x * y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}
