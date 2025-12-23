#include "primops.h"

#include <stdio.h>

#include "bytecode.h"
#include "print.h"

static ORef getErrorHandler(State const* state) {
    ORef const v = varToPtr(state->errorHandler)->val;
    if (eq(v, unboundToORef(state->unbound))) {
        exit(EXIT_FAILURE); // FIXME
    }

    return v;
}

[[nodiscard]]
static PrimopRes primopError(State* state, ORef err) {
    state->regs[calleeReg] = getErrorHandler(state);
    state->regs[firstArgReg] = err;
    state->entryRegc = 3;
    return PRIMOP_RES_TAILCALL;
}

// OPTIMIZE: We know the domain of primops at VM compile time so going through this is suboptimal:
[[nodiscard]]
static ORef checkDomain(State* state) {
    assert(isClosure(state, state->regs[calleeReg]));
    ClosureRef const calleeRef = uncheckedORefToClosure(state->regs[calleeReg]);
    Closure const* const callee = closureToPtr(calleeRef);
    assert(isMethod(state, callee->method));
    MethodRef const methodRef = uncheckedORefToMethod(callee->method);
    Method const* const method = methodToPtr(methodRef);
    size_t const arity = (uintptr_t)fixnumToInt(flexLength(methodToORef(methodRef)));

    uint8_t const callArity = state->entryRegc - firstArgReg;
    if (callArity != arity) {
        if (!(unwrapBool(method->hasVarArg) && callArity >= arity - 1)) {
            return arityErrorToORef(createArityError(state, calleeRef, tagInt(callArity)));
        }
    }

    bool const hasVarArg = unwrapBool(method->hasVarArg);
    size_t const minArity = !hasVarArg ? arity : arity - 1;

    for (size_t i = 0; i < minArity; ++i) {
        TypeRef const type = method->domain[i];
        ORef const v = state->regs[firstArgReg + i];
        if (!isa(state, type, v)) {
            return typeErrorToORef(createTypeError(state, type, v));
        }
    }

    if (hasVarArg) {
        TypeRef const type = method->domain[minArity];
        for (size_t i = minArity; i < callArity; ++i) {
            ORef const v = state->regs[firstArgReg + i];
            if (!isa(state, type, v)) {
                return typeErrorToORef(createTypeError(state, type, v));
            }
        }
    }

    return fixnumToORef(Zero);
}

static PrimopRes callBytecode(State* /*state*/) { return PRIMOP_RES_TAILCALL; }

static PrimopRes primopAbort(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const error = state->regs[firstArgReg];

    fputs("Runtime error: ", stderr);
    print(state, stderr, error);
    putc('\n', stderr);

    return PRIMOP_RES_ABORT;
}

static PrimopRes primopIdentical(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const x = state->regs[firstArgReg];
    ORef const y = state->regs[firstArgReg + 1];

    state->regs[retReg] = boolToORef(tagBool(eq(x, y)));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxAdd(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    state->regs[retReg] = fixnumToORef(tagInt(x + y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxSub(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    state->regs[retReg] = fixnumToORef(tagInt(x - y)); // TODO: Underflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxMul(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    state->regs[retReg] = fixnumToORef(tagInt(x * y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxDiv(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    if (y == 0) { assert(false); } // TODO: Proper error
    state->regs[retReg] = fixnumToORef(tagInt(x / y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}
