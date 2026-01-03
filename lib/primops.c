#include "primops.h"

#include <stdlib.h>
#include <stdio.h>

#include "bytecode.h"

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
    state->entryRegc = firstArgReg + 1;
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
        assert(isType(state, method->domain[i]));
        TypeRef const type = uncheckedORefToTypeRef(method->domain[i]);
        ORef const v = state->regs[firstArgReg + i];
        if (!isa(state, type, v)) {
            return typeErrorToORef(createTypeError(state, type, v));
        }
    }

    if (hasVarArg) {
        assert(isType(state, method->domain[minArity]));
        TypeRef const type = uncheckedORefToTypeRef(method->domain[minArity]);
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

static PrimopRes primopCallCC(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[calleeReg] = state->regs[firstArgReg];
    state->regs[firstArgReg] = state->regs[retContReg];
    state->entryRegc = firstArgReg + 1;
    return PRIMOP_RES_TAILCALL;
}

static PrimopRes primopContinue(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[retContReg] = state->regs[firstArgReg];
    state->regs[retReg] = state->regs[firstArgReg + 1];
    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopIdentical(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const x = state->regs[firstArgReg];
    ORef const y = state->regs[firstArgReg + 1];

    state->regs[retReg] = boolToORef(tagBool(eq(x, y)));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopMake(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    TypeRef typeRef = uncheckedORefToTypeRef(state->regs[firstArgReg]);
    uint8_t const callArity = state->entryRegc - firstArgReg;

    Type const* type = toPtr(typeRef);
    if (!unwrapBool(type->isFlex)) {
        // Alloc:
        void* ptr = tryAlloc(&state->heap.tospace, type);
        if (mustCollect(ptr)) {
            collect(state);
            typeRef = uncheckedORefToTypeRef(state->regs[firstArgReg]);
            type = toPtr(typeRef);
            ptr = allocOrDie(&state->heap.tospace, type);
        }

        // Init:
        if (!unwrapBool(type->isBytes)) {
            size_t const fieldCount = (uintptr_t)fixnumToInt(type->minSize) / sizeof(ORef);
            if (callArity - 1u != fieldCount) {
                exit(EXIT_FAILURE); // TODO: Proper error (but not really an arity error!)
            }

            {
                ORef* const fields = (ORef*)ptr;
                for (size_t i = 0; i < fieldCount; ++i) {
                    fields[i] = state->regs[firstArgReg + 1 + i];
                }
            }
        } else {
            exit(EXIT_FAILURE); // TODO:
        }

        state->regs[retReg] = tagHeaped(ptr);

        return PRIMOP_RES_CONTINUE;
    } else {
        exit(EXIT_FAILURE); // TODO
    }
}

static PrimopRes primopSlotGet(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];
    size_t const slotIdx = (uintptr_t)uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    Type const* type = toPtr(typeOf(state, v));
    if (!unwrapBool(type->isBytes)) {
        size_t const slotCount = (uintptr_t)fixnumToInt(type->minSize) / sizeof(ORef);
        if (slotIdx >= slotCount) {
            assert(false); // TODO: Proper bounds error
        }

        ORef const* const slots = (ORef const*)uncheckedORefToPtr(v);
        state->regs[retReg] = slots[slotIdx];
    } else {
        assert(false); // TODO
    }

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFlexCount(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];

    Type const* type = toPtr(typeOf(state, v));
    if (!unwrapBool(type->isFlex)) {
        assert(false); // TODO: Proper nonflex error
    }

    state->regs[retReg] = toORef(((FlexHeader const*)uncheckedORefToPtr(v) - 1)->length);

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFlexGet(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];
    intptr_t const i = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    Type const* type = toPtr(typeOf(state, v));
    if (!unwrapBool(type->isFlex)) {
        assert(false); // TODO: Proper nonflex error
    }
    if (unwrapBool(type->isBytes)) {
        assert(false); // TODO: Proper nonslots error
    }

    void const* const ptr = uncheckedORefToPtr(v);
    intptr_t const count = fixnumToInt(((FlexHeader const*)ptr - 1)->length);
    if (i < 0 || i >= count) {
        assert(false); // TODO: Proper bounds error
    }

    ORef const* const flexSlots = (ORef const*)((char const*)ptr + fixnumToInt(type->minSize));
    state->regs[retReg] = flexSlots[i];

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

static PrimopRes primopFxQuot(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    if (y == 0) { assert(false); } // TODO: Proper error
    state->regs[retReg] = fixnumToORef(tagInt(x / y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxLt(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    state->regs[retReg] = toORef(tagBool(x < y));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopWrite(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    print(state, stdout, state->regs[firstArgReg]);

    return PRIMOP_RES_CONTINUE; // TODO: Maybe do not return written value?
}
