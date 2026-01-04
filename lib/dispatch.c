#include "dispatch.h"

#include "bytecode.h"

[[nodiscard]]
static ORef checkDomain(State* state) {
    assert(isClosure(state, state->regs[calleeReg]));
    ClosureRef const calleeRef = uncheckedORefToClosure(state->regs[calleeReg]);
    Closure const* const callee = closureToPtr(calleeRef);
    assert(isMethod(state, callee->method));
    MethodRef const methodRef = uncheckedORefToMethod(callee->method);
    Method const* const method = methodToPtr(methodRef);
    size_t const arity = (uintptr_t)fixnumToInt(uncheckedFlexCount(methodToORef(methodRef)));

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

// TODO: Can we somehow (efficiently!) DRY this wrt. `checkDomain`?
[[nodiscard]]
static bool closureIsApplicable(State const* state, Closure const* callee) {
    assert(isMethod(state, callee->method));
    MethodRef const methodRef = uncheckedORefToMethod(callee->method);
    Method const* const method = methodToPtr(methodRef);
    size_t const arity = (uintptr_t)fixnumToInt(uncheckedFlexCount(methodToORef(methodRef)));

    uint8_t const callArity = state->entryRegc - firstArgReg;
    if (callArity != arity) {
        if (!(unwrapBool(method->hasVarArg) && callArity >= arity - 1)) {
            return false;
        }
    }

    bool const hasVarArg = unwrapBool(method->hasVarArg);
    size_t const minArity = !hasVarArg ? arity : arity - 1;

    for (size_t i = 0; i < minArity; ++i) {
        assert(isType(state, method->domain[i]));
        TypeRef const type = uncheckedORefToTypeRef(method->domain[i]);
        ORef const v = state->regs[firstArgReg + i];
        if (!isa(state, type, v)) {
            return false;
        }
    }

    if (hasVarArg) {
        assert(isType(state, method->domain[minArity]));
        TypeRef const type = uncheckedORefToTypeRef(method->domain[minArity]);
        for (size_t i = minArity; i < callArity; ++i) {
            ORef const v = state->regs[firstArgReg + i];
            if (!isa(state, type, v)) {
                return false;
            }
        }
    }

    return true;
}

static ORef applicableClosure(State const* state, Multimethod const* callee) {
    ArrayRef const methodsRef = callee->methods;
    ORef const* const methods = toPtr(methodsRef);

    size_t const methodCount = (uintptr_t)fixnumToInt(arrayCount(methodsRef));
    for (size_t i = 0; i < methodCount; ++i) {
        assert(isa(state, state->closureType, (methods[i])));
        ClosureRef const methodRef = uncheckedORefToClosure(methods[i]);
        Closure const* const method = toPtr(methodRef);

        if (closureIsApplicable(state, method)) {
            return toORef(methodRef);
        }
    }

    return toORef(Zero);
}
