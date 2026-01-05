#include "dispatch.h"

#include "bytecode.h"
#include "primops.h"

[[nodiscard]]
static ORef doCheckDomain(
    State* state, ClosureRef calleeRef, ORef const* args, size_t argc
) {
    Closure const* const callee = closureToPtr(calleeRef);
    assert(isMethod(state, callee->method));
    MethodRef const methodRef = uncheckedORefToMethod(callee->method);
    Method const* const method = methodToPtr(methodRef);
    size_t const arity = (uintptr_t)fixnumToInt(uncheckedFlexCount(toORef(methodRef)));

    if (argc != arity) {
        if (!(unwrapBool(method->hasVarArg) && argc >= arity - 1)) {
            return toORef(createArityError(state, calleeRef, tagInt((intptr_t)argc)));
        }
    }

    // OPTIMIZE: Skip these loops if no typed params (= not a specialization):

    bool const hasVarArg = unwrapBool(method->hasVarArg);
    size_t const minArity = !hasVarArg ? arity : arity - 1;

    for (size_t i = 0; i < minArity; ++i) {
        assert(isType(state, method->domain[i]));
        TypeRef const type = uncheckedORefToType(method->domain[i]);
        ORef const v = args[i];
        if (!isa(state, type, v)) {
            return toORef(createTypeError(state, type, v));
        }
    }

    if (hasVarArg) {
        assert(isType(state, method->domain[minArity]));
        TypeRef const type = uncheckedORefToType(method->domain[minArity]);
        for (size_t i = minArity; i < argc; ++i) {
            ORef const v = args[i];
            if (!isa(state, type, v)) {
                return toORef(createTypeError(state, type, v));
            }
        }
    }

    return toORef(Zero);
}

// TODO: Can we somehow (efficiently!) DRY this wrt. `doCheckDomain`?
[[nodiscard]]
static bool closureIsApplicable(
    State const* state, Closure const* callee, ORef const* args, size_t argc
) {
    assert(isMethod(state, callee->method));
    MethodRef const methodRef = uncheckedORefToMethod(callee->method);
    Method const* const method = methodToPtr(methodRef);
    size_t const arity = (uintptr_t)fixnumToInt(uncheckedFlexCount(toORef(methodRef)));

    if (argc != arity) {
        if (!(unwrapBool(method->hasVarArg) && argc >= arity - 1)) {
            return false;
        }
    }

    // OPTIMIZE: Skip these loops if no typed params (= not a specialization):

    bool const hasVarArg = unwrapBool(method->hasVarArg);
    size_t const minArity = !hasVarArg ? arity : arity - 1;

    // Fixed args:
    for (size_t i = 0; i < minArity; ++i) {
        assert(isType(state, method->domain[i]));
        TypeRef const type = uncheckedORefToType(method->domain[i]);
        ORef const v = args[i];
        if (!isa(state, type, v)) {
            return false;
        }
    }

    if (hasVarArg) { // Vararg:
        assert(isType(state, method->domain[minArity]));
        TypeRef const type = uncheckedORefToType(method->domain[minArity]);
        for (size_t i = minArity; i < argc; ++i) {
            ORef const v = args[i];
            if (!isa(state, type, v)) {
                return false;
            }
        }
    }

    return true;
}

// TODO: DRY wrt. `closureIsApplicable`:
[[nodiscard]]
static bool closureIsApplicableToList(State const* state, Closure const* callee, ORef args) {
    assert(isMethod(state, callee->method));
    MethodRef const methodRef = uncheckedORefToMethod(callee->method);
    Method const* const method = methodToPtr(methodRef);
    size_t const arity = (uintptr_t)fixnumToInt(uncheckedFlexCount(toORef(methodRef)));

    bool const hasVarArg = unwrapBool(method->hasVarArg);
    size_t const minArity = !hasVarArg ? arity : arity - 1;

    // Fixed args:
    for (size_t i = 0; i < minArity; ++i) {
        if (isPair(state, args)) {
            Pair const* const argsPair = toPtr(uncheckedORefToPair(args));

            assert(isType(state, method->domain[i]));
            TypeRef const type = uncheckedORefToType(method->domain[i]);
            ORef const v = argsPair->car;
            if (!isa(state, type, v)) {
                return false;
            }

            args = argsPair->cdr;
        } else if (isEmptyList(state, args)) {
            return false; // Insufficient argc
        } else {
            assert(false); // TODO: Proper improper args error
        }
    }

    if (hasVarArg) { // Vararg:
        assert(isType(state, method->domain[minArity]));
        TypeRef const type = uncheckedORefToType(method->domain[minArity]);
        for (;/*ever*/;) {
            if (isPair(state, args)) {
                Pair const* const argsPair = toPtr(uncheckedORefToPair(args));

                ORef const v = argsPair->car;
                if (!isa(state, type, v)) {
                    return false;
                }

                args = argsPair->cdr;
            } else if (isEmptyList(state, args)) {
                break;
            } else {
                assert(false); // TODO: Proper improper args error
            }
        }
    }

    if (!isEmptyList(state, args)) {
        return false; // Excessive argc
    }

    return true;
}

[[nodiscard]]
static ORef checkDomainForArgs(
    State* state, ClosureRef calleeRef, ORef const* args, size_t argc
) {
    if (!state->checkDomain) {
        state->checkDomain = true;
        return toORef(Zero);
    }

    return doCheckDomain(state, calleeRef, args, argc);
}

[[nodiscard]]
static ORef checkDomain(State* state) {
    if (!state->checkDomain) {
        state->checkDomain = true;
        return toORef(Zero);
    }

    assert(isClosure(state, state->regs[calleeReg]));
    ClosureRef const calleeRef = uncheckedORefToClosure(state->regs[calleeReg]);
    ORef const* const args = state->regs + firstArgReg;
    size_t const argc = state->entryRegc - firstArgReg;
    return doCheckDomain(state, calleeRef, args, argc);
}

/// Returns applicable closure from `callee`, `Zero` if none is found.
static ORef applicableClosureForArgs(
    State* state, Multimethod const* callee, ORef const* args, size_t argc
) {
    ArrayRef const methodsRef = callee->methods;
    ORef const* const methods = toPtr(methodsRef);

    size_t const methodCount = (uintptr_t)fixnumToInt(arrayCount(methodsRef));
    for (size_t i = 0; i < methodCount; ++i) {
        assert(isa(state, state->closureType, (methods[i])));
        ClosureRef const methodRef = uncheckedORefToClosure(methods[i]);

        if (closureIsApplicable(state, toPtr(methodRef), args, argc)) {
            state->checkDomain = false;
            return toORef(methodRef);
        }
    }

    return toORef(Zero);
}

// TODO: DRY wrt. `applicableClosureForArgs`:
/// Returns applicable closure from `callee`, `Zero` if none is found.
static ORef applicableClosureForArglist(State* state, Multimethod const* callee, ORef args) {
    ArrayRef const methodsRef = callee->methods;
    ORef const* const methods = toPtr(methodsRef);

    size_t const methodCount = (uintptr_t)fixnumToInt(arrayCount(methodsRef));
    for (size_t i = 0; i < methodCount; ++i) {
        assert(isa(state, state->closureType, (methods[i])));
        ClosureRef const methodRef = uncheckedORefToClosure(methods[i]);

        if (closureIsApplicableToList(state, toPtr(methodRef), args)) {
            state->checkDomain = false;
            return toORef(methodRef);
        }
    }

    return toORef(Zero);
}

static bool calleeClosureForArgs(State* state, ORef callee, ORef const* args, size_t argc) {
    // TODO: Make continuations directly callable?
    // TODO: Make this extensible (à la JVM `invokedynamic`)?:

    if (isClosure(state, callee)) {
        state->regs[calleeReg] = callee;
        return true;
    } else if (isMultimethod(state, callee)) {
        MultimethodRef const multiCalleeRef = uncheckedORefToMultimethod(callee);

        ORef const maybeClosure =
            applicableClosureForArgs(state, toPtr(multiCalleeRef), args, argc);
        if (isHeaped(maybeClosure)) {
            state->regs[calleeReg] = maybeClosure;
            return true;
        } else {
            state->regs[calleeReg] = getErrorHandler(state);
            state->regs[firstArgReg] =
                toORef(createInapplicableError(state, multiCalleeRef));
            state->entryRegc = firstArgReg + 1;

            assert(isClosure(state, state->regs[calleeReg]));
            return false;
        }
    } else { // TODO: DRY with "inapplicable" directly above:
        state->regs[calleeReg] = getErrorHandler(state);
        // TODO: `UncallableError` as closure is no longer the only callable type:
        state->regs[firstArgReg] = toORef(createTypeError(state, state->closureType, callee));
        state->entryRegc = firstArgReg + 1;

        assert(isClosure(state, state->regs[calleeReg]));
        return false;
    }
}

// TODO: DRY wrt. `calleeClosureForArgs`:
static bool calleeClosureForArglist(State* state, ORef callee, ORef args) {
    // TODO: Make continuations directly callable?
    // TODO: Make this extensible (à la JVM `invokedynamic`)?:

    if (isClosure(state, callee)) {
        state->regs[calleeReg] = callee;
        return true;
    } else if (isMultimethod(state, callee)) {
        MultimethodRef const multiCalleeRef = uncheckedORefToMultimethod(callee);

        ORef const maybeClosure =
            applicableClosureForArglist(state, toPtr(multiCalleeRef), args);
        if (isHeaped(maybeClosure)) {
            state->regs[calleeReg] = maybeClosure;
            return true;
        } else {
            state->regs[calleeReg] = getErrorHandler(state);
            state->regs[firstArgReg] =
                toORef(createInapplicableError(state, multiCalleeRef));
            state->entryRegc = firstArgReg + 1;

            assert(isClosure(state, state->regs[calleeReg]));
            return false;
        }
    } else { // TODO: DRY with "inapplicable" directly above:
        state->regs[calleeReg] = getErrorHandler(state);
        // TODO: `UncallableError` as closure is no longer the only callable type:
        state->regs[firstArgReg] = toORef(createTypeError(state, state->closureType, callee));
        state->entryRegc = firstArgReg + 1;

        assert(isClosure(state, state->regs[calleeReg]));
        return false;
    }
}

static bool calleeClosure(State* state, ORef callee) {
    ORef const* const args = state->regs + firstArgReg;
    size_t const argc = state->entryRegc - firstArgReg;
    return calleeClosureForArgs(state, callee, args, argc);
}
