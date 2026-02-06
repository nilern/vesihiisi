#include "dispatch.hpp"

#include "bytecode.hpp"
#include "primops.hpp"

namespace {

[[nodiscard]]
ORef doCheckDomain(
    State* state, HRef<Closure> callee, ORef const* args, size_t argc
) {
    assert(isa<Method>(*state, callee->method));
    HRef<Method> const method = HRef<Method>::fromUnchecked(callee->method);
    size_t const arity = (uint64_t)method->flexCount().val();
    bool const hasVarArg = method->hasVarArg.val();

    if (argc != arity) {
        if (!(hasVarArg && argc >= arity - 1)) {
            return createArityError(state, callee, Fixnum((intptr_t)argc));
        }
    }

    // OPTIMIZE: Skip these loops if no typed params (= not a specialization):

    size_t const minArity = !hasVarArg ? arity : arity - 1;

    for (size_t i = 0; i < minArity; ++i) {
        assert(isa<Type>(*state, method->domain()[i].get()));
        HRef<Type> const type = HRef<Type>::fromUnchecked(method->domain()[i].get());
        ORef const v = args[i];
        if (!isa(state, type, v)) {
            return createTypeError(state, type, v);
        }
    }

    if (hasVarArg) {
        assert(isa<Type>(*state, method->domain()[minArity].get()));
        HRef<Type> const type = HRef<Type>::fromUnchecked(method->domain()[minArity].get());
        for (size_t i = minArity; i < argc; ++i) {
            ORef const v = args[i];
            if (!isa(state, type, v)) {
                return createTypeError(state, type, v);
            }
        }
    }

    return Default;
}

// TODO: Can we somehow (efficiently!) DRY this wrt. `doCheckDomain`?
[[nodiscard]]
bool closureIsApplicable(
    State const* state, Closure const* callee, ORef const* args, size_t argc
) {
    assert(isa<Method>(*state, callee->method));
    HRef<Method> const method = HRef<Method>::fromUnchecked(callee->method);
    size_t const arity = (uint64_t)method->flexCount().val();
    bool const hasVarArg = method->hasVarArg.val();

    if (argc != arity) {
        if (!(hasVarArg && argc >= arity - 1)) {
            return false;
        }
    }

    // OPTIMIZE: Skip these loops if no typed params (= not a specialization):

    size_t const minArity = !hasVarArg ? arity : arity - 1;

    // Fixed args:
    for (size_t i = 0; i < minArity; ++i) {
        assert(isa<Type>(*state, method->domain()[i].get()));
        HRef<Type> const type = HRef<Type>::fromUnchecked(method->domain()[i].get());
        ORef const v = args[i];
        if (!isa(state, type, v)) {
            return false;
        }
    }

    if (hasVarArg) { // Vararg:
        assert(isa<Type>(*state, method->domain()[minArity].get()));
        HRef<Type> const type = HRef<Type>::fromUnchecked(method->domain()[minArity].get());
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
bool closureIsApplicableToList(State const* state, Closure const* callee, ORef args) {
    assert(isa<Method>(*state, callee->method));
    HRef<Method> const method = HRef<Method>::fromUnchecked(callee->method);
    size_t const arity = (uint64_t)method->flexCount().val();

    bool const hasVarArg = method->hasVarArg.val();
    size_t const minArity = !hasVarArg ? arity : arity - 1;

    // Fixed args:
    for (size_t i = 0; i < minArity; ++i) {
        if (isa<Pair>(*state, args)) {
            auto const argsPair = HRef<Pair>::fromUnchecked(args);

            assert(isa<Type>(*state, method->domain()[i].get()));
            HRef<Type> const type = HRef<Type>::fromUnchecked(method->domain()[i].get());
            ORef const v = argsPair->car().get();
            if (!isa(state, type, v)) {
                return false;
            }

            args = argsPair->cdr().get();
        } else if (isEmptyList(state, args)) {
            return false; // Insufficient argc
        } else {
            assert(false); // TODO: Proper improper args error
        }
    }

    if (hasVarArg) { // Vararg:
        assert(isa<Type>(*state, method->domain()[minArity].get()));
        HRef<Type> const type = HRef<Type>::fromUnchecked(method->domain()[minArity].get());
        for (;/*ever*/;) {
            if (isa<Pair>(*state, args)) {
                auto const argsPair = HRef<Pair>::fromUnchecked(args);

                ORef const v = argsPair->car().get();
                if (!isa(state, type, v)) {
                    return false;
                }

                args = argsPair->cdr().get();
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
ORef checkDomainForArgs(
    State* state, HRef<Closure> calleeRef, ORef const* args, size_t argc
) {
    if (state->domainChecking != State::DomainChecking::CHECK) {
        state->domainChecking = State::DomainChecking::CHECK;
        return Default;
    }

    return doCheckDomain(state, calleeRef, args, argc);
}

[[nodiscard]]
ORef checkDomain(State* state) {
    if (state->domainChecking != State::DomainChecking::CHECK) {
        state->domainChecking = State::DomainChecking::CHECK;
        return Default;
    }

    assert(isa<Closure>(*state, state->regs[calleeReg]));
    HRef<Closure> const calleeRef = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
    ORef const* const args = state->regs + firstArgReg;
    size_t const argc = state->entryRegc - firstArgReg;
    return doCheckDomain(state, calleeRef, args, argc);
}

/// Returns applicable closure from `callee`, `Default` if none is found.
ORef applicableClosureForArgs(
    State* state, Multimethod const* callee, ORef const* args, size_t argc
) {
    HRef<Array> const methodsRef = callee->methods().get();
    ORefSpan const methods = methodsRef->flexItems();

    size_t const methodCount = (uint64_t)methodsRef->flexCount().val();
    for (size_t i = 0; i < methodCount; ++i) {
        assert(isa(state, state->types.closure, (methods[i])));
        auto const methodRef = HRef<Closure>::fromUnchecked(methods[i]);

        if (closureIsApplicable(state, &*methodRef, args, argc)) {
            state->domainChecking = State::DomainChecking::SKIP;
            return methodRef;
        }
    }

    return Default;
}

// TODO: DRY wrt. `applicableClosureForArgs`:
/// Returns applicable closure from `callee`, `Default` if none is found.
ORef applicableClosureForArglist(State* state, Multimethod const* callee, ORef args) {
    HRef<Array> const methodsRef = callee->methods().get();
    ORefSpan const methods = methodsRef->flexItems();

    size_t const methodCount = (uint64_t)methodsRef->flexCount().val();
    for (size_t i = 0; i < methodCount; ++i) {
        assert(isa(state, state->types.closure, (methods[i])));
        HRef<Closure> const methodRef = HRef<Closure>::fromUnchecked(methods[i]);

        if (closureIsApplicableToList(state, &*methodRef, args)) {
            state->domainChecking = State::DomainChecking::SKIP;
            return methodRef;
        }
    }

    return Default;
}

bool calleeClosureForArgs(State* state, ORef callee, ORef const* args, size_t argc) {
    // TODO: Make continuations directly callable?
    // TODO: Make this extensible (à la JVM `invokedynamic`)?:

    if (isa<Closure>(*state, callee)) {
        state->regs[calleeReg] = callee;
        return true;
    } else if (isa<Multimethod>(*state, callee)) {
        HRef<Multimethod> const multiCalleeRef = HRef<Multimethod>::fromUnchecked(callee);

        ORef const maybeClosure =
            applicableClosureForArgs(state, &*multiCalleeRef, args, argc);
        if (isHeaped(maybeClosure)) {
            state->regs[calleeReg] = maybeClosure;
            return true;
        } else {
            state->regs[calleeReg] = getErrorHandler(state);
            state->regs[firstArgReg] = createInapplicableError(state, multiCalleeRef);
            state->entryRegc = firstArgReg + 1;

            assert(isa<Closure>(*state, state->regs[calleeReg]));
            return false;
        }
    } else { // TODO: DRY with "inapplicable" directly above:
        state->regs[calleeReg] = getErrorHandler(state);
        // TODO: `UncallableError` as closure is no longer the only callable type:
        state->regs[firstArgReg] = createTypeError(state, state->types.closure, callee);
        state->entryRegc = firstArgReg + 1;

        assert(isa<Closure>(*state, state->regs[calleeReg]));
        return false;
    }
}

// TODO: DRY wrt. `calleeClosureForArgs`:
bool calleeClosureForArglist(State* state, ORef callee, ORef args) {
    // TODO: Make continuations directly callable?
    // TODO: Make this extensible (à la JVM `invokedynamic`)?:

    if (isa<Closure>(*state, callee)) {
        state->regs[calleeReg] = callee;
        return true;
    } else if (isa<Multimethod>(*state, callee)) {
        HRef<Multimethod> const multiCalleeRef = HRef<Multimethod>::fromUnchecked(callee);

        ORef const maybeClosure =
            applicableClosureForArglist(state, &*multiCalleeRef, args);
        if (isHeaped(maybeClosure)) {
            state->regs[calleeReg] = maybeClosure;
            return true;
        } else {
            state->regs[calleeReg] = getErrorHandler(state);
            state->regs[firstArgReg] = createInapplicableError(state, multiCalleeRef);
            state->entryRegc = firstArgReg + 1;

            assert(isa<Closure>(*state, state->regs[calleeReg]));
            return false;
        }
    } else { // TODO: DRY with "inapplicable" directly above:
        state->regs[calleeReg] = getErrorHandler(state);
        // TODO: `UncallableError` as closure is no longer the only callable type:
        state->regs[firstArgReg] = createTypeError(state, state->types.closure, callee);
        state->entryRegc = firstArgReg + 1;

        assert(isa<Closure>(*state, state->regs[calleeReg]));
        return false;
    }
}

bool calleeClosure(State* state, ORef callee) {
    ORef const* const args = state->regs + firstArgReg;
    size_t const argc = state->entryRegc - firstArgReg;
    return calleeClosureForArgs(state, callee, args, argc);
}

} // namespace
