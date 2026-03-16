#include "dispatch.hpp"

#include "bytecode.hpp"
#include "primops.hpp"

namespace {

[[nodiscard]]
PrimopRes doCheckDomainWithPrejudice(
    RT* state, HRef<Closure> callee, ORef const* args, size_t argc
) {
    assert(isa<Method>(*state, callee->method));
    HRef<Method> const method = HRef<Method>::fromUnchecked(callee->method);
    size_t const arity = (uint64_t)method->flexCount().val();
    bool const hasVarArg = method->hasVarArg.val();

    if (argc != arity) {
        if (!(hasVarArg && argc >= arity - 1)) {
            return primopArityError(state, callee, argc);
        }
    }

    // OPTIMIZE: Skip these loops if no typed params (= not a specialization):

    size_t const minArity = !hasVarArg ? arity : arity - 1;

    for (size_t i = 0; i < minArity; ++i) {
        assert(isa<Type>(*state, method->domain()[i].get()));
        HRef<Type> const type = HRef<Type>::fromUnchecked(method->domain()[i].get());
        ORef const v = args[i];
        if (!isa(state, type, v)) {
            return primopTypeError(state, type, v);
        }
    }

    if (hasVarArg) {
        assert(isa<Type>(*state, method->domain()[minArity].get()));
        HRef<Type> const type = HRef<Type>::fromUnchecked(method->domain()[minArity].get());
        for (size_t i = minArity; i < argc; ++i) {
            ORef const v = args[i];
            if (!isa(state, type, v)) {
                return primopTypeError(state, type, v);
            }
        }
    }

    return PrimopRes::CONTINUE; // HACK
}

// TODO: Can we somehow (efficiently!) DRY this wrt. `doCheckDomain`?
[[nodiscard]]
bool closureIsApplicable(
    RT const* state, Closure const* callee, ORef const* args, size_t argc
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
bool closureIsApplicableToList(RT const* state, Closure const* callee, ORef args) {
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

DomainCheckRes doCheckDomain(RT* rt, HRef<Closure> calleeRef, ORef const* args, size_t argc) {
    switch (rt->domainChecking) {
    case RT::DomainChecking::CHECK: {
        if (doCheckDomainWithPrejudice(rt, calleeRef, args, argc) == PrimopRes::ERROR) {
            return DomainCheckRes::ERROR;
        }
    }; break;

    case RT::DomainChecking::SPECULATE: {
        rt->domainChecking = RT::DomainChecking::CHECK;

        if (!closureIsApplicable(rt, &*calleeRef, args, argc)) {
            return DomainCheckRes::MISSPECULATION;
        }
    }; break;

    case RT::DomainChecking::SKIP: PANIC("Unreachable code reached.");
    }

    return DomainCheckRes::OK;
}

DomainCheckRes checkDomainForArgs(RT* rt, HRef<Closure> calleeRef, ORef const* args, size_t argc) {
    if (rt->domainChecking == RT::DomainChecking::SKIP) {
        rt->domainChecking = RT::DomainChecking::CHECK;
        return DomainCheckRes::OK;
    }

    return doCheckDomain(rt, calleeRef, args, argc);
}

DomainCheckRes checkDomain(RT* rt) {
    if (rt->domainChecking == RT::DomainChecking::SKIP) {
        rt->domainChecking = RT::DomainChecking::CHECK;
        return DomainCheckRes::OK;
    }

    assert(isa<Closure>(*rt, rt->regs[calleeReg]));
    HRef<Closure> const calleeRef = HRef<Closure>::fromUnchecked(rt->regs[calleeReg]);
    ORef const* const args = rt->regs + firstArgReg;
    size_t const argc = rt->entryRegc - firstArgReg;
    return doCheckDomain(rt, calleeRef, args, argc);
}

/// Returns applicable closure from `callee`, `Default` if none is found.
ORef applicableClosureForArgs(
    RT* state, Multimethod const* callee, ORef const* args, size_t argc
) {
    HRef<Array> const methodsRef = callee->methods().get();
    ORefSpan const methods = methodsRef->flexItems();

    size_t const methodCount = (uint64_t)methodsRef->flexCount().val();
    for (size_t i = 0; i < methodCount; ++i) {
        assert(isa(state, state->types.closure, (methods[i])));
        auto const methodRef = HRef<Closure>::fromUnchecked(methods[i]);

        if (closureIsApplicable(state, &*methodRef, args, argc)) {
            state->domainChecking = RT::DomainChecking::SKIP;
            return methodRef;
        }
    }

    return Default;
}

// TODO: DRY wrt. `applicableClosureForArgs`:
/// Returns applicable closure from `callee`, `Default` if none is found.
ORef applicableClosureForArglist(RT* state, Multimethod const* callee, ORef args) {
    HRef<Array> const methodsRef = callee->methods().get();
    ORefSpan const methods = methodsRef->flexItems();

    size_t const methodCount = (uint64_t)methodsRef->flexCount().val();
    for (size_t i = 0; i < methodCount; ++i) {
        assert(isa(state, state->types.closure, (methods[i])));
        HRef<Closure> const methodRef = HRef<Closure>::fromUnchecked(methods[i]);

        if (closureIsApplicableToList(state, &*methodRef, args)) {
            state->domainChecking = RT::DomainChecking::SKIP;
            return methodRef;
        }
    }

    return Default;
}

bool calleeClosureForArgs(RT* state, ORef callee, ORef const* args, size_t argc) {
    // TODO: Make continuations directly callable?
    // TODO: Make this extensible (à la JVM `invokedynamic`)?:

    if (isa<Closure>(*state, callee)) {
        state->regs[calleeReg] = callee;
        return true;
    } else if (isa<Multimethod>(*state, callee)) {
        HRef<Multimethod> const multiCalleeRef = HRef<Multimethod>::fromUnchecked(callee);

        ORef const maybeClosure = applicableClosureForArgs(state, &*multiCalleeRef, args, argc);
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
bool calleeClosureForArglist(RT* state, ORef callee, ORef args) {
    // TODO: Make continuations directly callable?
    // TODO: Make this extensible (à la JVM `invokedynamic`)?:

    if (isa<Closure>(*state, callee)) {
        state->regs[calleeReg] = callee;
        return true;
    } else if (isa<Multimethod>(*state, callee)) {
        HRef<Multimethod> const multiCalleeRef = HRef<Multimethod>::fromUnchecked(callee);

        ORef const maybeClosure = applicableClosureForArglist(state, &*multiCalleeRef, args);
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
bool calleeClosure(RT* state, ORef callee, std::optional<uint8_t> inlineCacheIdx) {
    // TODO: Make continuations directly callable?
    // TODO: Make this extensible (à la JVM `invokedynamic`)?:

    if (isa<Closure>(*state, callee)) {
        state->regs[calleeReg] = callee;
        return true;
    } else if (isa<Multimethod>(*state, callee)) {
        HRef<Multimethod> const multiCalleeRef = HRef<Multimethod>::fromUnchecked(callee);

        if (inlineCacheIdx) {
            if (eq(state->consts[*inlineCacheIdx].get(), multiCalleeRef->methods().get())) {
                state->regs[calleeReg] = state->consts[*inlineCacheIdx + 1].get();
                state->domainChecking = RT::DomainChecking::SPECULATE;

                assert(isa<Closure>(*state, state->regs[calleeReg]));
                return true;
            } else {
                state->consts[*inlineCacheIdx].set(*state, Default);
                state->consts[*inlineCacheIdx + 1].set(*state, Default);
            }
        }

        ORef const* const args = state->regs + firstArgReg;
        size_t const argc = state->entryRegc - firstArgReg;
        ORef const maybeClosure = applicableClosureForArgs(state, &*multiCalleeRef, args, argc);
        if (isHeaped(maybeClosure)) {
            state->regs[calleeReg] = maybeClosure;
            if (inlineCacheIdx) {
                state->consts[*inlineCacheIdx].set(*state, multiCalleeRef->methods().get());
                // Post-GC reload of callee:
                state->consts[*inlineCacheIdx + 1].set(*state, state->regs[calleeReg]);
            }

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

} // namespace
