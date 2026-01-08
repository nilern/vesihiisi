#pragma once

#include "state.hpp"

namespace {

/// Returns (first) error, `Default` if none.
[[nodiscard]]
ORef checkDomainForArgs(State* state, HRef<Closure> calleeRef, ORef const* args, size_t argc);

// OPTIMIZE: We know the domain of primops at VM compile time so going through this is suboptimal:
/// Like `checkDomainForArgs` but assumes args are in `state->regs[firstArgReg...]`.
[[nodiscard]]
ORef checkDomain(State* state);

/// Sets calleeReg to closure to call and returns `true`. If `callee` is not callable or
/// inapplicable to the given arguments, sets up error call and returns `false`.
bool calleeClosureForArgs(State* state, ORef callee, ORef const* args, size_t argc);

bool calleeClosureForArglist(State* state, ORef callee, ORef args);

/// A wrapper for `calleeClosureForArgs` that assumes args are in `state->regs[firstArgReg...]`.
bool calleeClosure(State* state, ORef callee);

} // namespace
