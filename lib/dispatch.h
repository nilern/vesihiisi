#pragma once

#include "state.h"

/// Returns (first) error, `Zero` if none.
[[nodiscard]]
static ORef checkDomainForArgs(
    State* state, ClosureRef calleeRef, ORef const* args, size_t argc);

// OPTIMIZE: We know the domain of primops at VM compile time so going through this is suboptimal:
/// Like `checkDomainForArgs` but assumes args are in `state->regs[firstArgReg...]`.
[[nodiscard]]
static ORef checkDomain(State* state);

/// Sets calleeReg to closure to call and returns `true`. If `callee` is not callable or
/// inapplicable to the given arguments, sets up error call and returns `false`.
static bool calleeClosureForArgs(State* state, ORef callee, ORef const* args, size_t argc);

/// A wrapper for `calleeClosureForArgs` that assumes args are in `state->regs[firstArgReg...]`.
static bool calleeClosure(State* state, ORef callee);
