#pragma once

#include "state.hpp"

namespace {

enum class DomainCheckRes : uintptr_t { OK, MISSPECULATION, ERROR };

/// Check args. If result is `DomainCheckRes::ERROR`, also sets up error call.
[[nodiscard]]
DomainCheckRes checkDomainForArgs(
    State* state, HRef<Closure> calleeRef, ORef const* args, size_t argc);

/// Like `checkDomainForArgs` but assumes args are in `state->regs[firstArgReg...]`.
[[nodiscard]]
DomainCheckRes checkDomain(State* state);

/// Sets calleeReg to closure to call and returns `true`. If `callee` is not callable or is a
/// multimethod inapplicable to the given arguments, sets up error call and returns `false`.
bool calleeClosureForArgs(State* state, ORef callee, ORef const* args, size_t argc);

bool calleeClosureForArglist(State* state, ORef callee, ORef args);

/// A wrapper for `calleeClosureForArgs` that assumes args are in `state->regs[firstArgReg...]`.
bool calleeClosure(State* state, ORef callee);

} // namespace
