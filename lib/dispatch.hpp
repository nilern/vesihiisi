#pragma once

#include "rt.hpp"

namespace {

enum class DomainCheckRes : uintptr_t { OK, MISSPECULATION, ERROR };

/// Check args. If result is `DomainCheckRes::ERROR`, also sets up error call.
[[nodiscard]]
DomainCheckRes checkDomainForArgs(
    RT* state, HRef<Closure> calleeRef, ORef const* args, size_t argc);

/// Like `checkDomainForArgs` but assumes args are in `state->regs[firstArgReg...]`.
[[nodiscard]]
DomainCheckRes checkDomain(RT* state);

/// Sets calleeReg to closure to call and returns `true`. If `callee` is not callable or is a
/// multimethod inapplicable to the given arguments, sets up error call and returns `false`.
bool calleeClosureForArgs(RT* state, ORef callee, ORef const* args, size_t argc);

bool calleeClosureForArglist(RT* state, ORef callee, ORef args);

/// Like `calleeClosureForArgs`, but assumes args are in `state->regs[firstArgReg...]` and can also
/// utilize the inline cache.
bool calleeClosure(RT* state, ORef callee, std::optional<uint8_t> inlineCacheIdx);

} // namespace
