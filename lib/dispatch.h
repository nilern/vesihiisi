#pragma once

#include "state.h"

// OPTIMIZE: We know the domain of primops at VM compile time so going through this is suboptimal:
/// Returns (first) error, `Zero` if none:
[[nodiscard]]
static ORef checkDomain(State* state);

/// Returns applicable closure from `callee`, `Zero` if none is found:
static ORef applicableClosure(State const* state, Multimethod const* callee);
