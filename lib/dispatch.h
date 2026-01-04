#pragma once

#include "state.h"

// OPTIMIZE: We know the domain of primops at VM compile time so going through this is suboptimal:
[[nodiscard]]
static ORef checkDomain(State* state);

[[nodiscard]]
static bool closureIsApplicable(State const* state, Closure const* callee);

static ORef applicableClosure(State const* state, Multimethod const* callee);
