#pragma once

#include "object.h"
#include "state.h"

static ORef getErrorHandler(State const* state);

[[nodiscard]]
static ORef checkDomain(State* state);

static ORef applicableClosure(State const* state, Multimethod const* callee);

static PrimopRes callBytecode(State* /*state*/);
