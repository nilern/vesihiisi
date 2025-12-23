#pragma once

#include "object.h"
#include "state.h"

static ORef getErrorHandler(State const* state);

[[nodiscard]]
static ORef checkDomain(State* state);

static PrimopRes callBytecode(State* /*state*/);
