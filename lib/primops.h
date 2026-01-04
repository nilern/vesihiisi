#pragma once

#include "object.h"
#include "state.h"

static ORef getErrorHandler(State const* state);

static PrimopRes callBytecode(State* /*state*/);
