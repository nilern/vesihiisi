#pragma once

#include "object.hpp"
#include "state.hpp"

namespace {

ORef getErrorHandler(State const* state);

PrimopRes callBytecode(State* state);

} // namespace
