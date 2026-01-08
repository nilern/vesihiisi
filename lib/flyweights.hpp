#pragma once

#include "state.hpp"

namespace {

Specializations newSpecializations(void);

void freeSpecializations(Specializations* specializations);

HRef<Method> specialize(State* state, HRef<Method> generic, HRef<ArrayMut> types);

void pruneSpecializations(Specializations* specializations);

} // namespace
