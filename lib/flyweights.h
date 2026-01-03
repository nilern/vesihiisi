#pragma once

#include "state.h"

static Specializations newSpecializations(void);

static void freeSpecializations(Specializations* specializations);

static MethodRef specialize(State* state, MethodRef generic, ArrayMutRef types);

static void pruneSpecializations(Specializations* specializations);
