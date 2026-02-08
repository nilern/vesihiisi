#pragma once

#include "value.hpp"

namespace {

typedef struct VMRes {
    ORef val;
    bool success;
} VMRes;

VMRes run(RT* state, HRef<Closure> self);

} // namespace
