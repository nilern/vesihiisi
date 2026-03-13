#pragma once

#include "value.hpp"

namespace {

extern "C" uint64_t callForeign(
    void* f, bool fRet, uint8_t const* unboxings, ORef const* args, size_t argc);

typedef struct VMRes {
    ORef val;
    bool success;
} VMRes;

VMRes run(RT* state, HRef<Closure> self);

} // namespace
