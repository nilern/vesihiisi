#pragma once

#include "rt.hpp"

namespace {

typedef struct FindVarRes {
    enum {
        NS_FOUND_VAR,
        NS_FOUND_VAR_DEST_IDX
    } type;
    union {
        HRef<Var> var;
        size_t destIndex;
    };
} FindVarRes;

FindVarRes findVar(HRef<Namespace> nsRef, HRef<Symbol> name);

HRef<Var> getVar(RT* state, HRef<Namespace> nsRef, HRef<Symbol> name);

} // namespace
