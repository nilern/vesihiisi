#pragma once

#include "state.h"

typedef struct FindVarRes {
    enum {
        NS_FOUND_VAR,
        NS_FOUND_VAR_DEST_IDX
    } type;
    union {
        VarRef var;
        size_t destIndex;
    };
} FindVarRes;

static FindVarRes findVar(NamespaceRef nsRef, SymbolRef name);

static VarRef getVar(State* state, NamespaceRef nsRef, SymbolRef name);
