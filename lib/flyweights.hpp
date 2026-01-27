#pragma once

#include "object.hpp"

namespace {

// Symbols
// =================================================================================================

typedef struct SymbolTable {
    ORef* entries;
    size_t count;
    size_t cap;
} SymbolTable;

SymbolTable newSymbolTable();

void freeSymbols(SymbolTable* symbols);

void pruneSymbols(SymbolTable* symbols);

// Specializations
// =================================================================================================

typedef struct Specializations {
    ORef* entries;
    size_t count;
    size_t cap;
} Specializations;

Specializations newSpecializations();

void freeSpecializations(Specializations* specializations);

HRef<Method> specialize(State* state, HRef<Method> generic, HRef<ArrayMut> types);

void pruneSpecializations(Specializations* specializations);

} // namespace
