#pragma once

#include "state.hpp"

namespace {

// Symbols
// =================================================================================================

SymbolTable newSymbolTable();

void freeSymbols(SymbolTable* symbols);

void pruneSymbols(SymbolTable* symbols);

// Specializations
// =================================================================================================

Specializations newSpecializations();

void freeSpecializations(Specializations* specializations);

HRef<Method> specialize(State* state, HRef<Method> generic, HRef<ArrayMut> types);

void pruneSpecializations(Specializations* specializations);

} // namespace
