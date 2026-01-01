#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef struct Str {
    char const* data;
    size_t len;
} Str;

typedef struct ORef { uintptr_t bits; } ORef;

// OPTIMIZE: Could use tagged nullptr instead of !.hasVal:
typedef struct MaybeORef {
    ORef val;
    bool hasVal;
} MaybeORef;

typedef struct State State;

State* tryCreateState(size_t heapSize);
void freeState(State* state);

typedef struct Parser {
    char const* curr;
    char const* end;
} Parser;

Parser createParser(Str src);

bool read(State* state, MaybeORef* dest, Parser* parser);

typedef struct VMRes {
    ORef val;
    bool success;
} VMRes;

VMRes eval(State* state, ORef expr, bool debug);

void print(State const* state, FILE* dest, ORef v);
