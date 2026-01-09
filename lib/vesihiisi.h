#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Str {
    char const* data;
    size_t len;
} Str;

typedef struct ORef { uint64_t bits; } ORef;

// OPTIMIZE: Could use tagged nullptr instead of !.hasVal:
typedef struct MaybeORef {
    ORef val;
    bool hasVal;
} MaybeORef;

struct Vshs_State;

struct Vshs_State* tryCreateState(size_t heapSize);
void freeState(struct Vshs_State* state);

typedef struct Parser Parser;

typedef enum ParseErrorType {
    EXPECTED_CHAR,
    EXPECTED_CHAR_CLASS
} ParseErrorType;

Parser* createParser(Str src);
void freeParser(Parser* parser);

typedef struct ParseError {
    size_t byteIdx;
    int actualMaybeChar;
    union {
        char expectedChar;
        char const* expectedCharClass; // With static storage duration
    };
    ParseErrorType type;
} ParseError;

typedef struct ParseRes {
    union {
        MaybeORef val;
        ParseError err;
    };
    bool success;
} ParseRes;

void printParseError(FILE* dest, ParseError const* err);

ParseRes read(struct Vshs_State* state, Parser* parser);

typedef struct VMRes {
    ORef val;
    bool success;
} VMRes;

VMRes eval(struct Vshs_State* state, ORef expr, bool debug);

void print(struct Vshs_State const* state, FILE* dest, ORef v);

#ifdef __cplusplus
}
#endif
