#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Str {
    uint8_t const* data;
    size_t len;
} Str;

typedef struct ORef { uint64_t bits; } ORef;

// OPTIMIZE: Could use tagged nullptr instead of !.hasVal:
typedef struct MaybeORef {
    ORef val;
    bool hasVal;
} MaybeORef;

struct Vshs_State;

struct Vshs_State* tryCreateState(size_t heapSize, int argc, char const* argv[]);
void freeState(struct Vshs_State* state);

void pushStackRoot(struct Vshs_State* state, ORef* stackLoc);
void popStackRoots(struct Vshs_State* state, size_t count);

typedef struct Parser Parser;

typedef enum ParseErrorType {
    EXPECTED_CHAR,
    EXPECTED_CHAR_CLASS,
    INVALID_UTF8
} ParseErrorType;

Parser* createParser(struct Vshs_State* state, Str src, Str filename);
void freeParser(Parser* parser);

void pushFilenameRoot(struct Vshs_State* state, Parser* parser); // HACK

typedef struct Vshs_LocatedORef {
    ORef val;
    ORef loc; // Actually `HRef<Loc>` but obviously we can't have that in C
} Vshs_LocatedORef;

typedef struct Vshs_MaybeLocatedORef {
    Vshs_LocatedORef val;
    bool hasVal;
} Vshs_MaybeLocatedORef;

typedef struct ParseError {
    ORef loc; // Actually `HRef<Loc>` but obviously we can't have that in C
    int32_t actualMaybeChar;
    union {
        char expectedChar;
        char const* expectedCharClass; // With static storage duration
    };
    ParseErrorType type;
} ParseError;

typedef struct ParseRes {
    union {
        Vshs_MaybeLocatedORef val;
        ParseError err;
    };
    bool success;
} ParseRes;

void printParseError(FILE* dest, Str src, ParseError const* err);

ParseRes read(struct Vshs_State* state, Parser* parser);

typedef enum SyntaxErrorType {
    INVALID_DEFINIEND,
    INVALID_PARAM,
    INVALID_BINDING,
    INVALID_BINDER,
    OVERLONG_BINDING,
} SyntaxErrorType;

typedef struct SyntaxError {
    ORef maybeLoc;
    SyntaxErrorType type;
} SyntaxError;

void printSyntaxError(
    struct Vshs_State const* extState, FILE* dest, Str src, SyntaxError const* err);

typedef struct SyntaxErrors {
    SyntaxError* vals;
    size_t count;
} SyntaxErrors;

void freeSyntaxErrors(SyntaxErrors* syntaxErrors);

typedef enum EvalErrorType {
    SYNTAX_ERROR,
    RUNTIME_ERROR
} EvalErrorType;

typedef struct EvalError {
    union {
        SyntaxErrors syntaxErrs;
        // Runtime errors are handled by `State::errorHandler`, we just need to know it failed
    };
    EvalErrorType type;
} EvalError;

typedef struct EvalRes {
    union {
        ORef val;
        EvalError err;
    };
    bool success;
} EvalRes;

EvalRes eval(struct Vshs_State* state, ORef expr, ORef loc, bool debug);

void print(struct Vshs_State const* state, FILE* dest, ORef v);

#ifdef __cplusplus
}
#endif
