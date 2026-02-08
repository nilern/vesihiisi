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

struct Vshs_State* tryCreateState(
    size_t heapSize, char const* vshsHome, int argc, char const* argv[]);
void freeState(struct Vshs_State* state);

typedef struct Parser Parser;

typedef enum ParseErrorType {
    EXPECTED_CHAR,
    EXPECTED_CHAR_CLASS,
    INVALID_UTF8
} ParseErrorType;

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

void print(struct Vshs_State const* state, FILE* dest, ORef v);

typedef enum ResTag {RES_ERR, RES_OK} ResTag;

typedef struct Vshs_Err {
    union {
        ParseError parseErr;
        SyntaxErrors syntaxErrs;
    };
    enum {
        VSHS_PARSE_ERR,
        VSHS_SYNTAX_ERRS,
        VSHS_RUNTIME_ERR
    } type;
} Vshs_Err;

void Vshs_freeError(Vshs_Err* err);

typedef struct Vshs_Res {
    union {
        Vshs_Err err;
        ORef val;
    };
    ResTag tag;
} Vshs_Res;

typedef struct Vshs_MaybeRes {
    Vshs_Res val;
    bool hasVal;
} Vshs_MaybeRes;

bool bootstrap(struct Vshs_State* state, char const* bootstrapFilename);

Vshs_MaybeRes Vshs_evalString(struct Vshs_State* state, Str src, Str filename);

#ifdef __cplusplus
}
#endif
