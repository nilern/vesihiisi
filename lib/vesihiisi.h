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

struct Vshs_RT;

struct Vshs_RT* Vshs_tryCreateRT(
    size_t heapSize, char const* vshsHome, int argc, char const* argv[]);
void Vshs_freeRT(struct Vshs_RT* state);

typedef enum Vshs_ParseErrorType {
    EXPECTED_CHAR,
    EXPECTED_CHAR_CLASS,
    INVALID_UTF8
} Vshs_ParseErrorType;

typedef struct Vshs_LocatedORef {
    ORef val;
    ORef loc; // Actually `HRef<Loc>` but obviously we can't have that in C
} Vshs_LocatedORef;

typedef struct Vshs_MaybeLocatedORef {
    Vshs_LocatedORef val;
    bool hasVal;
} Vshs_MaybeLocatedORef;

typedef struct Vshs_ParseError {
    ORef loc; // Actually `HRef<Loc>` but obviously we can't have that in C
    int32_t actualMaybeChar;
    union {
        char expectedChar;
        char const* expectedCharClass; // With static storage duration
    };
    Vshs_ParseErrorType type;
} Vshs_ParseError;

typedef struct Vshs_ParseRes {
    union {
        Vshs_MaybeLocatedORef val;
        Vshs_ParseError err;
    };
    bool success;
} Vshs_ParseRes;

void Vshs_printParseError(FILE* dest, Str src, Vshs_ParseError const* err);

typedef enum Vshs_SyntaxErrorType {
    INVALID_DEFINIEND,
    INVALID_PARAM,
    INVALID_BINDING,
    INVALID_BINDER,
    OVERLONG_BINDING,
} Vshs_SyntaxErrorType;

typedef struct Vshs_SyntaxError {
    ORef maybeLoc;
    Vshs_SyntaxErrorType type;
} Vshs_SyntaxError;

void Vshs_printSyntaxError(
    struct Vshs_RT const* extRT, FILE* dest, Str src, Vshs_SyntaxError const* err);

typedef struct Vshs_SyntaxErrors {
    Vshs_SyntaxError* vals;
    size_t count;
} Vshs_SyntaxErrors;

void Vshs_freeSyntaxErrors(Vshs_SyntaxErrors* syntaxErrors);

void Vshs_write(struct Vshs_RT const* state, FILE* dest, ORef v);

typedef enum ResTag {RES_ERR, RES_OK} ResTag;

typedef struct Vshs_Err {
    union {
        Vshs_ParseError parseErr;
        Vshs_SyntaxErrors syntaxErrs;
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

bool Vshs_bootstrap(struct Vshs_RT* state, char const* bootstrapFilename);

Vshs_MaybeRes Vshs_evalString(struct Vshs_RT* state, Str src, Str filename);

#ifdef __cplusplus
}
#endif
