#pragma once

#include <string.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "../vesihiisi.h"

#define CAT_IMPL(A, B) A##_##B
#define CAT(A, B) CAT_IMPL(A, B)

typedef struct MaybeSize {
    size_t val;
    bool hasVal;
} MaybeSize;

typedef struct MaybeUInt8 {
    uint8_t val;
    bool hasVal;
} MaybeUInt8;

typedef struct BucketIdx {
    size_t idx;
    bool occupied;
} BucketIdx;

typedef void (*SwapFn)(void* restrict x, void* restrict y);

static void reverse(void* arr, size_t count, size_t size, SwapFn swap);

// TODO: Enforce string literal `data`, incidentally avoiding `strlen`:
inline static Str strLit(char const* data) { return (Str){.data = data, .len = strlen(data)}; }

static bool strEq(Str s1, Str s2);

typedef struct StringBuilder {
    char* data;
    size_t len;
    size_t cap;
} StringBuilder;

static StringBuilder createStringBuilder(void);

inline static Str stringBuilderStr(StringBuilder const* s) { return (Str){s->data, s->len}; }

static void stringBuilderPush(StringBuilder* s, char c);

inline static void freeStringBuilder(StringBuilder* s) { free(s->data); }

static uint64_t fnv1aHash_n(char const* ptr, size_t count);

static uint64_t fnv1aHash(Str s);

static uint64_t hashCombine(uint64_t h1, uint64_t h2);
