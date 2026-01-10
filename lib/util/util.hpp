#pragma once

#include <string.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "../vesihiisi.h"

#define CAT_IMPL(A, B) A##_##B
#define CAT(A, B) CAT_IMPL(A, B)

namespace {

template<typename T>
struct Maybe {
    T val;
    bool hasVal;
};

typedef struct BucketIdx {
    size_t idx;
    bool occupied;
} BucketIdx;

template<typename Err, typename T>
struct Res {
    union {
        T val;
        Err err;
    };
    bool success;
};

typedef void (*SwapFn)(void* x, void* y);

void reverse(void* arr, size_t count, size_t size, SwapFn swap);

template<typename T>
struct Slice {
    T* data;
    size_t count;

    T const& operator[](size_t i) const { return data[i]; }
    T& operator[](size_t i) { return data[i]; }
};

// TODO: Enforce string literal `data`, incidentally avoiding `strlen`:
inline Str strLit(char const* data) { return Str{.data = data, .len = strlen(data)}; }

bool strEq(Str s1, Str s2);

typedef struct StringBuilder {
    char* data;
    size_t len;
    size_t cap;
} StringBuilder;

StringBuilder createStringBuilder(void);

inline Str stringBuilderStr(StringBuilder const* s) { return Str{s->data, s->len}; }

void stringBuilderPush(StringBuilder* s, char c);

inline void freeStringBuilder(StringBuilder* s) { free(s->data); }

uint64_t fnv1aHash_n(char const* ptr, size_t count);

uint64_t fnv1aHash(Str s);

uint64_t hashCombine(uint64_t h1, uint64_t h2);

} // namespace
