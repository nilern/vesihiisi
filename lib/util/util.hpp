#pragma once

#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbit.h>

#include "../vesihiisi.h"

#define CAT_IMPL(A, B) A##_##B
#define CAT(A, B) CAT_IMPL(A, B)

namespace {

inline size_t requiredBitsize(int64_t n) {
    // Hacker's Delight Section 5-3 Counting Leading 0's:
    int64_t const nabs = n ^ (n >> (INT64_WIDTH - 1)); // `n >= 0 ? n : -n - 1`
    return (INT64_WIDTH + 1) - stdc_leading_zeros(nabs);
}

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

    T const& operator[](size_t i) const {
        assert(i < count);
        return data[i];
    }

    T& operator[](size_t i) {
        assert(i < count);
        return data[i];
    }
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

void printFilename(FILE* dest, Str filename);

struct Coord {
    size_t line;
    size_t col;

    Coord() : line{1}, col{1} {}

    void advance(int32_t c) {
        if (c != '\n') {
            ++col;
        } else {
            ++line;
            col = 1;
        }
    }

    void print(FILE* dest) { fprintf(dest, "%lu:%lu", line, col); }
};

// A bit slow but will save lots of debug info space. And fixing the error that the position is
// calculated for will be far slower still. While we do not have e.g. .fasl files `src` should be
// available (if the user lost it they have bigger problems than missing line and column
// numbers...).
Coord byteIdxToCoord(Str src, size_t byteIdx);

} // namespace
