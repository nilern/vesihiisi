#pragma once

#include <assert.h>
#include <string.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbit.h>
#include <utility>

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

    explicit Maybe(T t_val) : val{t_val}, hasVal{true} {}
    Maybe() : val{}, hasVal{false} {}
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

    explicit Res(T t_val) : val{t_val}, success{true} {}
    explicit Res(Err t_err) : err{t_err}, success{false} {}
};

#define TRY(ResultType, expr) \
    ({auto const TRY_IMPL_result = (expr); \
      if (!TRY_IMPL_result.success) { return ResultType{TRY_IMPL_result.err}; } \
      TRY_IMPL_result.val;})

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
inline Str strLit(char const* data) {
    return Str{.data = /*HACK:(?)*/reinterpret_cast<uint8_t const*>(data), .len = strlen(data)};
}

bool strEq(Str s1, Str s2);

typedef struct StringBuilder {
    uint8_t* data;
    size_t len;
    size_t cap;
} StringBuilder;

StringBuilder createStringBuilder(void);

inline Str stringBuilderStr(StringBuilder const* s) { return Str{s->data, s->len}; }

void stringBuilderPush(StringBuilder* s, uint8_t c);

inline void freeStringBuilder(StringBuilder* s) { free(s->data); }

uint64_t fnv1aHash_n(uint8_t const* ptr, size_t count);

uint64_t fnv1aHash(Str s);

uint64_t hashCombine(uint64_t h1, uint64_t h2);

constexpr bool isUTF8Cont(uint8_t b) { return (b & 0xc0) == 0x80; }

int utf8EncodedWidth(int32_t codepoint);

class UTF8InputFile {
    static constexpr size_t bufCap = 4;

    FILE* cfile;
    size_t bufCount = 0;
    uint8_t buf[bufCap];

    explicit UTF8InputFile(Str filename);

public:
    UTF8InputFile() : cfile{nullptr} {}

    explicit UTF8InputFile(FILE* file) : cfile(file) {}

    bool isValid() const { return static_cast<bool>(cfile); }

    [[nodiscard]]
    static bool open(UTF8InputFile& dest, Str filename) {
        dest = UTF8InputFile{filename};
        return dest.isValid();
    }

    void close() {
        fclose(cfile);
        cfile = nullptr;
    }

    ~UTF8InputFile() { if (cfile) { fclose(cfile); } }

    UTF8InputFile(UTF8InputFile const&) = delete;
    UTF8InputFile& operator=(UTF8InputFile const&) = delete;

    UTF8InputFile(UTF8InputFile&& that) { *this = std::move(that); }

    UTF8InputFile& operator=(UTF8InputFile&& that) {
        cfile = that.cfile;
        bufCount = that.bufCount;
        memcpy(buf, that.buf, bufCount);

        that.cfile = nullptr;

        return *this;
    }

    /// @return Positive codepoint on success, EOF at end, EOF - 1 on invalid UTF-8
    int32_t peec();

    /// @return Positive codepoint on success, EOF at end, EOF - 1 on invalid UTF-8
    int32_t getc();

    size_t skipInvalid();
};

void printFilename(FILE* dest, Str filename);

struct Coord {
    size_t line = 1;
    size_t col = 1;

    void advance(int32_t c) {
        if (c != '\n') {
            ++col;
        } else {
            ++line;
            col = 1;
        }
    }

    void print(FILE* dest) const { fprintf(dest, "%lu:%lu", line, col); }
};

// A bit slow but will save lots of debug info space. And fixing the error that the position is
// calculated for will be far slower still. While we do not have e.g. .fasl files `src` should be
// available (if the user lost it they have bigger problems than missing line and column
// numbers...).
Coord byteIdxToCoord(Str src, size_t byteIdx);
Maybe<Coord> fileByteIdxToCoord(Str filename, size_t byteIdx);

} // namespace
