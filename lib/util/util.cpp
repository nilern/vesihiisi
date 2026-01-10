#include "util.hpp"

#include <stdlib.h>
#include <string.h>
#include <filesystem> // OPTIMIZE: Avoid this, requires linking to a bunch of `std::` stuff.

namespace {

void reverse(void* arr, size_t count, size_t size, SwapFn swap) {
    if (count < 2) { return; }

    for (char *begin = (char*)arr, *end = begin + (count - 1) * size;
         begin < end;
         begin += size, end -= size
    ) {
        swap(begin, end);
    }
}

bool strEq(Str s1, Str s2) {
    return s1.len == s2.len
        && strncmp(s1.data, s2.data, s1.len) == 0;
}

StringBuilder createStringBuilder(void) {
    size_t const cap = 2;
    
    char* data = (char*)malloc(cap);
    if (!data) { exit(EXIT_FAILURE); }
    
    return StringBuilder{
        .data = data,
        .len = 0,
        .cap = cap
    };
}

void stringBuilderPush(StringBuilder* s, char c) {
    if (s->len == s->cap) {
        size_t const newCap = s->cap + (s->cap >> 1); // cap * 1.5
        
        char* const data = (char*)realloc(s->data, newCap);
        if (!data) { exit(EXIT_FAILURE); }
        
        s->data = data;
        s->cap = newCap;
    }
    
    s->data[s->len++] = c;
}

uint64_t fnv1aHash_n(char const* ptr, size_t count) {
    uint64_t hash = 14695981039346656037u;

    for (size_t i = 0; i < count; ++i) {
        hash ^= (uint8_t)ptr[i];
        hash *= 1099511628211 ;
    }

    return hash;
}

// FIXME: Replace with SipHash to prevent DoS attacks:
uint64_t fnv1aHash(Str s) { return fnv1aHash_n(s.data, s.len); }

// OPTIMIZE: More sophisticated combination algorithm:
uint64_t hashCombine(uint64_t h1, uint64_t h2) { return 3 * h1 + h2; }

void printFilename(FILE* dest, Str filename) {
    std::filesystem::path const path{filename.data, filename.data + filename.len};
    std::error_code pathErr;
    auto const relative = std::filesystem::relative(path, pathErr);
    if (!pathErr && !relative.empty()) {
        fprintf(dest, "%s:", relative.c_str());
    } else { // Default to printing the absolute path:
        // TODO: Avoid using POSIX `printf` extension:
        fprintf(dest, "%.*s:", (int)filename.len, filename.data);
    }
}

// A bit slow but will save lots of debug info space. And fixing the error that the position is
// calculated for will be far slower still. While we do not have e.g. .fasl files `src` should be
// available (if the user lost it they have bigger problems than missing line and column
// numbers...).
Coord byteIdxToCoord(Str src, size_t byteIdx) {
    Coord pos{};

    size_t const limit = byteIdx < src.len ? byteIdx : src.len; // Basically use `src.len` for EOF
    for (size_t i = 0; i < limit; ++i) {
        pos.advance(src.data[i]);
    }

    return pos;
}

} // namespace

