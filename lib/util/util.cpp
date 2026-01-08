#include "util.hpp"

#include <stdlib.h>
#include <string.h>

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

} // namespace

