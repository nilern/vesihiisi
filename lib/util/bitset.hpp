#pragma once

#include <stddef.h>
#include <stdint.h>

#include "util.hpp"
#include "arena.hpp"

namespace {

typedef struct BitSet {
    uintptr_t* words;
    size_t wordCount;
    size_t wordCap;
} BitSet;

BitSet createBitSet(Arena* arena, size_t cap);

BitSet bitSetClone(Arena* arena, BitSet const* bits);

inline size_t bitSetLimit(BitSet const* bits) {
    return bits->wordCount * UINTPTR_WIDTH;
}

inline size_t bitSetBitCap(BitSet const* bits) {
    return bits->wordCap * UINTPTR_WIDTH;
}

bool bitSetContains(BitSet const* bits, size_t n);

void bitSetSet(Arena* arena, BitSet* bits, size_t n);

void bitSetRemove(BitSet* bits, size_t n);

void bitSetUnionInto(Arena* arena, BitSet* dest, BitSet const* src);

typedef struct BitSetIter {
    BitSet const* bits;
    size_t idx;
    size_t bitCount;
} BitSetIter;

inline BitSetIter newBitSetIter(BitSet const* bits) {
    return BitSetIter{.bits = bits, .idx = 0, .bitCount = bitSetLimit(bits)};
}

Maybe<size_t> bitSetIterNext(BitSetIter* it);

} // namespace
