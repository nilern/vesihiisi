#pragma once

#include <stddef.h>
#include <stdint.h>

#include "util.h"
#include "arena.h"

typedef struct BitSet {
    uintptr_t* words;
    size_t wordCount;
    size_t wordCap;
} BitSet;

static BitSet createBitSet(Arena* arena, size_t cap);

static BitSet bitSetClone(Arena* arena, BitSet const* bits);

inline static size_t bitSetLimit(BitSet const* bits) {
    return bits->wordCount * UINTPTR_WIDTH;
}

inline static size_t bitSetBitCap(BitSet const* bits) {
    return bits->wordCap * UINTPTR_WIDTH;
}

static bool bitSetContains(BitSet const* bits, size_t n);

static void bitSetSet(Arena* arena, BitSet* bits, size_t n);

static void bitSetRemove(BitSet* bits, size_t n);

static void bitSetUnionInto(Arena* arena, BitSet* dest, BitSet const* src);

typedef struct BitSetIter {
    BitSet const* bits;
    size_t idx;
    size_t bitCount;
} BitSetIter;

inline static BitSetIter newBitSetIter(BitSet const* bits) {
    return (BitSetIter){.bits = bits, .idx = 0, .bitCount = bitSetLimit(bits)};
}

static MaybeSize bitSetIterNext(BitSetIter* it);
