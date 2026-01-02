#include "bitset.h"

#include <string.h>

static BitSet createBitSet(Arena* arena, size_t cap) {
    size_t wordCap = cap / UINTPTR_WIDTH;
    if (wordCap < 2) { wordCap = 2; }
    uintptr_t* const words = acalloc(arena, wordCap, sizeof *words);

    return (BitSet){.words = words, .wordCount = 0, .wordCap = wordCap};
}

static BitSet bitSetClone(Arena* arena, BitSet const* bits) {
    size_t const wordCount = bits->wordCount;
    size_t const wordCap = bits->wordCap;
    uintptr_t* const words = amalloc(arena, wordCap * sizeof *words);
    memcpy(words, bits->words, wordCount * sizeof *words);

    return (BitSet){.words = words, .wordCount = wordCount, .wordCap = wordCap};
}

static bool bitSetContains(BitSet const* bits, size_t n) {
    size_t const wordIdx = n / UINTPTR_WIDTH;

    if (wordIdx >= bits->wordCount) { return false; } // Cannot have been set

    uintptr_t const word = bits->words[wordIdx];
    size_t const subIdx = n % UINTPTR_WIDTH;
    uintptr_t const mask = 1lu << (UINTPTR_WIDTH - 1 - subIdx);
    return (word & mask) != 0;
}

static void bitSetSet(Arena* arena, BitSet* bits, size_t n) {
    size_t const wordIdx = n / UINTPTR_WIDTH;

    if (wordIdx >= bits->wordCap) { // Does not even fit allocation => grow:
        size_t newCap = bits->wordCap + bits->wordCap / 2;
        if (newCap < wordIdx + 1) { newCap = wordIdx + 1; }
        uintptr_t* const newWords = amalloc(arena, newCap * sizeof *newWords);

        memcpy(newWords, bits->words, bits->wordCount * sizeof *newWords);

        bits->words = newWords;
        bits->wordCap = newCap;
    }

    if (wordIdx >= bits->wordCount) { // In uninitialized => zero-initialize up to:
        size_t const newCount = wordIdx + 1;
        memset(
            bits->words + bits->wordCount,
            0,
            (newCount - bits->wordCount) * sizeof *bits->words
        );
        bits->wordCount = newCount;
    }

    size_t const subIdx = n % UINTPTR_WIDTH;
    bits->words[wordIdx] |= 1lu << (UINTPTR_WIDTH - 1 - subIdx);
}

static void bitSetRemove(BitSet* bits, size_t n) {
    size_t const wordIdx = n / UINTPTR_WIDTH;

    if (wordIdx >= bits->wordCount) { return; } // Was never set to begin with

    size_t const subIdx = n % UINTPTR_WIDTH;
    bits->words[wordIdx] &= ~(1lu << (UINTPTR_WIDTH - 1 - subIdx));
}

static void bitSetUnionInto(Arena* arena, BitSet* dest, BitSet const* src) {
    bool const extend = dest->wordCount < src->wordCount;

    if (src->wordCount > dest->wordCap) { // Will not even fit allocation => grow:
        size_t const newCap = src->wordCap;
        uintptr_t* const newWords = amalloc(arena, newCap * sizeof *newWords);

        // `src->wordCount > dest->wordCap` => `dest->wordCount < src->wordCount`:
        size_t const commonCount = dest->wordCount;
        for (size_t i = 0; i < commonCount; ++i) {
            newWords[i] = dest->words[i] | src->words[i];
        }

        dest->words = newWords;
        dest->wordCap = newCap;
    } else {
        size_t const commonCount = extend ? dest->wordCount : src->wordCount;
        for (size_t i = 0; i < commonCount; ++i) {
            dest->words[i] |= src->words[i];
        }
    }

    if (extend) {
        memcpy(
            dest->words + dest->wordCount,
            src->words + dest->wordCount,
            (src->wordCount - dest->wordCount) * sizeof *dest->words
        );
        dest->wordCount = src->wordCount;
    }
}

// OPTIMIZE: Skip empty words and bytes:
static MaybeSize bitSetIterNext(BitSetIter* it) {
    size_t const count = it->bitCount;

    for (size_t i = it->idx; i < count; ++i) {
        if (bitSetContains(it->bits, i)) {
            it->idx = i + 1;
            return (MaybeSize){.val = i, .hasVal = true};
        }
    }

    it->idx = count;
    return (MaybeSize){};
}
