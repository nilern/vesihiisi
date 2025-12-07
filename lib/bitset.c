typedef struct BitSet {
    uintptr_t* words;
    size_t wordCount;
    size_t wordCap;
} BitSet;

inline static void freeBitSet(BitSet* bits) { free(bits->words); }

[[maybe_unused]] // FIXME
static BitSet createBitSet(size_t cap) {
    size_t wordCap = cap / (8 * sizeof(uintptr_t)); // OPTIMIZE
    if (wordCap < 2) { wordCap = 2; }
    uintptr_t* const words = calloc(wordCap, sizeof *words);

    return (BitSet){
        .words = words,
        .wordCount = 0,
        .wordCap = wordCap
    };
}

[[maybe_unused]] // FIXME
static bool bitSetContains(BitSet const* bits, size_t n) {
    size_t const wordIdx = n / (8 * sizeof *bits->words); // OPTIMIZE
    if (wordIdx >= bits->wordCount) { return false; }

    uintptr_t const word = bits->words[wordIdx];
    size_t const subIdx = n % (8 * sizeof *bits->words); // OPTIMIZE
    uintptr_t const mask = 1lu << (8 * sizeof *bits->words - 1 - subIdx); // OPTIMIZE
    return (word & mask) != 0;
}

[[maybe_unused]] // FIXME
static void bitSetSet(BitSet* bits, size_t n) {
    size_t const wordIdx = n / (8 * sizeof *bits->words); // OPTIMIZE

    if (wordIdx >= bits->wordCap) { // Does not even fit allocation => grow:
        size_t const newCap = bits->wordCap + (bits->wordCap << 1);
        uintptr_t* const newWords = malloc(newCap * sizeof *newWords);

        memcpy(newWords, bits->words, bits->wordCount * sizeof *newWords);

        free(bits->words);
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

    size_t const subIdx = n % (8 * sizeof *bits->words); // OPTIMIZE
    bits->words[wordIdx] |= 1lu << (8 * sizeof *bits->words - 1 - subIdx); // OPTIMIZE
}

[[maybe_unused]] // FIXME
static void bitSetUnionInto(BitSet* dest, BitSet const* src) {
    bool const extend = dest->wordCount < src->wordCount;

    if (src->wordCount > dest->wordCap) { // Will not even fit allocation => grow:
        size_t const newCap = src->wordCap;
        uintptr_t* const newWords = malloc(newCap * sizeof *newWords);

        // `src->wordCount > dest->wordCap` => `dest->wordCount < src->wordCount`:
        size_t const commonCount = dest->wordCount;
        for (size_t i = 0; i < commonCount; ++i) {
            newWords[i] = dest->words[i] | src->words[i];
        }

        free(dest->words);
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
