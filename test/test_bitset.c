#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "../lib/util/util.c"
#include "../lib/util/arena.c"
#include "../lib/util/bitset.c"

static const size_t wordBitSize = sizeof(uintptr_t) * 8;

static void test_lifecycle(void) {
    Arena arena = newArena(4 * (1 << 10));
    createBitSet(&arena, 0);

    freeArena(&arena);
}

static void test_empty(void) {
    Arena arena = newArena(4 * (1 << 10));
    BitSet bits = createBitSet(&arena, 0);

    for (size_t b = 1; b < 1000; b *= 10) {
        assert(!bitSetContains(&bits, b));
    }

    freeArena(&arena);
}

static void test_one_word(void) {
    Arena arena = newArena(4 * (1 << 10));
    BitSet bits = createBitSet(&arena, 0);

    for (size_t i = 0; i < wordBitSize; ++i) {
        assert(!bitSetContains(&bits, i));
        bitSetSet(&arena, &bits, i);
        assert(bitSetContains(&bits, i));
        bitSetRemove(&bits, i);
        assert(!bitSetContains(&bits, i));
    }

    freeArena(&arena);
}

static void test_geometric(void) {
    Arena arena = newArena(4 * (1 << 10));
    BitSet bits = createBitSet(&arena, 0);

    for (size_t b = 1; b < 1000; b *= 10) {
        assert(!bitSetContains(&bits, b));
        bitSetSet(&arena, &bits, b);
        assert(bitSetContains(&bits, b));
        bitSetRemove(&bits, b);
        assert(!bitSetContains(&bits, b));
    }

    freeArena(&arena);
}

static void test_union_into(void) {
    Arena arena = newArena(4 * (1 << 10));

    {
        BitSet dest = createBitSet(&arena, 0);
        for (size_t i = 0; i < 1000; i += 10) { bitSetSet(&arena, &dest, i); }
        BitSet src = createBitSet(&arena, 0);
        for (size_t i = 0; i < 1000; i += 5) { bitSetSet(&arena, &src, i); }

        bitSetUnionInto(&arena, &dest, &src);

        for (size_t i = 0; i < 1000; ++i) { assert(bitSetContains(&dest, i) == (i % 5 == 0)); }
    }

    {
        BitSet dest = createBitSet(&arena, 0);
        for (size_t i = 0; i < 500; i += 5) { bitSetSet(&arena, &dest, i); }
        BitSet src = createBitSet(&arena, 0);
        for (size_t i = 0; i < 1000; i += 10) { bitSetSet(&arena, &src, i); }

        bitSetUnionInto(&arena, &dest, &src);

        for (size_t i = 0; i < 500; ++i) { assert(bitSetContains(&dest, i) == (i % 5 == 0)); }
        for (size_t i = 500; i < 1000; ++i) { assert(bitSetContains(&dest, i) == (i % 10 == 0)); }
    }

    {
        BitSet dest = createBitSet(&arena, 0);
        for (size_t i = 0; i < 1000; i += 10) { bitSetSet(&arena, &dest, i); }
        BitSet src = createBitSet(&arena, 0);
        for (size_t i = 0; i < 500; i += 5) { bitSetSet(&arena, &src, i); }

        bitSetUnionInto(&arena, &dest, &src);

        for (size_t i = 0; i < 500; ++i) { assert(bitSetContains(&dest, i) == (i % 5 == 0)); }
        for (size_t i = 500; i < 1000; ++i) { assert(bitSetContains(&dest, i) == (i % 10 == 0)); }
    }

    freeArena(&arena);
}

int main(int /*argc*/, char** /*argv*/) {
    test_lifecycle();
    test_empty();
    test_one_word();
    test_geometric();
    test_union_into();

    return EXIT_SUCCESS;
}
