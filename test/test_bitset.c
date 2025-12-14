#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "../lib/util.c"
#include "../lib/bitset.c"

static const size_t wordBitSize = sizeof(uintptr_t) * 8;

static void test_lifecycle(void) {
    BitSet bits = createBitSet(0);

    freeBitSet(&bits);
}

static void test_empty(void) {
    BitSet bits = createBitSet(0);

    for (size_t b = 1; b < 1000; b *= 10) {
        assert(!bitSetContains(&bits, b));
    }

    freeBitSet(&bits);
}

static void test_one_word(void) {
    BitSet bits = createBitSet(0);

    for (size_t i = 0; i < wordBitSize; ++i) {
        assert(!bitSetContains(&bits, i));
        bitSetSet(&bits, i);
        assert(bitSetContains(&bits, i));
        bitSetRemove(&bits, i);
        assert(!bitSetContains(&bits, i));
    }

    freeBitSet(&bits);
}

static void test_geometric(void) {
    BitSet bits = createBitSet(0);

    for (size_t b = 1; b < 1000; b *= 10) {
        assert(!bitSetContains(&bits, b));
        bitSetSet(&bits, b);
        assert(bitSetContains(&bits, b));
        bitSetRemove(&bits, b);
        assert(!bitSetContains(&bits, b));
    }

    freeBitSet(&bits);
}

static void test_union_into(void) {
    {
        BitSet dest = createBitSet(0);
        for (size_t i = 0; i < 1000; i += 10) { bitSetSet(&dest, i); }
        BitSet src = createBitSet(0);
        for (size_t i = 0; i < 1000; i += 5) { bitSetSet(&src, i); }

        bitSetUnionInto(&dest, &src);

        for (size_t i = 0; i < 1000; ++i) { assert(bitSetContains(&dest, i) == (i % 5 == 0)); }

        freeBitSet(&dest);
        freeBitSet(&src);
    }

    {
        BitSet dest = createBitSet(0);
        for (size_t i = 0; i < 500; i += 5) { bitSetSet(&dest, i); }
        BitSet src = createBitSet(0);
        for (size_t i = 0; i < 1000; i += 10) { bitSetSet(&src, i); }

        bitSetUnionInto(&dest, &src);

        for (size_t i = 0; i < 500; ++i) { assert(bitSetContains(&dest, i) == (i % 5 == 0)); }
        for (size_t i = 500; i < 1000; ++i) { assert(bitSetContains(&dest, i) == (i % 10 == 0)); }

        freeBitSet(&dest);
        freeBitSet(&src);
    }

    {
        BitSet dest = createBitSet(0);
        for (size_t i = 0; i < 1000; i += 10) { bitSetSet(&dest, i); }
        BitSet src = createBitSet(0);
        for (size_t i = 0; i < 500; i += 5) { bitSetSet(&src, i); }

        bitSetUnionInto(&dest, &src);

        for (size_t i = 0; i < 500; ++i) { assert(bitSetContains(&dest, i) == (i % 5 == 0)); }
        for (size_t i = 500; i < 1000; ++i) { assert(bitSetContains(&dest, i) == (i % 10 == 0)); }

        freeBitSet(&dest);
        freeBitSet(&src);
    }
}

int main(int /*argc*/, char** /*argv*/) {
    test_lifecycle();
    test_empty();
    test_one_word();
    test_geometric();
    test_union_into();

    return EXIT_SUCCESS;
}
