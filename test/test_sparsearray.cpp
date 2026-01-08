#include <assert.h>

#include "../lib/util/arena.cpp"

#define SPARSE_ARRAY_IMPLEMENTATION
#define T char
#include "../lib/util/sparsearray.h"

void test_lifecycle(void) {
    Arena arena = newArena(defaultArenaBlockSize);
    newSparseArray_char(&arena, 0);

    freeArena(&arena);
}

static void test_empty(void) {
    Arena arena = newArena(defaultArenaBlockSize);
    SparseArray_char const sparr = newSparseArray_char(&arena, 0);

    assert(!sparseArrayGet_char(&sparr, 0).found);

    assert(sparseArrayIterEq_char(sparseArrayBegin_char(&sparr), sparseArrayEnd_char(&sparr)));

    freeArena(&arena);
}

static void test_one_word(void) {
    Arena arena = newArena(defaultArenaBlockSize);
    SparseArray_char sparr = newSparseArray_char(&arena, 0);

    for (size_t i = 0; i < SIZE_WIDTH; ++i) {
        assert(!sparseArrayGet_char(&sparr, i).found);
    }

    {
        size_t count = 0;

        SparseArrayIter_char const end = sparseArrayEnd_char(&sparr);
        for (SparseArrayIter_char it = sparseArrayBegin_char(&sparr);
             !sparseArrayIterEq_char(it, end);
             sparseArrayIterInc_char(&it)
        ) {
            ++count;
        }

        assert(count == 0);
    }

    for (size_t i = 0; i < SIZE_WIDTH; ++i) {
        sparseArraySet_char(&sparr, i, (char)i);
    }

    for (size_t i = 0; i < SIZE_WIDTH; ++i) {
        SparseArrayGot_char const gotChar = sparseArrayGet_char(&sparr, i);
        assert(gotChar.found);
        assert(*gotChar.val == (char)i);
    }

    {
        size_t count = 0;

        SparseArrayIter_char const end = sparseArrayEnd_char(&sparr);
        for (SparseArrayIter_char it = sparseArrayBegin_char(&sparr);
             !sparseArrayIterEq_char(it, end);
             sparseArrayIterInc_char(&it)
        ) {
            ++count;
        }

        assert(count == SIZE_WIDTH);
    }

    for (size_t i = 0; i < SIZE_WIDTH; ++i) {
        sparseArrayRemove_char(&sparr, i);
    }

    for (size_t i = 0; i < SIZE_WIDTH; ++i) {
        assert(!sparseArrayGet_char(&sparr, i).found);
    }

    {
        size_t count = 0;

        SparseArrayIter_char const end = sparseArrayEnd_char(&sparr);
        for (SparseArrayIter_char it = sparseArrayBegin_char(&sparr);
             !sparseArrayIterEq_char(it, end);
             sparseArrayIterInc_char(&it)
        ) {
            ++count;
        }

        assert(count == 0);
    }

    freeArena(&arena);
}

static void test_geometric(void) {
    Arena arena = newArena(defaultArenaBlockSize);
    SparseArray_char sparr = newSparseArray_char(&arena, 0);

    for (size_t i = 1; i < 1000; i *= 3) {
        assert(!sparseArrayGet_char(&sparr, i).found);
    }

    {
        size_t count = 0;

        SparseArrayIter_char const end = sparseArrayEnd_char(&sparr);
        for (SparseArrayIter_char it = sparseArrayBegin_char(&sparr);
             !sparseArrayIterEq_char(it, end);
             sparseArrayIterInc_char(&it)
        ) {
            ++count;
        }

        assert(count == 0);
    }

    for (size_t i = 1; i < 1000; i *= 3) {
        sparseArraySet_char(&sparr, i, (char)i);
    }

    for (size_t i = 1; i < 1000; i *= 3) {
        SparseArrayGot_char const gotChar = sparseArrayGet_char(&sparr, i);
        assert(gotChar.found);
        assert(*gotChar.val == (char)i);
    }

    {
        size_t count = 0;

        SparseArrayIter_char const end = sparseArrayEnd_char(&sparr);
        for (SparseArrayIter_char it = sparseArrayBegin_char(&sparr);
             !sparseArrayIterEq_char(it, end);
             sparseArrayIterInc_char(&it)
        ) {
            ++count;
        }

        assert(count == 7);
    }

    for (size_t i = 1; i < 1000; i *= 3) {
        sparseArrayRemove_char(&sparr, i);
    }

    for (size_t i = 1; i < 1000; i *= 3) {
        assert(!sparseArrayGet_char(&sparr, i).found);
    }

    {
        size_t count = 0;

        SparseArrayIter_char const end = sparseArrayEnd_char(&sparr);
        for (SparseArrayIter_char it = sparseArrayBegin_char(&sparr);
             !sparseArrayIterEq_char(it, end);
             sparseArrayIterInc_char(&it)
        ) {
            ++count;
        }

        assert(count == 0);
    }

    freeArena(&arena);
}

int main(int /*argc*/, char** /*argv*/) {
    puts("Testing lib/util/sparsearray.h...");

    test_lifecycle();
    puts("\ttest_lifecycle passed.");
    test_empty();
    puts("\ttest_empty passed.");
    test_one_word();
    puts("\ttest_one_word passed.");
    test_geometric();
    puts("\ttest_geometric passed.");

    puts("All lib/util/sparsearray.h tests passed.");
    return EXIT_SUCCESS;
}
