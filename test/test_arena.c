#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "../lib/arena.c"

static void test_lifecycle(void) {
    Arena arena = newArena(defaultArenaBlockSize);

    freeArena(&arena);
}

static void test_amalloc(void) {
    Arena arena = newArena(defaultArenaBlockSize);
    uint8_t* const obj = amalloc(&arena, defaultArenaBlockSize / 2);
    assert(obj);
    assert(arena.free >= obj + defaultArenaBlockSize / 2);

    freeArena(&arena);
}

static void test_amallocHuge(void) {
    Arena arena = newArena(defaultArenaBlockSize);
    void* const obj = amalloc(&arena, defaultArenaBlockSize * 2);
    assert(obj);

    freeArena(&arena);
}

static void test_acalloc(void) {
    Arena arena = newArena(defaultArenaBlockSize);
    size_t const count = 3;
    size_t const size = defaultArenaBlockSize / 4;
    uint8_t* const obj = acalloc(&arena, count, size);

    assert(obj);
    assert(arena.free >= obj + count * size);

    size_t const totalSize = count * size;
    for (size_t i = 0; i < totalSize; ++i) {
        assert(obj[i] == 0);
    }

    freeArena(&arena);
}

static void test_acallocHuge(void) {
    Arena arena = newArena(defaultArenaBlockSize);
    size_t const count = 3;
    size_t const size = defaultArenaBlockSize;
    uint8_t* const obj = acalloc(&arena, count, size);

    assert(obj);

    size_t const totalSize = count * size;
    for (size_t i = 0; i < totalSize; ++i) {
        assert(obj[i] == 0);
    }

    freeArena(&arena);
}

int main(int /*argc*/, char** /*argv*/) {
    test_lifecycle();
    test_amalloc();
    test_amallocHuge();
    test_acalloc();
    test_acallocHuge();

    return EXIT_SUCCESS;
}
