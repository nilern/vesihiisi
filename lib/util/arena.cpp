#include "arena.hpp"

#include <stdlib.h>
#include <string.h>

namespace {

void freeFullBlocks(ArenaFullBlocks* blocks, size_t /*blockSize*/) {
    size_t const count = blocks->count;
    for (size_t i = 0; i < count; ++i) {
        free(blocks->vals[i]);
    }

    free(blocks->vals);
}

ArenaFullBlocks newArenaFullBlocks(void) {
    size_t const cap = 2;
    uint8_t** const vals = (uint8_t**)malloc(cap * sizeof *vals);
    return ArenaFullBlocks{.vals = vals, .count = 0, .cap = cap};
}

void pushFullBlock(ArenaFullBlocks* blocks, uint8_t* block) {
    if (blocks->count == blocks->cap) {
        size_t const newCap = blocks->cap + blocks->cap / 2;
        blocks->vals = (uint8_t**)realloc(blocks->vals, newCap * sizeof *blocks->vals);
        blocks->cap = newCap;
    }

    blocks->vals[blocks->count++] = block;
}

void freeHugeObjs(ArenaHugeObjs* hugeObjs) {
    size_t const count = hugeObjs->count;
    for (size_t i = 0; i < count; ++i) {
        free(hugeObjs->vals[i].val);
    }

    free(hugeObjs->vals);
}

ArenaHugeObjs newArenaHugeObjs(void) {
    size_t const cap = 2;
    ArenaHugeObj* const vals = (ArenaHugeObj*)malloc(cap * sizeof *vals);
    return ArenaHugeObjs{.vals = vals, .count = 0, .cap = cap};
}

void pushHugeObj(ArenaHugeObjs* hugeObjs, void* obj, size_t size) {
    if (hugeObjs->count == hugeObjs->cap) {
        size_t const newCap = hugeObjs->cap + hugeObjs->cap / 2;
        hugeObjs->vals = (ArenaHugeObj*)realloc(hugeObjs->vals, newCap * sizeof *hugeObjs->vals);
        hugeObjs->cap = newCap;
    }

    hugeObjs->vals[hugeObjs->count++] = ArenaHugeObj{.size = size, .val = obj};
}

void* amallocHuge(ArenaHugeObjs* hugeObjs, size_t size) {
    void* const obj = malloc(size);
    pushHugeObj(hugeObjs, obj, size);
    return obj;
}

void* acallocHuge(ArenaHugeObjs* hugeObjs, size_t count, size_t size) {
    void* const obj = calloc(count, size);
    pushHugeObj(hugeObjs, obj, size);
    return obj;
}

void freeArena(Arena* arena) {
    free(arena->start);
    freeFullBlocks(&arena->fullBlocks, arena->blockSize);
    freeHugeObjs(&arena->hugeObjs);
}

Arena newArena(size_t blockSize) {
    return Arena{
        .free = nullptr,
        .limit = nullptr,
        .start = nullptr,
        .blockSize = blockSize,
        .fullBlocks = newArenaFullBlocks(),
        .hugeObjs = newArenaHugeObjs()
    };
}

void arenaGrow(Arena* arena) {
    if (arena->start) {
        pushFullBlock(&arena->fullBlocks, arena->start);
    }

    uint8_t* newStart = (uint8_t*)malloc(arena->blockSize);
    arena->free = newStart;
    arena->limit = newStart + arena->blockSize;
    arena->start = newStart;
}

[[nodiscard]]
void* amalloc(Arena* arena, size_t size) {
    for (;/*ever*/;) {
        uintptr_t address = (uintptr_t)arena->free;
        size_t const align = alignof(max_align_t);
        address = (address + align - 1) & ~(align - 1); // Align `address` up

        uint8_t* newFree = (uint8_t*)(address + size);
        if (newFree >= arena->limit) {
            if (size < arena->blockSize) {
                arenaGrow(arena);
                continue;
            } else {
                return amallocHuge(&arena->hugeObjs, size);
            }
        }

        arena->free = newFree;
        return (void*)address;
    }
}

[[nodiscard]]
void* arealloc(Arena* arena, void* ptr, size_t oldSize, size_t size) {
    void* const obj = amalloc(arena, size);

    if (ptr) {
        memcpy(obj, ptr, oldSize);
    }

    return obj;
}

[[nodiscard]]
void* acalloc(Arena* arena, size_t count, size_t size) {
    size_t const totalSize = count * size;

    for (;/*ever*/;) {
        uintptr_t address = (uintptr_t)arena->free;
        size_t const align = alignof(max_align_t);
        address = (address + align - 1) & ~(align - 1); // Align `address` up

        uint8_t* newFree = (uint8_t*)(address + totalSize);
        if (newFree >= arena->limit) {
            if (totalSize < arena->blockSize) {
                arenaGrow(arena);
                continue;
            } else {
                return acallocHuge(&arena->hugeObjs, count, size);
            }
        }

        arena->free = newFree;
        void* const obj = (void*)address;
        memset(obj, 0, totalSize);
        return obj;
    }
}

} // namespace
