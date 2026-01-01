#pragma once

#include <stddef.h>
#include <stdint.h>

typedef struct ArenaFullBlocks {
    uint8_t** vals;
    size_t count;
    size_t cap;
} ArenaFullBlocks;

typedef struct ArenaHugeObj {
    size_t size;
    void* val;
} ArenaHugeObj;

typedef struct ArenaHugeObjs {
    ArenaHugeObj* vals;
    size_t count;
    size_t cap;
} ArenaHugeObjs;

typedef struct Arena {
    uint8_t* free;
    uint8_t* limit;
    uint8_t* start;
    size_t blockSize;
    ArenaFullBlocks fullBlocks;
    ArenaHugeObjs hugeObjs;
} Arena;

static Arena newArena(size_t blockSize);

static void freeArena(Arena* arena);

static size_t const defaultArenaBlockSize;

[[nodiscard]]
static void* amalloc(Arena* arena, size_t size);

[[nodiscard]]
static void* arealloc(Arena* arena, void* ptr, size_t oldSize, size_t size);

[[nodiscard]]
static void* acalloc(Arena* arena, size_t count, size_t size);
