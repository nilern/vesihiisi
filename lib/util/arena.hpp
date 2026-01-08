#pragma once

#include <stddef.h>
#include <stdint.h>

namespace {

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

Arena newArena(size_t blockSize);

void freeArena(Arena* arena);

constexpr size_t defaultArenaBlockSize = 4096; // 4 KiB, a common page size;

[[nodiscard]]
void* amalloc(Arena* arena, size_t size);

[[nodiscard]]
void* arealloc(Arena* arena, void* ptr, size_t oldSize, size_t size);

[[nodiscard]]
void* acalloc(Arena* arena, size_t count, size_t size);

} // namespace
