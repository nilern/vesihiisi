#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct BytefulBitSet {
    uint8_t* bytes;
    size_t byteCount;
    size_t byteCap;
} BytefulBitSet;

static BytefulBitSet newBytefulBitSet(size_t cap);

inline static void freeBytefulBitSet(BytefulBitSet* bits) { free(bits->bytes); }

inline static size_t bytefulBitSetByteCount(BytefulBitSet const* bits) { return bits->byteCount; }

inline static uint8_t bytefulBitSetByte(BytefulBitSet const* bits, size_t i) {
    assert(i < bits->byteCount);

    return bits->bytes[i];
}

static void bytefulBitSetSet(BytefulBitSet* bits, size_t n);
