#include "bytefulbitset.h"

#include <string.h>

static BytefulBitSet newBytefulBitSet(size_t cap) {
    size_t byteCap = cap / UINT8_WIDTH;
    if (byteCap < 2) { byteCap = 2; }
    uint8_t* const bytes = calloc(byteCap, sizeof *bytes);

    return (BytefulBitSet){.bytes = bytes, .byteCount = 0, .byteCap = byteCap};
}

static void bytefulBitSetSet(BytefulBitSet* bits, size_t n) {
    size_t const byteIdx = n / UINT8_WIDTH;

    if (byteIdx >= bits->byteCap) { // Does not even fit allocation => grow:
        size_t const newCap = bits->byteCap + bits->byteCap / 2;
        uint8_t* const newBytes = malloc(newCap * sizeof *newBytes);

        memcpy(newBytes, bits->bytes, bits->byteCount * sizeof *newBytes);

        free(bits->bytes);
        bits->bytes = newBytes;
        bits->byteCap = newCap;
    }

    if (byteIdx >= bits->byteCount) { // In uninitialized => zero-initialize up to:
        size_t const newCount = byteIdx + 1;
        memset(
            bits->bytes + bits->byteCount,
            0,
            (newCount - bits->byteCount) * sizeof *bits->bytes
        );
        bits->byteCount = newCount;
    }

    size_t const subIdx = n % UINT8_WIDTH;
    bits->bytes[byteIdx] |= 1u << (UINT8_WIDTH - 1 - subIdx);
}
