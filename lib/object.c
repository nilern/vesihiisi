#include <stdint.h>

typedef struct ORef {
    uint32_t bits;
} ORef;

static const uint32_t tag_bits = 0b11;

static const uint32_t heap_tag = 0b01;

static const uint32_t flonum_tag = 0b10;

static const uint32_t miscScalar_tag = 0b11;

inline static bool isHeaped(ORef oref) { return (oref.bits & tag_bits) == heap_tag; }

inline static bool isFixnum(ORef oref) { return !(oref.bits & tag_bits); }

inline static bool isFlonum(ORef oref) { return (oref.bits & tag_bits) == flonum_tag; }

inline static bool isMiscScalar(ORef oref) { return (oref.bits & tag_bits) == miscScalar_tag; }

typedef void* HeapRef;

typedef struct Header {
    uint32_t bits;
} Header;

static const uint32_t header_tag_bits = 0b11;

static const uint32_t slots_tag = 0b1;

static const uint32_t bytes_tag = 0b10;

inline static Header header(HeapRef href) { return *((Header*)href - 1); }

