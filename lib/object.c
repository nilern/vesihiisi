typedef struct ORef {
    uint32_t bits;
} ORef;

static const uint32_t tag_width = 2;

static const uint32_t tag_bits = (1 << tag_width) - 1; // `tag_width` ones

static const uint32_t heap_tag = 0b01;

static const uint32_t flonum_tag = 0b10;

static const uint32_t miscScalar_tag = 0b11;

static const uint32_t char_tag = miscScalar_tag;

inline static bool isHeaped(ORef oref) { return (oref.bits & tag_bits) == heap_tag; }

inline static bool isFixnum(ORef oref) { return !(oref.bits & tag_bits); }

inline static bool isFlonum(ORef oref) { return (oref.bits & tag_bits) == flonum_tag; }

inline static bool isChar(ORef oref) { return (oref.bits & tag_bits) == char_tag; }

inline static bool isMiscScalar(ORef oref) { return (oref.bits & tag_bits) == miscScalar_tag; }

inline static ORef int32ToFixnum(int32_t n) { return (ORef){(uint32_t)n << tag_width}; }

inline static int32_t uncheckedFixnumToInt32(ORef v) { return (int32_t)v.bits >> tag_width; }

inline static ORef charToORef(char c) { return (ORef){((uint32_t)c << tag_width) | char_tag}; }

inline static char uncheckedORefToChar(ORef v) { return (char)(v.bits >> tag_width); }

typedef void* HeapRef;

typedef struct Header {
    uint32_t bits;
} Header;

static const uint32_t header_tag_bits = 0b11;

static const uint32_t slots_tag = 0b1;

static const uint32_t bytes_tag = 0b10;

inline static Header header(HeapRef href) { return *((Header*)href - 1); }

