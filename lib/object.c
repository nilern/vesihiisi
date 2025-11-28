typedef struct ORef {
    uint32_t bits;
} ORef;

static const uint32_t tag_width = 3;

static const uint32_t tag_bits = (1 << tag_width) - 1; // `tag_width` ones

static const uint32_t char_tag = 0b010;

static const uint32_t bool_tag = 0b110;

inline static bool isFixnum(ORef oref) { return !(oref.bits & tag_bits); }

inline static bool isChar(ORef oref) { return (oref.bits & tag_bits) == char_tag; }

inline static bool isBool(ORef oref) { return (oref.bits & tag_bits) == bool_tag; }

inline static bool uncheckedORefToBool(ORef v) { return (bool)(v.bits >> tag_width); }

inline static ORef int32ToFixnum(int32_t n) { return (ORef){(uint32_t)n << tag_width}; }

inline static int32_t uncheckedFixnumToInt32(ORef v) { return (int32_t)v.bits >> tag_width; }

inline static ORef charToORef(char c) { return (ORef){((uint32_t)c << tag_width) | char_tag}; }

inline static char uncheckedORefToChar(ORef v) { return (char)(v.bits >> tag_width); }

inline static ORef boolToORef(bool b) { return (ORef){((uint32_t)b << tag_width) | bool_tag}; }

