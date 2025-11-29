static_assert(sizeof(void*) == sizeof(uint64_t)); // Only 64-bit supported (for now)

typedef struct ORef { uintptr_t bits; } ORef;

typedef struct Fixnum { uintptr_t bits; } Fixnum;

typedef struct Char { uintptr_t bits; } Char;

typedef struct Bool { uintptr_t bits; } Bool;

static uintptr_t const tag_width = 3;
static uintptr_t const tag_bits = (1 << tag_width) - 1; // `tag_width` ones

static uintptr_t const char_tag = 0b010;
static uintptr_t const bool_tag = 0b110;
static uintptr_t const heap_tag = 0b001;

inline static bool eq(ORef x, ORef y) { return x.bits == y.bits; }

inline static bool isFixnum(ORef oref) { return !(oref.bits & tag_bits); }

inline static bool isChar(ORef oref) { return (oref.bits & tag_bits) == char_tag; }

inline static bool isBool(ORef oref) { return (oref.bits & tag_bits) == bool_tag; }

inline static bool isHeaped(ORef oref) { return (oref.bits & tag_bits) == heap_tag; }

static Bool const True = {((uintptr_t)true << tag_width) | bool_tag};
static Bool const False = {((uintptr_t)false << tag_width) | bool_tag};

inline static ORef boolToORef(Bool b) { return (ORef){b.bits}; }

inline static bool uncheckedORefToBool(ORef v) { return (bool)(v.bits >> tag_width); }

inline static ORef fixnumToORef(Fixnum n) { return (ORef){n.bits}; }

// FIXME: Handle overflow (fixnum is only 61 bits):
inline static Fixnum tagInt(intptr_t n) { return (Fixnum){(uintptr_t)n << tag_width}; }

inline static intptr_t fixnumToInt(Fixnum n) { return (intptr_t)(n.bits >> tag_width); }

inline static intptr_t uncheckedFixnumToInt(ORef v) { return (intptr_t)v.bits >> tag_width; }

inline static ORef charToORef(Char c) { return (ORef){c.bits}; }

inline static Char tagChar(char c) { return (Char){((uintptr_t)c << tag_width) | char_tag}; }

inline static char uncheckedORefToChar(ORef v) { return (char)(v.bits >> tag_width); }

inline static Bool tagBool(bool b) { return (Bool){((uintptr_t)b << tag_width) | bool_tag}; }

inline static bool unwrapBool(Bool b) { return (bool)(b.bits >> tag_width); }

inline static void* uncheckedORefToPtr(ORef oref) { return (void*)(oref.bits & ~tag_bits); }

inline static void* tryORefToPtr(ORef oref) {
    return isHeaped(oref) ? uncheckedORefToPtr(oref) : nullptr;
}

typedef struct Type {
    Fixnum minSize;
    Fixnum align;
    Bool isBytes;
    Bool isFlex;
} Type;

typedef struct TypeRef { uintptr_t bits; } TypeRef;

inline static TypeRef tagType(Type const* type) {
    return (TypeRef){(uintptr_t)(void*)type  | heap_tag};
}

inline static Type* typeToPtr(TypeRef type) { return (Type*)(void*)(type.bits & ~tag_bits); }

inline static ORef typeToORef(TypeRef type) { return (ORef){type.bits}; }

typedef struct Header { uintptr_t bits; } Header;

inline static Header fixedHeader(Type const* type) {
    return (Header){(uintptr_t)(void*)type | heap_tag};
}

inline static TypeRef headerType(Header header) {
    return (TypeRef){(header.bits & ~tag_bits) | heap_tag};
}

typedef struct FlexHeader {
    Fixnum length;
    Header base;
} FlexHeader;

inline static FlexHeader flexHeader(Fixnum length, Type const* type) {
    return (FlexHeader){length, fixedHeader(type)};
}

[[maybe_unused]] // FIXME
static TypeRef typeOf(ORef oref) {
    void* const ptr = tryORefToPtr(oref);
    if (ptr) {
        return headerType(*((Header*)ptr - 1));
    } else {
        assert(false); // FIXME
    }
}

static const size_t objectMinAlign = alignof(Header);

inline static Fixnum flexLength(ORef v) {
    assert(unwrapBool(typeToPtr(typeOf(v))->isFlex));
    
    void* const ptr = uncheckedORefToPtr(v);
    return ((FlexHeader*)ptr - 1)->length;
}

typedef struct StringRef { uintptr_t bits; } StringRef;

inline static bool isString(Type const* stringType, ORef v) {
    return typeToPtr(typeOf(v)) == stringType;
}

inline static StringRef tagString(char* s) { return (StringRef){(uintptr_t)(void*)s | heap_tag}; }

inline static ORef stringToORef(StringRef s) { return (ORef){s.bits}; }

inline static StringRef uncheckedORefToString(ORef v) { return (StringRef){v.bits}; }

static Str stringStr(StringRef s) {
    return (Str){
        .data = (char*)(void*)(s.bits & ~tag_bits),
        .len = (size_t)fixnumToInt(flexLength(stringToORef(s)))
    };
}

