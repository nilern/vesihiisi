static_assert(sizeof(void*) == sizeof(uint64_t)); // Only 64-bit supported (for now)

typedef enum Tag {
    TAG_FIXNUM = 0b000,
    TAG_FLONUM = 0b100,
    TAG_CHAR = 0b010,
    TAG_BOOL = 0b110,
    TAG_HEAPED = 0b001
} Tag;

typedef struct ORef { uintptr_t bits; } ORef;

typedef struct Fixnum { uintptr_t bits; } Fixnum;

typedef struct Char { uintptr_t bits; } Char;

typedef struct Bool { uintptr_t bits; } Bool;

static uintptr_t const tag_width = 3;
static uintptr_t const tag_bits = (1 << tag_width) - 1; // `tag_width` ones

inline static Tag getTag(ORef v) { return (Tag)(v.bits & tag_bits); }

inline static bool eq(ORef x, ORef y) { return x.bits == y.bits; }

inline static bool isFixnum(ORef v) { return getTag(v) == TAG_FIXNUM; }

inline static bool isChar(ORef v) { return getTag(v) == TAG_CHAR; }

inline static bool isBool(ORef v) { return getTag(v) == TAG_BOOL; }

inline static bool isHeaped(ORef v) { return getTag(v) == TAG_HEAPED; }

static Fixnum const Zero = {0};

static Bool const True = {((uintptr_t)true << tag_width) | (uintptr_t)TAG_BOOL};
static Bool const False = {((uintptr_t)false << tag_width) | (uintptr_t)TAG_BOOL};

inline static ORef boolToORef(Bool b) { return (ORef){b.bits}; }

inline static bool uncheckedORefToBool(ORef v) { return (bool)(v.bits >> tag_width); }

inline static ORef fixnumToORef(Fixnum n) { return (ORef){n.bits}; }

// FIXME: Handle overflow (fixnum is only 61 bits):
inline static Fixnum tagInt(intptr_t n) { return (Fixnum){(uintptr_t)n << tag_width}; }

inline static intptr_t fixnumToInt(Fixnum n) { return (intptr_t)(n.bits >> tag_width); }

inline static intptr_t uncheckedFixnumToInt(ORef v) { return (intptr_t)v.bits >> tag_width; }

inline static ORef charToORef(Char c) { return (ORef){c.bits}; }

inline static Char tagChar(char c) {
    return (Char){((uintptr_t)c << tag_width) | (uintptr_t)TAG_CHAR};
}

inline static char uncheckedORefToChar(ORef v) { return (char)(v.bits >> tag_width); }

inline static Bool tagBool(bool b) {
    return (Bool){((uintptr_t)b << tag_width) | (uintptr_t)TAG_BOOL};
}

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
    return (TypeRef){(uintptr_t)(void*)type  | (uintptr_t)TAG_HEAPED};
}

inline static Type* typeToPtr(TypeRef type) { return (Type*)(void*)(type.bits & ~tag_bits); }

inline static ORef typeToORef(TypeRef type) { return (ORef){type.bits}; }

inline static TypeRef uncheckedORefToTypeRef(ORef v) { return (TypeRef){v.bits}; }

typedef struct Header { uintptr_t bits; } Header;

inline static Header fixedHeader(Type const* type) {
    return (Header){(uintptr_t)(void*)type | (uintptr_t)TAG_HEAPED};
}

inline static TypeRef headerType(Header header) {
    return (TypeRef){(header.bits & ~tag_bits) | (uintptr_t)TAG_HEAPED};
}

typedef struct FlexHeader {
    Fixnum length;
    Header base;
} FlexHeader;

inline static FlexHeader flexHeader(Fixnum length, Type const* type) {
    return (FlexHeader){length, fixedHeader(type)};
}

static const size_t objectMinAlign = alignof(Header);

inline static Fixnum flexLength(ORef v) {
    void* const ptr = uncheckedORefToPtr(v);
    return ((FlexHeader*)ptr - 1)->length;
}

typedef struct StringRef { uintptr_t bits; } StringRef;

inline static StringRef tagString(char* s) {
    return (StringRef){(uintptr_t)(void*)s | (uintptr_t)TAG_HEAPED};
}

inline static ORef stringToORef(StringRef s) { return (ORef){s.bits}; }

inline static StringRef uncheckedORefToString(ORef v) { return (StringRef){v.bits}; }

static Str stringStr(StringRef s) {
    return (Str){
        .data = (char*)(void*)(s.bits & ~tag_bits),
        .len = (size_t)fixnumToInt(flexLength(stringToORef(s)))
    };
}

typedef struct Symbol {
    Fixnum hash;
    char name[];
} Symbol;

typedef struct SymbolRef { uintptr_t bits; } SymbolRef;

inline static SymbolRef tagSymbol(Symbol* ptr) {
    return (SymbolRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef symbolToORef(SymbolRef sym) { return (ORef){sym.bits}; }

inline static SymbolRef uncheckedORefToSymbol(ORef v) { return (SymbolRef){v.bits}; }

inline static Symbol* symbolToPtr(SymbolRef sym) { return (Symbol*)(void*)(sym.bits & ~tag_bits); }

inline static Str symbolName(SymbolRef sym) {
    return (Str){
        .data = symbolToPtr(sym)->name,
        .len = (uintptr_t)fixnumToInt(flexLength(symbolToORef(sym)))
    };
}

typedef struct ArrayRef { uintptr_t bits; } ArrayRef;

inline static ArrayRef tagArray(ORef* xs) {
    return (ArrayRef){(uintptr_t)(void*)xs | (uintptr_t)TAG_HEAPED};
}

inline static ORef arrayToORef(ArrayRef xs) { return (ORef){xs.bits}; }

inline static Fixnum arrayCount(ArrayRef xs) { return flexLength(arrayToORef(xs)); }

inline static ORef* arrayToPtr(ArrayRef xs) { return (ORef*)(void*)(xs.bits & ~tag_bits); }

typedef struct ByteArrayRef { uintptr_t bits; } ByteArrayRef;

inline static ByteArrayRef tagByteArray(uint8_t* bs) {
    return (ByteArrayRef){(uintptr_t)(void*)bs | (uintptr_t)TAG_HEAPED};
}

inline static ORef byteArrayToORef(ByteArrayRef bs) { return (ORef){bs.bits}; }

inline static Fixnum byteArrayCount(ByteArrayRef bs) { return flexLength(byteArrayToORef(bs)); }

inline static uint8_t* byteArrayToPtr(ByteArrayRef bs) {
    return (uint8_t*)(void*)(bs.bits & ~tag_bits);
}

typedef struct Pair {
    ORef car;
    ORef cdr;
} Pair;

typedef struct PairRef { uintptr_t bits; } PairRef;

inline static PairRef uncheckedORefToPair(ORef v) { return (PairRef){v.bits}; }

inline static Pair* pairToPtr(PairRef pair) { return (Pair*)(void*)(pair.bits & ~tag_bits); }

inline static PairRef tagPair(Pair* ptr) {
    return (PairRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef pairToORef(PairRef pair) { return (ORef){pair.bits}; }

typedef struct EmptyListRef { uintptr_t bits; } EmptyListRef;

inline static EmptyListRef tagEmptyList(void const* ptr) {
    return (EmptyListRef){(uintptr_t)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef emptyListToORef(EmptyListRef v) { return (ORef){v.bits}; }

typedef struct Method {
    ByteArrayRef code;
    ArrayRef consts;
} Method;

typedef struct MethodRef { uintptr_t bits; } MethodRef;

inline static MethodRef tagMethod(Method* ptr) {
    return (MethodRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef methodToORef(MethodRef v) { return (ORef){v.bits}; }

inline static MethodRef uncheckedORefToMethod(ORef v) { return (MethodRef){v.bits}; }

inline static Method* methodToPtr(MethodRef v) { return (Method*)(void*)(v.bits & ~tag_bits); }

typedef struct Closure {
    ORef method;
    ORef const clovers[];
} Closure;

typedef struct ClosureRef { uintptr_t bits; } ClosureRef;

inline static ORef closureToORef(ClosureRef v) { return (ORef){v.bits}; }

inline static ClosureRef uncheckedORefToClosure(ORef v) { return (ClosureRef){v.bits}; }

inline static Closure* closureToPtr(ClosureRef v) { return (Closure*)(void*)(v.bits & ~tag_bits); }

inline static ClosureRef tagClosure(Closure* ptr) {
    return (ClosureRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}
