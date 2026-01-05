#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#include "vesihiisi.h"

static_assert(sizeof(void*) == sizeof(uint64_t)); // Only 64-bit supported (for now)

typedef enum Tag : uintptr_t {
    TAG_FIXNUM = 0b000,
    TAG_FLONUM = 0b100,
    TAG_CHAR = 0b010,
    TAG_BOOL = 0b110,
    TAG_HEAPED = 0b001,
    TAG_BROKEN_HEART = 0b011
} Tag;

typedef struct Fixnum { uintptr_t bits; } Fixnum;

typedef struct Char { uintptr_t bits; } Char;

typedef struct Bool { uintptr_t bits; } Bool;

static uintptr_t const tag_width;
static uintptr_t const tag_bits;

static uintptr_t const markBit;

inline static Tag getTag(ORef v) { return (Tag)(v.bits & tag_bits); }

inline static bool eq(ORef x, ORef y) { return x.bits == y.bits; }

inline static bool isFixnum(ORef v) { return getTag(v) == TAG_FIXNUM; }

inline static bool isChar(ORef v) { return getTag(v) == TAG_CHAR; }

inline static bool isBool(ORef v) { return getTag(v) == TAG_BOOL; }

inline static bool isHeaped(ORef v) { return getTag(v) == TAG_HEAPED; }

static Fixnum const Zero;
static Fixnum const One;

static Bool const True;
static Bool const False;

static ORef const AlignmentHole;

static ORef const Tombstone;

inline static ORef boolToORef(Bool b) { return (ORef){b.bits}; }

inline static bool uncheckedORefToBool(ORef v) { return (bool)(v.bits >> tag_width); }

inline static ORef fixnumToORef(Fixnum n) { return (ORef){n.bits}; }

inline static Fixnum uncheckedORefToFixnum(ORef v) { return (Fixnum){v.bits}; }

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

inline static ORef tagHeaped(void* ptr) { return (ORef){(uintptr_t)ptr | (uintptr_t)TAG_HEAPED}; }

inline static void* uncheckedORefToPtr(ORef oref) { return (void*)(oref.bits & ~tag_bits); }

inline static void* tryORefToPtr(ORef oref) {
    return isHeaped(oref) ? uncheckedORefToPtr(oref) : nullptr;
}

typedef struct SymbolRef { uintptr_t bits; } SymbolRef;

typedef struct Type {
    Fixnum minSize;
    Fixnum align;
    Bool isBytes;
    Bool hasCodePtr;
    Bool isFlex;
    Fixnum hash;
    SymbolRef name;
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

inline static Header relocationHeader(void* copy) {
    return (Header){(uintptr_t)copy | (uintptr_t)TAG_BROKEN_HEART};
}

static void* tryForwarded(void* obj) {
    Header const header = *((Header*)obj - 1);
    return header.bits & markBit ? (void*)(header.bits & ~tag_bits) : nullptr;
}

inline static void forwardTo(void* obj, void* copy) {
    *((Header*)obj - 1) = relocationHeader(copy);
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

static const size_t objectMinAlign;

inline static Fixnum uncheckedFlexCount(ORef v) {
    assert(isHeaped(v)
           && unwrapBool(typeToPtr(headerType(*((Header*)uncheckedORefToPtr(v) - 1)))->isFlex));
    void* const ptr = uncheckedORefToPtr(v);
    return ((FlexHeader*)ptr - 1)->length;
}

static char const* uncheckedUntypedFlexPtr(ORef v) {
    assert(isHeaped(v)
           && unwrapBool(typeToPtr(headerType(*((Header*)uncheckedORefToPtr(v) - 1)))->isFlex));
    char const* const ptr = uncheckedORefToPtr(v);
    return ptr + fixnumToInt(typeToPtr(headerType(*((Header*)ptr - 1)))->minSize);
}

static char* uncheckedUntypedFlexPtrMut(ORef v) {
    assert(isHeaped(v)
           && unwrapBool(typeToPtr(headerType(*((Header*)uncheckedORefToPtr(v) - 1)))->isFlex));
    char* const ptr = uncheckedORefToPtr(v);
    return ptr + fixnumToInt(typeToPtr(headerType(*((Header*)ptr - 1)))->minSize);
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
        .len = (size_t)fixnumToInt(uncheckedFlexCount(stringToORef(s)))
    };
}

typedef struct Symbol {
    Fixnum hash;
    char name[];
} Symbol;

inline static SymbolRef tagSymbol(Symbol* ptr) {
    return (SymbolRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef symbolToORef(SymbolRef sym) { return (ORef){sym.bits}; }

inline static SymbolRef uncheckedORefToSymbol(ORef v) { return (SymbolRef){v.bits}; }

inline static Symbol* symbolToPtr(SymbolRef sym) { return (Symbol*)(void*)(sym.bits & ~tag_bits); }

inline static Str symbolName(SymbolRef sym) {
    return (Str){
        .data = symbolToPtr(sym)->name,
        .len = (uintptr_t)fixnumToInt(uncheckedFlexCount(symbolToORef(sym)))
    };
}

typedef struct { uintptr_t bits; } ArrayRef;

inline static ORef arrayToORef(ArrayRef xs) { return (ORef){xs.bits}; }

inline static ArrayRef uncheckedORefToArray(ORef v) { return (ArrayRef){v.bits}; }

inline static Fixnum arrayCount(ArrayRef xs) { return uncheckedFlexCount(arrayToORef(xs)); }

inline static ORef const* arrayToPtr(ArrayRef xs) {
    return (ORef const*)(void const*)(xs.bits & ~tag_bits);
}

typedef struct ArrayMutRef { uintptr_t bits; } ArrayMutRef;

inline static ArrayMutRef tagArrayMut(ORef* xs) {
    return (ArrayMutRef){(uintptr_t)(void*)xs | (uintptr_t)TAG_HEAPED};
}

inline static ORef arrayMutToORef(ArrayMutRef xs) { return (ORef){xs.bits}; }

inline static ArrayMutRef uncheckedORefToArrayMut(ORef v) { return (ArrayMutRef){v.bits}; }

inline static Fixnum arrayMutCount(ArrayMutRef xs) { return uncheckedFlexCount(arrayMutToORef(xs)); }

inline static ORef* arrayMutToPtr(ArrayMutRef xs) { return (ORef*)(void*)(xs.bits & ~tag_bits); }

typedef struct ByteArrayRef { uintptr_t bits; } ByteArrayRef;

inline static ByteArrayRef tagByteArray(uint8_t* bs) {
    return (ByteArrayRef){(uintptr_t)(void*)bs | (uintptr_t)TAG_HEAPED};
}

inline static ORef byteArrayToORef(ByteArrayRef bs) { return (ORef){bs.bits}; }

inline static ByteArrayRef uncheckedORefToByteArray(ORef v) { return (ByteArrayRef){v.bits}; }

inline static Fixnum byteArrayCount(ByteArrayRef bs) { return uncheckedFlexCount(byteArrayToORef(bs)); }

inline static uint8_t const* byteArrayToPtr(ByteArrayRef bs) {
    return (uint8_t const*)(void const*)(bs.bits & ~tag_bits);
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

typedef enum PrimopRes {
    PRIMOP_RES_CONTINUE,
    PRIMOP_RES_TAILCALL,
    PRIMOP_RES_TAILAPPLY,
    PRIMOP_RES_ABORT
} PrimopRes;

struct State;

typedef PrimopRes (*MethodCode)(struct State*);

typedef struct Method {
    MethodCode nativeCode;
    ORef code;
    ORef consts;
    Bool hasVarArg;
    Fixnum hash;
    ORef maybeName;
    ORef domain[];
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

typedef struct {
    ArrayRef methods;
} Multimethod;

typedef struct { uintptr_t bits; } MultimethodRef;

inline static MultimethodRef uncheckedORefToMultimethod(ORef v) {
    return (MultimethodRef){v.bits};
}

inline static Multimethod* multimethodToPtr(MultimethodRef v) {
    return (Multimethod*)(void*)(v.bits & ~tag_bits);
}

typedef struct Continuation {
    ORef method;
    Fixnum pc;
    ORef const saves[];
} Continuation;

typedef struct ContinuationRef { uintptr_t bits; } ContinuationRef;

inline static ORef continuationToORef(ContinuationRef v) { return (ORef){v.bits}; }

inline static ContinuationRef uncheckedORefToContinuation(ORef v) {
    return (ContinuationRef){v.bits};
}

inline static Continuation* continuationToPtr(ContinuationRef v) {
    return (Continuation*)(void*)(v.bits & ~tag_bits);
}

inline static ContinuationRef tagContinuation(Continuation* ptr) {
    return (ContinuationRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

typedef struct UnboundRef { uintptr_t bits; } UnboundRef;

inline static UnboundRef tagUnbound(void const* ptr) {
    return (UnboundRef){(uintptr_t)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef unboundToORef(UnboundRef v) { return (ORef){v.bits}; }

typedef struct Var {
    ORef val;
} Var;

typedef struct VarRef { uintptr_t bits; } VarRef;

inline static ORef varToORef(VarRef v) { return (ORef){v.bits}; }

inline static VarRef uncheckedORefToVar(ORef v) { return (VarRef){v.bits}; }

inline static VarRef tagVar(Var* ptr) {
    return (VarRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static Var* varToPtr(VarRef v) { return (Var*)(void*)(v.bits & ~tag_bits); }

typedef struct Knot {
    ORef val;
} Knot;

typedef struct KnotRef { uintptr_t bits; } KnotRef;

inline static KnotRef uncheckedORefToKnot(ORef v) { return (KnotRef){v.bits}; }

inline static KnotRef tagKnot(Knot* ptr) {
    return (KnotRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static Knot* knotToPtr(KnotRef v) { return (Knot*)(void*)(v.bits & ~tag_bits); }

typedef struct Namespace {
    ArrayMutRef keys;
    ArrayMutRef vals;
    Fixnum count;
} Namespace;

typedef struct NamespaceRef { uintptr_t bits; } NamespaceRef;

inline static NamespaceRef tagNamespace(Namespace* ptr) {
    return (NamespaceRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef namespaceToORef(NamespaceRef v) { return (ORef){v.bits}; }

inline static NamespaceRef uncheckedORefToNamespace(ORef v) { return (NamespaceRef){v.bits}; }

inline static Namespace* namespaceToPtr(NamespaceRef v) {
    return (Namespace*)(void*)(v.bits & ~tag_bits);
}

typedef struct {
    SymbolRef name;
} UnboundError;

typedef struct { uintptr_t bits; } UnboundErrorRef;

inline static UnboundErrorRef tagUnboundError(UnboundError* ptr) {
    return (UnboundErrorRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static UnboundErrorRef uncheckedORefToUnboundError(ORef v) {
    return (UnboundErrorRef){v.bits};
}

inline static UnboundError* unboundErrorToPtr(UnboundErrorRef v) {
    return (UnboundError*)(void*)(v.bits & ~tag_bits);
}

typedef struct TypeError {
    TypeRef type;
    ORef val;
} TypeError;

typedef struct TypeErrorRef { uintptr_t bits; } TypeErrorRef;

inline static TypeErrorRef tagTypeError(TypeError* ptr) {
    return (TypeErrorRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef typeErrorToORef(TypeErrorRef v) { return (ORef){v.bits}; }

inline static TypeErrorRef uncheckedORefToTypeError(ORef v) { return (TypeErrorRef){v.bits}; }

inline static TypeError* typeErrorToPtr(TypeErrorRef v) {
    return (TypeError*)(void*)(v.bits & ~tag_bits);
}

typedef struct ArityError {
    ClosureRef callee;
    Fixnum callArgc;
} ArityError;

typedef struct ArityErrorRef { uintptr_t bits; } ArityErrorRef;

inline static ArityErrorRef tagArityError(ArityError* ptr) {
    return (ArityErrorRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static ORef arityErrorToORef(ArityErrorRef v) { return (ORef){v.bits}; }

inline static ArityErrorRef uncheckedORefToArityError(ORef v) { return (ArityErrorRef){v.bits}; }

inline static ArityError* arityErrorToPtr(ArityErrorRef v) {
    return (ArityError*)(void*)(v.bits & ~tag_bits);
}

typedef struct {
    MultimethodRef callee;
} InapplicableError;

typedef struct { uintptr_t bits; } InapplicableErrorRef;

inline static InapplicableErrorRef tagInapplicableError(InapplicableError* ptr) {
    return (InapplicableErrorRef){(uintptr_t)(void*)ptr | (uintptr_t)TAG_HEAPED};
}

inline static InapplicableErrorRef uncheckedORefToInapplicableError(ORef v) {
    return (InapplicableErrorRef){v.bits};
}

inline static InapplicableError* inapplicableErrorToPtr(InapplicableErrorRef v) {
    return (InapplicableError*)(void*)(v.bits & ~tag_bits);
}

#define uncheckedToORef(v) (ORef){(v).bits}

#define toORef(v) _Generic((v), \
    Fixnum: uncheckedToORef(v), \
    Bool: uncheckedToORef(v), \
    TypeRef: uncheckedToORef(v), \
    SymbolRef: uncheckedToORef(v), \
    ArrayRef: uncheckedToORef(v), \
    ArrayMutRef: uncheckedToORef(v), \
    ByteArrayRef: uncheckedToORef(v), \
    MethodRef: uncheckedToORef(v), \
    ClosureRef: uncheckedToORef(v), \
    MultimethodRef: uncheckedToORef(v), \
    KnotRef: uncheckedToORef(v), \
    UnboundErrorRef: uncheckedToORef(v), \
    TypeErrorRef: uncheckedToORef(v), \
    InapplicableErrorRef: uncheckedToORef(v))

#define toPtr(v) _Generic((v), \
    TypeRef: typeToPtr, \
    SymbolRef: symbolToPtr, \
    ArrayMutRef: arrayMutToPtr, \
    ArrayRef: arrayToPtr, \
    ByteArrayRef: byteArrayToPtr, \
    PairRef: pairToPtr, \
    ClosureRef: closureToPtr, \
    MultimethodRef: multimethodToPtr, \
    MethodRef: methodToPtr, \
    KnotRef: knotToPtr, \
    UnboundErrorRef: unboundErrorToPtr, \
    InapplicableErrorRef: inapplicableErrorToPtr \
    )(v)
