#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#include "vesihiisi.h"

static_assert(sizeof(void*) == sizeof(uint64_t)); // Only 64-bit supported (for now)
// Could support 32-bit now that we NaN-tag. If we get native threads the non-atomicity of 64-bit
// loads and stores would complicate synchronization primitives on 32-bit though.

inline static bool eq(ORef x, ORef y) { return x.bits == y.bits; }

static uint64_t const tagMask;
static uint64_t const payloadWidth;
static uint64_t const payloadMask;

static uint64_t const nonFlonumTag;

static uint64_t const fixnumTag;
static uint64_t const charTag;
static uint64_t const boolTag;
static uint64_t const heapedTag;

typedef struct Fixnum { uint64_t bits; } Fixnum;
static_assert(sizeof(Fixnum) == sizeof(ORef));

typedef struct Flonum { double bits; } Flonum;
static_assert(sizeof(Flonum) == sizeof(ORef));

typedef struct Char { uint64_t bits; } Char;
static_assert(sizeof(Char) == sizeof(ORef));

typedef struct Bool { uint64_t bits; } Bool;
static_assert(sizeof(Bool) == sizeof(ORef));

inline static bool isFixnum(ORef v) { return (v.bits & tagMask) == fixnumTag; }

inline static bool isFlonum(ORef v) {
    return v.bits == nonFlonumTag // Actual NaN OPTIMIZE: Is this necessary?
           || (v.bits & nonFlonumTag) != nonFlonumTag; // OPTIMIZE: Do not short-circuit?
}

inline static bool isChar(ORef v) { return (v.bits & tagMask) == charTag; }

inline static bool isBool(ORef v) { return (v.bits & tagMask) == boolTag; }

inline static bool isHeaped(ORef v) {
    return v.bits != nonFlonumTag // Not a NaN OPTIMIZE: Is this necessary?
           && (v.bits & tagMask) == heapedTag; // OPTIMIZE: Do not short-circuit?
}

typedef enum TaggedType : uint64_t {
    TYPE_FLONUM = 0,
    TYPE_FIXNUM = 1,
    TYPE_CHAR = 2,
    TYPE_BOOL = 3,
    TYPE_HEAPED = 4
} TaggedType;

static inline TaggedType getTag(ORef v) {
    if (isHeaped(v)) { return TYPE_HEAPED; }
    if (isFlonum(v)) { return TYPE_FLONUM; }
    return (TaggedType)((v.bits >> payloadWidth) & 0b11);
}

static Fixnum const Zero;
static Fixnum const One;

static Bool const True;
static Bool const False;

static ORef const AlignmentHole;

static ORef const Tombstone;

inline static Fixnum uncheckedORefToFixnum(ORef v) { return (Fixnum){v.bits}; }

// FIXME: Handle overflow (fixnum is only 61 bits):
inline static Fixnum tagInt(int64_t n) { return (Fixnum){fixnumTag | (uint64_t)n}; }

inline static int64_t fixnumToInt(Fixnum n) { return (int64_t)(n.bits & payloadMask); }

inline static int64_t uncheckedFixnumToInt(ORef v) { return (int64_t)(v.bits & payloadMask); }

inline static double uncheckedFlonumToDouble(ORef v) {
    return ((union {uint64_t i; double f;}){.i = v.bits}).f; // Might not fly in C++
}

inline static Char tagChar(char c) { return (Char){charTag | (uint64_t)c}; }

inline static char uncheckedORefToChar(ORef v) { return (char)(v.bits & payloadMask); }

inline static Bool tagBool(bool b) { return (Bool){boolTag | (uint64_t)b}; }

inline static bool unwrapBool(Bool b) { return (bool)(b.bits & payloadMask); }

inline static bool uncheckedORefToBool(ORef v) { return (bool)(v.bits & payloadMask); }

inline static ORef tagHeaped(void* ptr) { return (ORef){heapedTag | (uint64_t)ptr}; }

inline static void* uncheckedORefToPtr(ORef v) { return (void*)(v.bits & payloadMask); }

inline static void* tryORefToPtr(ORef oref) {
    return isHeaped(oref) ? uncheckedORefToPtr(oref) : nullptr;
}

typedef struct SymbolRef { uint64_t bits; } SymbolRef;

typedef struct Type {
    Fixnum minSize;
    Fixnum align;
    Bool isBytes;
    Bool hasCodePtr;
    Bool isFlex;
    Fixnum hash;
    SymbolRef name;
} Type;

typedef struct TypeRef { uint64_t bits; } TypeRef;

inline static TypeRef tagType(Type const* type) {
    return (TypeRef){heapedTag | (uint64_t)(void*)type};
}

inline static Type* typeToPtr(TypeRef type) { return (Type*)(void*)(type.bits & payloadMask); }

inline static TypeRef uncheckedORefToType(ORef v) { return (TypeRef){v.bits}; }

typedef struct Header { uint64_t bits; } Header;

static uint64_t const markBit;

inline static Header fixedHeader(Type const* type) { return (Header){(uint64_t)(void*)type}; }

inline static Header relocationHeader(void* copy) {
    return (Header){(uint64_t)copy | markBit};
}

static void* tryForwarded(void* obj) {
    Header const header = *((Header*)obj - 1);
    return header.bits & markBit ? (void*)(header.bits & payloadMask) : nullptr;
}

inline static void forwardTo(void* obj, void* copy) {
    *((Header*)obj - 1) = relocationHeader(copy);
}

// Cannot work on broken hearts (as they have relocation pointer instead):
inline static TypeRef headerType(Header header) {
    return (TypeRef){heapedTag | (header.bits & payloadMask)};
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

typedef struct StringRef { uint64_t bits; } StringRef;

inline static StringRef tagString(char* s) { return (StringRef){heapedTag | (uint64_t)(void*)s}; }

inline static StringRef uncheckedORefToString(ORef v) { return (StringRef){v.bits}; }

static Str stringStr(StringRef s) {
    return (Str){
        .data = (char*)(void*)(s.bits & payloadMask),
        .len = (size_t)fixnumToInt(uncheckedFlexCount((ORef){s.bits}))
    };
}

typedef struct Symbol {
    Fixnum hash;
    char name[];
} Symbol;

inline static SymbolRef tagSymbol(Symbol* ptr) {
    return (SymbolRef){heapedTag | (uint64_t)(void*)ptr};
}

inline static SymbolRef uncheckedORefToSymbol(ORef v) { return (SymbolRef){v.bits}; }

inline static Symbol* symbolToPtr(SymbolRef sym) {
    return (Symbol*)(void*)(sym.bits & payloadMask);
}

inline static Str symbolName(SymbolRef sym) {
    return (Str){
        .data = symbolToPtr(sym)->name,
        .len = (uint64_t)fixnumToInt(uncheckedFlexCount((ORef){sym.bits}))
    };
}

typedef struct { uint64_t bits; } ArrayRef;

inline static ArrayRef uncheckedORefToArray(ORef v) { return (ArrayRef){v.bits}; }

inline static Fixnum arrayCount(ArrayRef xs) { return uncheckedFlexCount((ORef){xs.bits}); }

inline static ORef const* arrayToPtr(ArrayRef xs) {
    return (ORef const*)(void const*)(xs.bits & payloadMask);
}

typedef struct ArrayMutRef { uint64_t bits; } ArrayMutRef;

inline static ArrayMutRef tagArrayMut(ORef* xs) {
    return (ArrayMutRef){heapedTag | (uint64_t)(void*)xs};
}

inline static ArrayMutRef uncheckedORefToArrayMut(ORef v) { return (ArrayMutRef){v.bits}; }

inline static Fixnum arrayMutCount(ArrayMutRef xs) { return uncheckedFlexCount((ORef){xs.bits}); }

inline static ORef* arrayMutToPtr(ArrayMutRef xs) { return (ORef*)(void*)(xs.bits & payloadMask); }

typedef struct ByteArrayRef { uint64_t bits; } ByteArrayRef;

inline static ByteArrayRef tagByteArray(uint8_t* bs) {
    return (ByteArrayRef){heapedTag | (uint64_t)(void*)bs};
}

inline static ByteArrayRef uncheckedORefToByteArray(ORef v) { return (ByteArrayRef){v.bits}; }

inline static Fixnum byteArrayCount(ByteArrayRef bs) { return uncheckedFlexCount((ORef){bs.bits}); }

inline static uint8_t const* byteArrayToPtr(ByteArrayRef bs) {
    return (uint8_t const*)(void const*)(bs.bits & payloadMask);
}

typedef struct Pair {
    ORef car;
    ORef cdr;
} Pair;

typedef struct PairRef { uint64_t bits; } PairRef;

inline static PairRef uncheckedORefToPair(ORef v) { return (PairRef){v.bits}; }

inline static Pair* pairToPtr(PairRef pair) { return (Pair*)(void*)(pair.bits & payloadMask); }

inline static PairRef tagPair(Pair* ptr) { return (PairRef){heapedTag | (uint64_t)(void*)ptr}; }

typedef struct EmptyListRef { uint64_t bits; } EmptyListRef;

inline static EmptyListRef tagEmptyList(void const* ptr) {
    return (EmptyListRef){heapedTag | (uint64_t)ptr};
}

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

typedef struct MethodRef { uint64_t bits; } MethodRef;

inline static MethodRef tagMethod(Method* ptr) {
    return (MethodRef){heapedTag | (uint64_t)(void*)ptr};
}

inline static MethodRef uncheckedORefToMethod(ORef v) { return (MethodRef){v.bits}; }

inline static Method* methodToPtr(MethodRef v) { return (Method*)(void*)(v.bits & payloadMask); }

typedef struct Closure {
    ORef method;
    ORef const clovers[];
} Closure;

typedef struct ClosureRef { uint64_t bits; } ClosureRef;

inline static ClosureRef uncheckedORefToClosure(ORef v) { return (ClosureRef){v.bits}; }

inline static Closure* closureToPtr(ClosureRef v) { return (Closure*)(void*)(v.bits & payloadMask); }

inline static ClosureRef tagClosure(Closure* ptr) {
    return (ClosureRef){heapedTag | (uint64_t)(void*)ptr};
}

typedef struct {
    ArrayRef methods;
} Multimethod;

typedef struct { uint64_t bits; } MultimethodRef;

inline static MultimethodRef uncheckedORefToMultimethod(ORef v) {
    return (MultimethodRef){v.bits};
}

inline static Multimethod* multimethodToPtr(MultimethodRef v) {
    return (Multimethod*)(void*)(v.bits & payloadMask);
}

typedef struct Continuation {
    ORef method;
    Fixnum pc;
    ORef const saves[];
} Continuation;

typedef struct ContinuationRef { uint64_t bits; } ContinuationRef;

inline static ContinuationRef uncheckedORefToContinuation(ORef v) {
    return (ContinuationRef){v.bits};
}

inline static Continuation* continuationToPtr(ContinuationRef v) {
    return (Continuation*)(void*)(v.bits & payloadMask);
}

inline static ContinuationRef tagContinuation(Continuation* ptr) {
    return (ContinuationRef){heapedTag | (uint64_t)(void*)ptr};
}

typedef struct UnboundRef { uint64_t bits; } UnboundRef;

inline static UnboundRef tagUnbound(void const* ptr) {
    return (UnboundRef){heapedTag | (uint64_t)ptr};
}

typedef struct Var {
    ORef val;
} Var;

typedef struct VarRef { uint64_t bits; } VarRef;

inline static VarRef uncheckedORefToVar(ORef v) { return (VarRef){v.bits}; }

inline static VarRef tagVar(Var* ptr) { return (VarRef){heapedTag | (uint64_t)(void*)ptr}; }

inline static Var* varToPtr(VarRef v) { return (Var*)(void*)(v.bits & payloadMask); }

typedef struct Knot {
    ORef val;
} Knot;

typedef struct KnotRef { uint64_t bits; } KnotRef;

inline static KnotRef uncheckedORefToKnot(ORef v) { return (KnotRef){v.bits}; }

inline static KnotRef tagKnot(Knot* ptr) {
    return (KnotRef){heapedTag | (uint64_t)(void*)ptr};
}

inline static Knot* knotToPtr(KnotRef v) { return (Knot*)(void*)(v.bits & payloadMask); }

typedef struct Namespace {
    ArrayMutRef keys;
    ArrayMutRef vals;
    Fixnum count;
} Namespace;

typedef struct NamespaceRef { uint64_t bits; } NamespaceRef;

inline static NamespaceRef tagNamespace(Namespace* ptr) {
    return (NamespaceRef){heapedTag | (uint64_t)(void*)ptr};
}

inline static NamespaceRef uncheckedORefToNamespace(ORef v) { return (NamespaceRef){v.bits}; }

inline static Namespace* namespaceToPtr(NamespaceRef v) {
    return (Namespace*)(void*)(v.bits & payloadMask);
}

typedef struct {
    SymbolRef name;
} UnboundError;

typedef struct { uint64_t bits; } UnboundErrorRef;

inline static UnboundErrorRef tagUnboundError(UnboundError* ptr) {
    return (UnboundErrorRef){heapedTag | (uint64_t)(void*)ptr};
}

inline static UnboundErrorRef uncheckedORefToUnboundError(ORef v) {
    return (UnboundErrorRef){v.bits};
}

inline static UnboundError* unboundErrorToPtr(UnboundErrorRef v) {
    return (UnboundError*)(void*)(v.bits & payloadMask);
}

typedef struct TypeError {
    TypeRef type;
    ORef val;
} TypeError;

typedef struct TypeErrorRef { uint64_t bits; } TypeErrorRef;

inline static TypeErrorRef tagTypeError(TypeError* ptr) {
    return (TypeErrorRef){heapedTag | (uint64_t)(void*)ptr};
}

inline static TypeErrorRef uncheckedORefToTypeError(ORef v) { return (TypeErrorRef){v.bits}; }

inline static TypeError* typeErrorToPtr(TypeErrorRef v) {
    return (TypeError*)(void*)(v.bits & payloadMask);
}

typedef struct ArityError {
    ClosureRef callee;
    Fixnum callArgc;
} ArityError;

typedef struct ArityErrorRef { uint64_t bits; } ArityErrorRef;

inline static ArityErrorRef tagArityError(ArityError* ptr) {
    return (ArityErrorRef){heapedTag | (uint64_t)(void*)ptr};
}

inline static ArityErrorRef uncheckedORefToArityError(ORef v) { return (ArityErrorRef){v.bits}; }

inline static ArityError* arityErrorToPtr(ArityErrorRef v) {
    return (ArityError*)(void*)(v.bits & payloadMask);
}

typedef struct {
    MultimethodRef callee;
} InapplicableError;

typedef struct { uint64_t bits; } InapplicableErrorRef;

inline static InapplicableErrorRef tagInapplicableError(InapplicableError* ptr) {
    return (InapplicableErrorRef){heapedTag | (uint64_t)(void*)ptr};
}

inline static InapplicableErrorRef uncheckedORefToInapplicableError(ORef v) {
    return (InapplicableErrorRef){v.bits};
}

inline static InapplicableError* inapplicableErrorToPtr(InapplicableErrorRef v) {
    return (InapplicableError*)(void*)(v.bits & payloadMask);
}

#define uncheckedToORef(v) (ORef){(v).bits}

#define toORef(v) _Generic((v), \
    Fixnum: uncheckedToORef(v), \
    Char: uncheckedToORef(v), \
    Bool: uncheckedToORef(v), \
    TypeRef: uncheckedToORef(v), \
    StringRef: uncheckedToORef(v), \
    SymbolRef: uncheckedToORef(v), \
    ArrayRef: uncheckedToORef(v), \
    ArrayMutRef: uncheckedToORef(v), \
    ByteArrayRef: uncheckedToORef(v), \
    PairRef: uncheckedToORef(v), \
    EmptyListRef: uncheckedToORef(v), \
    MethodRef: uncheckedToORef(v), \
    ClosureRef: uncheckedToORef(v), \
    ContinuationRef: uncheckedToORef(v), \
    MultimethodRef: uncheckedToORef(v), \
    KnotRef: uncheckedToORef(v), \
    NamespaceRef: uncheckedToORef(v), \
    UnboundRef: uncheckedToORef(v), \
    VarRef: uncheckedToORef(v), \
    UnboundErrorRef: uncheckedToORef(v), \
    ArityErrorRef: uncheckedToORef(v), \
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
