#pragma once

#include "heap.h"
#include "object.h"

typedef struct SymbolTable {
    ORef* entries;
    size_t count;
    size_t cap;
} SymbolTable;

typedef struct Shadowstack {
    ORef** vals;
    size_t count;
    size_t cap;
} Shadowstack;

#define REG_COUNT 256

#define BOOTSTRAP_TYPE_COUNT 24
#define BOOTSTRAP_SINGLETON_COUNT 3

typedef struct State {
    ORef method;
    uint8_t const* code;
    size_t pc;
    ORef regs[REG_COUNT];
    ORef const* consts;
    NamespaceRef ns;
    uint8_t entryRegc;

    Heap heap;

    union {
        struct {
            TypeRef fixnumType; // TAG_FIXNUM = 0b000 = 0
            ORef immTypesPadding1;
            TypeRef charType; // TAG_CHAR = 0b010 = 2
            ORef immTypesPadding2;
            TypeRef flonumType; // TAG_FLONUM = 0b100 = 4
            ORef immTypesPadding3;
            TypeRef boolType; // TAG_BOOL = 0b110 = 6
            ORef immTypesPadding4;

            TypeRef anyType;
            TypeRef typeType;
            TypeRef stringType;
            TypeRef arrayType;
            TypeRef byteArrayType;
            TypeRef symbolType;
            TypeRef pairType;
            TypeRef emptyListType;
            TypeRef unboundType;
            TypeRef methodType;
            TypeRef closureType;
            TypeRef continuationType;
            TypeRef varType;
            TypeRef nsType;
            TypeRef typeErrorType;
            TypeRef arityErrorType;
        };
        ORef types[BOOTSTRAP_TYPE_COUNT];
    };

    SymbolTable symbols;

    union {
        struct {
            EmptyListRef emptyList;
            UnboundRef unbound;
            ClosureRef exit;
        };
        ORef singletons[BOOTSTRAP_SINGLETON_COUNT];
    };

    VarRef errorHandler;

    Shadowstack shadowstack;
} State;

static void pushStackRoot(State* state, ORef* stackLoc);

inline static void popStackRoots(State* state, size_t count) { state->shadowstack.count -= count; }

// OPTIMIZE: If we already know that `isHeaped(v)`, the calls to `isa` -> `typeOf` recheck that
// redundantly:

static TypeRef typeOf(State const* state, ORef v);

static bool isa(State const* state, TypeRef type, ORef v);

inline static bool isString(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->stringType, v);
}

inline static bool isSymbol(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->symbolType, v);
}

inline static bool isPair(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->pairType, v);
}

inline static bool isEmptyList(State const* state, ORef v) {
    return eq(v, emptyListToORef(state->emptyList));
}

inline static bool isMethod(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->methodType, v);
}

inline static bool isClosure(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->closureType, v);
}

inline static bool isContinuation(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->continuationType, v);
}

inline static bool isType(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->typeType, v);
}

inline static bool isTypeError(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->typeErrorType, v);
}

static StringRef createString(State* state, Str str);

// `name` must not point into GC heap:
static SymbolRef intern(State* state, Str name);

inline static ORef* tryAllocArray(State* state, Fixnum count) {
    return (ORef*)tryAllocFlex(&state->heap.tospace, typeToPtr(state->arrayType), count);
}

inline static ORef* allocArrayOrDie(State* state, Fixnum count) {
    return (ORef*)allocFlexOrDie(&state->heap.tospace, typeToPtr(state->arrayType), count);
}

static ArrayRef createArray(State* state, Fixnum count);

inline static uint8_t* tryAllocByteArray(State* state, Fixnum count) {
    return (uint8_t*)tryAllocFlex(&state->heap.tospace, typeToPtr(state->byteArrayType), count);
}

inline static uint8_t* allocByteArrayOrDie(State* state, Fixnum count) {
    return (uint8_t*)allocFlexOrDie(&state->heap.tospace, typeToPtr(state->byteArrayType), count);
}

static PairRef allocPair(State* state);

static Method* tryAllocBytecodeMethod(
    State* state, ByteArrayRef code, ArrayRef consts, Fixnum arity, Bool hasVarArg);

static Method* allocBytecodeMethodOrDie(
    State* state, ByteArrayRef code, ArrayRef consts, Fixnum arity, Bool hasVarArg);

static ClosureRef allocClosure(State* state, MethodRef method, Fixnum cloverCount);

static ContinuationRef allocContinuation(
    State* state, MethodRef method, Fixnum pc, Fixnum cloverCount);

static TypeErrorRef createTypeError(State* state, TypeRef type, ORef val);

static ArityErrorRef createArityError(State* state, ClosureRef callee, Fixnum callArgc);

struct IRFn;

static void collect(State* state);

static void collectTracingIR(State* state, struct IRFn* fn);
