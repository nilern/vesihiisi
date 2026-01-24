#pragma once

#include "object.hpp"
#include "heap.hpp"

namespace {

typedef struct SymbolTable {
    ORef* entries;
    size_t count;
    size_t cap;
} SymbolTable;

typedef struct Specializations {
    ORef* entries;
    size_t count;
    size_t cap;
} Specializations;

typedef struct Shadowstack {
    ORef** vals;
    size_t count;
    size_t cap;
} Shadowstack;

#define REG_COUNT 256

#define BOOTSTRAP_TYPE_COUNT 31
#define BOOTSTRAP_SINGLETON_COUNT 6

struct NamedTypes {
    HRef<Type> flonum;
    HRef<Type> fixnum; // fixnumTag = 0b01 = 1
    HRef<Type> charr; // charTag = 0b10 = 2
    HRef<Type> booll; // boolTag = 0b11 = 3

    HRef<Type> any;
    HRef<Type> type;
    HRef<Type> string;
    HRef<Type> stringIterator;
    HRef<Type> array;
    HRef<Type> arrayMut;
    HRef<Type> byteArray;
    HRef<Type> byteArrayMut;
    HRef<Type> symbol;
    HRef<Type> loc;
    HRef<Type> pair;
    HRef<Type> emptyList;
    HRef<Type> unbound;
    HRef<Type> method;
    HRef<Type> closure;
    HRef<Type> multimethod;
    HRef<Type> continuation;
    HRef<Type> var;
    HRef<Type> knot;
    HRef<Type> ns;
    HRef<Type> end;
    HRef<Type> inputFile;
    HRef<Type> fatalError;
    HRef<Type> unboundError;
    HRef<Type> typeError;
    HRef<Type> arityError;
    HRef<Type> inapplicableError;
};
static_assert(sizeof(NamedTypes) / sizeof(HRef<Type>) == BOOTSTRAP_TYPE_COUNT);

struct NamedSingletons {
    HRef<End> end;
    HRef<EmptyList> emptyList;
    HRef<Unbound> unbound;
    HRef<Continuation> exit;
    HRef<Symbol> quote;
    HRef<Symbol> ofType;
};
static_assert(sizeof(NamedSingletons) / sizeof(ORef) == BOOTSTRAP_SINGLETON_COUNT);

struct State {
    ORef method;
    uint8_t const* code;
    size_t pc;
    ORef regs[REG_COUNT];
    ORef const* consts;
    HRef<Namespace> ns;
    uint8_t entryRegc;
    bool checkDomain;

    Heap heap;

    union {
        NamedTypes types;
        HRef<Type> typesArray[BOOTSTRAP_TYPE_COUNT];
    };

    SymbolTable symbols;
    Specializations specializations;

    union {
        NamedSingletons singletons;
        ORef singletonsArray[BOOTSTRAP_SINGLETON_COUNT];
    };

    HRef<Var> errorHandler;

    Shadowstack shadowstack;

    static State* tryCreate(size_t heapSize, char const* vshsHome, int argc, char const* argv[]);

    ~State(); // Need a destructor (while we have `freeSymbolTable` etc.)

    State(State&&) = default;
    State& operator=(State&&) = default;

private:
    State(Heap heap, NamedTypes types, NamedSingletons singletons, HRef<Namespace> ns,
          HRef<Var> errorHandler);
};

void pushStackRoot(State* state, ORef* stackLoc);

inline void popStackRoots(State* state, size_t count) { state->shadowstack.count -= count; }

// OPTIMIZE: If we already know that `isHeaped(v)`, the calls to `isa` -> `typeOf` recheck that
// redundantly:

HRef<Type> typeOf(State const* state, ORef v);

bool isa(State const* state, HRef<Type> type, ORef v);

inline bool isString(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.string, v);
}

inline bool isSymbol(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.symbol, v);
}

inline bool isPair(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.pair, v);
}

inline bool isEmptyList(State const* state, ORef v) {
    return eq(v, state->singletons.emptyList.oref());
}

inline bool isMethod(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.method, v);
}

inline bool isClosure(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.closure, v);
}

inline bool isMultimethod(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.multimethod, v);
}

inline bool isContinuation(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.continuation, v);
}

inline bool isType(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.type, v);
}

inline bool isTypeError(State const* state, ORef v) {
    return isHeaped(v) && isa(state, state->types.typeError, v);
}

HRef<Type> createSlotsType(State* state, HRef<Symbol> name, Fixnum slotCount, Bool isFlex);

String* allocString(State* state, Fixnum byteCount);

HRef<String> createString(State* state, Str str);

// `name` must not point into GC heap:
HRef<Symbol> intern(State* state, Str name);

HRef<Symbol> internHeaped(State* state, HRef<String> name);

inline Array* tryAllocArray(State* state, Fixnum count) {
    return (Array*)state->heap.tospace.tryAllocFlex(state->types.array.ptr(), count);
}

inline Array* allocArrayOrDie(State* state, Fixnum count) {
    return (Array*)state->heap.tospace.allocFlexOrDie(state->types.array.ptr(), count);
}

HRef<Array> createArray(State* state, Fixnum count);

inline ArrayMut* tryAllocArrayMut(State* state, Fixnum count) {
    return (ArrayMut*)state->heap.tospace.tryAllocFlex(state->types.arrayMut.ptr(), count);
}

inline ArrayMut* allocArrayMutOrDie(State* state, Fixnum count) {
    return (ArrayMut*)state->heap.tospace.allocFlexOrDie(state->types.arrayMut.ptr(), count);
}

HRef<ArrayMut> createArrayMut(State* state, Fixnum count);

inline ByteArray* tryAllocByteArray(State* state, Fixnum count) {
    return (ByteArray*)state->heap.tospace.tryAllocFlex(state->types.byteArray.ptr(), count);
}

inline ByteArray* allocByteArrayOrDie(State* state, Fixnum count) {
    return (ByteArray*)state->heap.tospace.allocFlexOrDie(state->types.byteArray.ptr(), count);
}

HRef<ByteArrayMut> createByteArrayMut(State* state, Fixnum count);

HRef<Loc> createLoc(State* state, HRef<String> filename, Fixnum byteIdx);

HRef<Pair> allocPair(State* state);
HRef<Pair> createPair(State* state, ORef car, ORef cdr, ORef maybeLoc);

Method* tryAllocBytecodeMethod(
    State* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs);

Method* allocBytecodeMethodOrDie(
    State* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs);

HRef<Method> allocBytecodeMethod(
    State* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs);

HRef<Closure> allocClosure(State* state, HRef<Method> method, Fixnum cloverCount);

HRef<Continuation> allocContinuation(
    State* state, HRef<Method> method, Fixnum pc, Fixnum cloverCount);

HRef<Knot> allocKnot(State* state);

HRef<InputFile> createInputFile(State* state, UTF8InputFile&& file);

HRef<UnboundError> createUnboundError(State* state, HRef<Symbol> name);

HRef<TypeError> createTypeError(State* state, HRef<Type> type, ORef val);

HRef<ArityError> createArityError(State* state, HRef<Closure> callee, Fixnum callArgc);

HRef<InapplicableError> createInapplicableError(State* state, HRef<Multimethod> callee);

HRef<FatalError> createOverflowError(State* state, HRef<Closure> callee, Fixnum x, Fixnum y);

HRef<FatalError> createDivByZeroError(State* state, HRef<Closure> callee, Fixnum x, Fixnum y);

void collect(State* state);

struct IRFn;
struct MethodBuilder;

void collectTracingIR(State* state, struct IRFn* fn, struct MethodBuilder* builder);

} // namespace
