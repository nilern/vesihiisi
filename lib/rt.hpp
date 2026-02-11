#pragma once

#include <vector>

#include "value.hpp"
#include "heap.hpp"
#include "flyweights.hpp"

namespace {

#define REG_COUNT 256

#define BOOTSTRAP_TYPE_COUNT 33
#define BOOTSTRAP_SINGLETON_COUNT 6

struct NamedTypes {
    HRef<Type> paddington; // heapedTag = 0b00 = 0, DO NOT USE
    HRef<Type> fixnum; // fixnumTag = 0b01 = 1
    HRef<Type> charr; // charTag = 0b10 = 2
    HRef<Type> booll; // boolTag = 0b11 = 3
    HRef<Type> flonum; // flonumTag = 0b04 = 4

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
    HRef<Type> pointer;
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

class RootGuard {
    RT* state;

    RootGuard(RT* state, ORef* handle);

    friend RT;
public:
    RootGuard() : state{nullptr} {}

    ~RootGuard();

    RootGuard(RootGuard&& that);
    RootGuard& operator=(RootGuard&&);

    RootGuard(RootGuard const&) = delete;
    RootGuard& operator=(RootGuard const&) = delete;
};

struct RT {
    enum class DomainChecking : uint8_t { CHECK, SPECULATE, SKIP };

    ORef method;
    uint8_t const* code;
    size_t pc;
    ORef regs[REG_COUNT];
    SlotsMut<ORef> consts;
    HRef<Namespace> ns;
    uint8_t entryRegc;
    DomainChecking domainChecking;

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

    HRef<Var> debug;
    HRef<Var> errorHandler;

    std::vector<ORef*> shadowstack;

    static RT* tryCreate(size_t heapSize, char const* vshsHome, int argc, char const* argv[]);

    [[nodiscard]]
    RootGuard pushRoot(ORef* handle) { return RootGuard{this, handle}; } // RVO => not even move

    template<typename T>
    HRef<Type> reify() const { return T::reify(*this); }
    // HACK: `ORef` is a C type, so it cannot have `static HRef<Type> reify`:
    template<typename T> requires (std::is_same<T, ORef>{}())
    HRef<Type> reify() const { return this->types.any; }

    void setMethod(HRef<Method> v) {
        method = v;
        code = HRef<ByteArray>::fromUnchecked(v->code)->flexData();
        consts = HRef<ArrayMut>::fromUnchecked(v->consts)->itemsMut().data();
    }

    Object* alloc(HRef<Type> type);

private:
    RT(Heap heap, NamedTypes types, NamedSingletons singletons, HRef<Namespace> ns,
          HRef<Var> debug, HRef<Var> errorHandler);
};

HRef<Type> typeOf(RT const* state, ORef v);
Type const* typePtrOf(RT const* state, ORef v);

template<typename T>
bool isa(RT const& state, ORef v) { return T::contains(state, v); }
template<typename T> requires (std::is_same<T, ORef>{}())
bool isa(RT const& /*state*/, ORef /*v*/) { return true; }

bool isa(RT const* state, HRef<Type> type, ORef v);

inline bool isEmptyList(RT const* state, ORef v) {
    return eq(v, state->singletons.emptyList);
}

HRef<Type> createSlotsType(RT* state, HRef<Symbol> name, Fixnum slotCount, Bool isFlex);

String* allocString(RT* state, Fixnum byteCount);

HRef<String> createString(RT* state, Str str);

// `name` must not point into GC heap:
HRef<Symbol> intern(RT* state, Str name);

HRef<Symbol> internHeaped(RT* state, HRef<String> name);

inline Array* tryAllocArray(RT* state, Fixnum count) {
    return (Array*)state->heap.tryAllocFlex(&*state->types.array, count);
}

inline Array* allocArrayOrDie(RT* state, Fixnum count) {
    return (Array*)state->heap.allocFlexOrDie(&*state->types.array, count);
}

HRef<Array> createArray(RT* state, Fixnum count);

inline ArrayMut* tryAllocArrayMut(RT* state, Fixnum count) {
    return (ArrayMut*)state->heap.tryAllocFlex(&*state->types.arrayMut, count);
}

inline ArrayMut* allocArrayMutOrDie(RT* state, Fixnum count) {
    return (ArrayMut*)state->heap.allocFlexOrDie(&*state->types.arrayMut, count);
}

HRef<ArrayMut> createArrayMut(RT* state, Fixnum count);

inline ByteArray* tryAllocByteArray(RT* state, Fixnum count) {
    return (ByteArray*)state->heap.tryAllocFlex(&*state->types.byteArray, count);
}

inline ByteArray* allocByteArrayOrDie(RT* state, Fixnum count) {
    return (ByteArray*)state->heap.allocFlexOrDie(&*state->types.byteArray, count);
}

HRef<ByteArrayMut> createByteArrayMut(RT* state, Fixnum count);

HRef<Loc> createLoc(RT* state, HRef<String> filename, Fixnum byteIdx);

HRef<Pair> allocPair(RT* state);
HRef<Pair> createPair(RT* state, ORef car, ORef cdr, ORef maybeLoc);

Method* tryAllocBytecodeMethod(
    RT* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs);

Method* allocBytecodeMethodOrDie(
    RT* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs);

HRef<Method> allocBytecodeMethod(
    RT* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs);

HRef<Closure> allocClosure(RT* state, HRef<Method> method, Fixnum cloverCount);

HRef<Continuation> allocContinuation(
    RT* state, HRef<Method> method, Fixnum pc, Fixnum cloverCount);

HRef<Knot> allocKnot(RT* state);

HRef<InputFile> createInputFile(RT* state, UTF8InputFile&& file);

HRef<UnboundError> createUnboundError(RT* state, HRef<Symbol> name);

HRef<TypeError> createTypeError(RT* state, HRef<Type> type, ORef val);

HRef<ArityError> createArityError(RT* state, HRef<Closure> callee, Fixnum callArgc);

HRef<InapplicableError> createInapplicableError(RT* state, HRef<Multimethod> callee);

HRef<FatalError> createOverflowError(RT* state, HRef<Closure> callee, Fixnum x, Fixnum y);

HRef<FatalError> createDivByZeroError(RT* state, HRef<Closure> callee, Fixnum x, Fixnum y);

void collect(RT* state);

struct IRFn;
class MethodBuilder;

void collectTracingIR(RT* state, struct IRFn* fn, MethodBuilder* builder);

} // namespace
