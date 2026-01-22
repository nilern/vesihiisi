#include "state.hpp"

#include <new>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "util/util.hpp"
#include "object.hpp"
#include "heap.hpp"
#include "flyweights.hpp"
#include "primops.hpp"

namespace {

char const* const typeNames[] = {
    "<flonum>",
    "<fixnum>",
    "<char>",
    "<bool>",
    "<any>",
    "<type>",
    "<string>",
    "<string-iterator>",
    "<array>",
    "<array!>",
    "<byte-array>",
    "<byte-array!>",
    "<symbol>",
    "<source-location>",
    "<pair>",
    "<empty-list>",
    "<unbound>",
    "<method>",
    "<fn>",
    "<multimethod>",
    "<continuation>",
    "<var>",
    "<knot>",
    "<ns>",
    "<end>",
    "<input-file>",
    "<fatal-error>",
    "<unbound-error>",
    "<type-error>",
    "<arity-error>",
    "<inapplicable-error>"
};
static_assert(sizeof(typeNames) / sizeof(*typeNames) == BOOTSTRAP_TYPE_COUNT);

inline void freeSymbols(SymbolTable* symbols) { free(symbols->entries); }

SymbolTable newSymbolTable(void) {
    size_t const cap = 2;
    ORef* const entries = (ORef*)calloc(cap, sizeof *entries);
    return SymbolTable{.entries = entries, .count = 0, .cap = cap};
}

void pruneSymbols(SymbolTable* symbols);

bool tryCreateNamespace(
    Semispace* semispace, HRef<Namespace>* dest, Type const* nsType, Type const* arrayType
) {
    Fixnum const count = Fixnum{2l};
    ArrayMut* const keys = (ArrayMut*)semispace->tryAllocFlex(arrayType, count);
    if (!keys) { return false; }
    ArrayMut* const vals = (ArrayMut*)semispace->tryAllocFlex(arrayType, count);
    if (!vals) { return false; }
    Namespace* const ptr = (Namespace*)semispace->tryAlloc(nsType);
    if (!ptr) { return false; }

    *ptr = Namespace{
        .keys = HRef(keys),
        .vals = HRef(vals),
        .count = count
    };

    *dest = HRef(ptr);
    return true;
}

inline void freeShadowstack(Shadowstack* shadowstack) {
    free(shadowstack->vals);
}

Shadowstack newShadowstack(void) {
    size_t const cap = 2;
    ORef** const vals = (ORef**)malloc(cap * sizeof *vals);
    return Shadowstack{.vals = vals, .count = 0, .cap = cap};
}

State::~State() {
    freeSymbols(&symbols);
    freeSpecializations(&specializations);
    freeShadowstack(&shadowstack);
}

void freeState(State* state) { delete(state); }

void pushStackRoot(State* state, ORef* stackLoc) {
    if (state->shadowstack.count == state->shadowstack.cap) {
        size_t const newCap = state->shadowstack.cap + state->shadowstack.cap / 2;
        state->shadowstack.vals =
            (ORef**)realloc(state->shadowstack.vals, newCap * sizeof * state->shadowstack.vals);
        state->shadowstack.cap = newCap;
    }

    state->shadowstack.vals[state->shadowstack.count++] = stackLoc;
}

void markRoots(State* state) {
    state->method = state->heap.mark(state->method);

    // OPTIMIZE: Only mark registers that are actually live (requires emitting liveness bitmaps for
    // safepoints:
    for (size_t i = 0; i < REG_COUNT; ++i) {
        state->regs[i] = state->heap.mark(state->regs[i]);
    }

    state->ns = HRef<Namespace>::fromUnchecked(state->heap.mark(state->ns.oref()));

    for (size_t i = 0; i < BOOTSTRAP_TYPE_COUNT; ++i) {
        state->typesArray[i] =
            HRef<Type>::fromUnchecked(state->heap.mark(state->typesArray[i].oref()));
    }

    for (size_t i = 0; i < BOOTSTRAP_SINGLETON_COUNT; ++i) {
        state->singletonsArray[i] = state->heap.mark(state->singletonsArray[i]);
    }

    state->errorHandler = HRef<Var>::fromUnchecked(state->heap.mark(state->errorHandler.oref()));

    {
        size_t const stackRootCount = state->shadowstack.count;
        for (size_t i = 0; i < stackRootCount; ++i) {
            *state->shadowstack.vals[i] = state->heap.mark(*state->shadowstack.vals[i]);
        }
    }
}

void updateWeakRefs(State* state) {
    pruneSymbols(&state->symbols);
    pruneSpecializations(&state->specializations);
}

void initSpecialPurposeRegs(State* state) {
    ORef const anyMethod = state->method;
    if (isHeaped(anyMethod)) {
        Method* const methodPtr = HRef<Method>::fromUnchecked(anyMethod).ptr();
        state->code = HRef<ByteArray>::fromUnchecked(methodPtr->code).ptr()->flexData();
        state->consts = HRef<Array>::fromUnchecked(methodPtr->consts).ptr()->flexData();
    }
}

// TODO: Extend this and expand its use:
template<typename T, bool isBytes>
Type* tryCreateFixedType(Semispace* semispace, Type const* typeType) {
    Type* const type = reinterpret_cast<Type*>(semispace->tryAlloc(typeType));
    if (!type) { return nullptr; }

    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(T)),
        .align = Fixnum((int64_t)alignof(T)),
        .isBytes = Bool{isBytes},
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateTypeType(Semispace* semispace) {
    Type const bootstrapTypeType = {
        .minSize = Fixnum((intptr_t)sizeof(Type)),
        .align = Fixnum((intptr_t)alignof(Type)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum{0l},
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
    
    void* const maybeTypeType = semispace->tryAlloc(&bootstrapTypeType);
    if (!maybeTypeType) { return nullptr; }
    
    Type* const typeType = (Type*)maybeTypeType;
    *((Header*)maybeTypeType - 1) = Header{typeType}; // Init header, closing loop
    *typeType = bootstrapTypeType; // Init data
    
    return typeType;
}

Type* tryCreateAnyType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{ // TODO: Avoid requiring some nonsensical values like this:
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateStringType(Semispace* semispace, Type const* typeType) {
    void* const maybeStringType = semispace->tryAlloc(typeType);
    if (!maybeStringType) { return nullptr; }
    
    Type* const stringType = (Type*)maybeStringType;
    *stringType = Type{
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
    
    return stringType;
}

Type* tryCreateStringIteratorType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((intptr_t)sizeof(StringIterator)),
        .align = Fixnum((intptr_t)alignof(StringIterator)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateSymbolType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((intptr_t)sizeof(Symbol)),
        .align = Fixnum((intptr_t)alignof(Symbol)),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
    
    return type;
}

Type* tryCreateArrayType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)alignof(ORef)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
    
    return type;
}

Type* tryCreateArrayMutType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)alignof(ORef)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateByteArrayType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateByteArrayMutType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreatePairType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(Pair)),
        .align = Fixnum((int64_t)alignof(Pair)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
    
    return type;
}

Type* tryCreateEmptyListType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
    
    return type;
}

Type* tryCreateUnboundType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateMethodType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(Method)),
        .align = Fixnum((int64_t)alignof(Method)),
        .isBytes = False,
        .hasCodePtr = True,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateClosureType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(Closure)),
        .align = Fixnum((int64_t)alignof(Closure)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateMultimethodType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(Multimethod)),
        .align = Fixnum((int64_t)alignof(Multimethod)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateContinuationType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(Continuation)),
        .align = Fixnum((int64_t)alignof(Continuation)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateVarType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(Var)),
        .align = Fixnum((int64_t)alignof(Var)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateKnotType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(Knot)),
        .align = Fixnum((int64_t)alignof(Knot)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateNamespaceType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(Namespace)),
        .align = Fixnum((int64_t)alignof(Namespace)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateInputFileType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(InputFile)),
        .align = Fixnum((int64_t)alignof(InputFile)),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateEndType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateFatalErrorType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(FatalError)),
        .align = Fixnum((int64_t)alignof(FatalError)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = True,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateUnboundErrorType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(UnboundError)),
        .align = Fixnum((int64_t)alignof(UnboundError)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateTypeErrorType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(TypeError)),
        .align = Fixnum((int64_t)alignof(TypeError)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateArityErrorType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(ArityError)),
        .align = Fixnum((int64_t)alignof(ArityError)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateInapplicableErrorType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = Type{
        .minSize = Fixnum((int64_t)sizeof(InapplicableError)),
        .align = Fixnum((int64_t)alignof(InapplicableError)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };

    return type;
}

Type* tryCreateImmType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = semispace->tryAlloc(typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = Type{ // TODO: Avoid requiring some nonsensical values like this:
        .minSize = Fixnum{0l},
        .align = Fixnum((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False,
        .hash = Fixnum::fromUnchecked(ORef{0}), // HACK
        .name = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
    
    return type;
}

inline Type* tryCreateFixnumType(Semispace* semispace, Type const* typeType) {
    return tryCreateImmType(semispace, typeType);
}

inline Type* tryCreateFlonumType(Semispace* semispace, Type const* typeType) {
    return tryCreateImmType(semispace, typeType);
}

inline Type* tryCreateCharType(Semispace* semispace, Type const* typeType) {
    return tryCreateImmType(semispace, typeType);
}

inline Type* tryCreateBoolType(Semispace* semispace, Type const* typeType) {
    return tryCreateImmType(semispace, typeType);
}

HRef<Method> vcreatePrimopMethod(
    State* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum arity, va_list domain);
HRef<Method> createPrimopMethod(
    State* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum arity, ...);

HRef<Closure> allocClosure(State* state, HRef<Method> method, Fixnum cloverCount);

HRef<Symbol> intern(State* state, Str name);

HRef<Var> getVar(State* state, HRef<Namespace> nsRef, HRef<Symbol> name);

void installPrimordial(State* state, Str name, ORef v) {
    pushStackRoot(state, &v);

    HRef<Symbol> const symbol = intern(state, name);
    HRef<Var> const var = getVar(state, state->ns, symbol);

    var.ptr()->val =v;

    popStackRoots(state, 1);
}

void installPrimop(
    State* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum arity, ...
) {
    va_list domain;
    va_start(domain, arity);
    HRef<Method> const method =
        vcreatePrimopMethod(state, name, nativeCode, hasVarArg, arity, domain);
    va_end(domain);
    HRef<Closure> const closure = allocClosure(state, method, Fixnum{0l});
    installPrimordial(state, name, closure.oref());
}

Var* tryCreateUnboundVar(
    Semispace* semispace, Type const* unboundType, HRef<Unbound> unbound);

void nameType(State* state, HRef<Type> typeRef, Str name) {
    pushStackRoot(state, (ORef*)&typeRef);
    HRef<Symbol> const nameSym = intern(state, name);
    popStackRoots(state, 1);

    Type* const type = typeRef.ptr();
    type->hash = nameSym.ptr()->hash;
    type->name = nameSym;
}

HRef<Array> createCommandLine(State* state, int argc, char const* argv[]) {
    HRef<Array> commandLine = createArray(state, Fixnum{int64_t(argc)});
    pushStackRoot(state, &commandLine);

    for (size_t i = 0; i < size_t(argc); ++i) {
        char const* const segCStr = argv[i];
        HRef<String> const seg =
            createString(state, Str{reinterpret_cast<uint8_t const*>(segCStr), strlen(segCStr)});
        const_cast<ORef*>(commandLine.ptr()->flexData())[i] = seg; // Initializing store
    }

    popStackRoots(state, 1);
    return commandLine;
}

State::State(
    Heap heap, NamedTypes types, NamedSingletons singletons, HRef<Namespace> ns,
    HRef<Var> errorHandler
) :
    method{Default},
    code{nullptr},
    pc{0},
    regs{},
    consts{nullptr},
    ns{ns},
    entryRegc{0}, // Intentionally invalid
    checkDomain{true},

    heap{std::move(heap)},
    types{types},

    symbols{newSymbolTable()},
    specializations{newSpecializations()},

    singletons{singletons},
    errorHandler{errorHandler},

    shadowstack{newShadowstack()}
{}

State* State::tryCreate(size_t heapSize, int argc, char const* argv[]) {
    Heap heap = Heap::tryCreate(heapSize);
    if (!heap.isValid()) { return nullptr; }
    
    Type* const typeType = tryCreateTypeType(&heap.tospace);
    if (!typeType) { return nullptr; }
    Type * const anyType = tryCreateAnyType(&heap.tospace, typeType);
    if (!anyType) { return nullptr; }
    Type* const stringType = tryCreateStringType(&heap.tospace, typeType);
    if (!stringType) { return nullptr; }
    Type* const stringIteratorType = tryCreateStringIteratorType(&heap.tospace, typeType);
    if (!stringIteratorType) { return nullptr; }
    Type* const arrayType = tryCreateArrayType(&heap.tospace, typeType);
    if (!arrayType) { return nullptr; }
    Type* const arrayMutType = tryCreateArrayMutType(&heap.tospace, typeType);
    if (!arrayMutType) { return nullptr; }
    Type* const byteArrayType = tryCreateByteArrayType(&heap.tospace, typeType);
    if (!byteArrayType) { return nullptr; }
    Type* const byteArrayMutType = tryCreateByteArrayMutType(&heap.tospace, typeType);
    if (!byteArrayMutType) { return nullptr; }
    Type* const symbolType = tryCreateSymbolType(&heap.tospace, typeType);
    if (!symbolType) { return nullptr; }
    Type* const locType = tryCreateFixedType<Loc, false>(&heap.tospace, typeType);
    if (!locType) { return nullptr; }
    Type* const pairType = tryCreatePairType(&heap.tospace, typeType);
    if (!pairType) { return nullptr; }
    Type* const emptyListType = tryCreateEmptyListType(&heap.tospace, typeType);
    if (!emptyListType) { return nullptr; }
    Type* const methodType = tryCreateMethodType(&heap.tospace, typeType);
    if (!methodType) { return nullptr; }
    Type* const closureType = tryCreateClosureType(&heap.tospace, typeType);
    if (!closureType) { return nullptr; }
    Type* const multimethodType = tryCreateMultimethodType(&heap.tospace, typeType);
    if (!multimethodType) { return nullptr; }
    Type* const continuationType = tryCreateContinuationType(&heap.tospace, typeType);
    if (!continuationType) { return nullptr; }
    Type* const unboundType = tryCreateUnboundType(&heap.tospace, typeType);
    if (!unboundType) { return nullptr; }
    Type* const varType = tryCreateVarType(&heap.tospace, typeType);
    if (!varType) { return nullptr; }
    Type* const knotType = tryCreateKnotType(&heap.tospace, typeType);
    if (!knotType) { return nullptr; }
    Type* const nsType = tryCreateNamespaceType(&heap.tospace, typeType);
    if (!nsType) { return nullptr; }
    Type* const inputFileType = tryCreateInputFileType(&heap.tospace, typeType);
    if (!inputFileType) { return nullptr; }
    Type* const endType = tryCreateEndType(&heap.tospace, typeType);
    if (!endType) { return nullptr; }
    Type* const fatalErrorType = tryCreateFatalErrorType(&heap.tospace, typeType);
    if (!fatalErrorType) { return nullptr; }
    Type* const unboundErrorType = tryCreateUnboundErrorType(&heap.tospace, typeType);
    if (!unboundType) { return nullptr; }
    Type* const typeErrorType = tryCreateTypeErrorType(&heap.tospace, typeType);
    if (!typeErrorType) { return nullptr; }
    Type* const arityErrorType = tryCreateArityErrorType(&heap.tospace, typeType);
    if (!arityErrorType) { return nullptr; }
    Type* const inapplicableErrorType =
        tryCreateInapplicableErrorType(&heap.tospace, typeType);
    if (!inapplicableErrorType) { return nullptr; }
    
    Type* const fixnumType = tryCreateFixnumType(&heap.tospace, typeType);
    if (!fixnumType) { return nullptr; }
    Type* const charType = tryCreateCharType(&heap.tospace, typeType);
    if (!charType) { return nullptr; }
    Type* const flonumType = tryCreateFlonumType(&heap.tospace, typeType);
    if (!flonumType) { return nullptr; }
    Type* const boolType = tryCreateBoolType(&heap.tospace, typeType);
    if (!boolType) { return nullptr; }

    End* const end = (End*)heap.tospace.tryAlloc(endType);
    if (!end) { return nullptr; }
    EmptyList* const emptyList = (EmptyList*)heap.tospace.tryAlloc(emptyListType);
    if (!emptyList) { return nullptr; }
    Unbound* const unbound = (Unbound*)heap.tospace.tryAlloc(unboundType);
    if (!unbound) { return nullptr; }
    Continuation* const exit =
        (Continuation*)heap.tospace.tryAllocFlex(continuationType, Fixnum{0l});
    if (!exit) { return nullptr; }
    exit->pc = Fixnum{0l};

    Var* const errorHandler = tryCreateUnboundVar(&heap.tospace, varType, HRef(unbound));
    if (!errorHandler) { return nullptr; }

    HRef<Namespace> ns = HRef<Namespace>::fromUnchecked(ORef{0}); // HACK;
    if (!tryCreateNamespace(&heap.tospace, &ns, nsType, arrayType)) { return nullptr; }

    State* const dest = new (std::nothrow) State{
        std::move(heap),
        {
            .flonum = HRef(flonumType),
            .fixnum = HRef(fixnumType),
            .charr = HRef(charType),
            .booll = HRef(boolType),

            .any = HRef(anyType),
            .type = HRef(typeType),
            .string = HRef(stringType),
            .stringIterator = HRef{stringIteratorType},
            .array = HRef(arrayType),
            .arrayMut = HRef(arrayMutType),
            .byteArray = HRef(byteArrayType),
            .byteArrayMut = HRef(byteArrayMutType),
            .symbol = HRef(symbolType),
            .loc = HRef{locType},
            .pair = HRef(pairType),
            .emptyList = HRef(emptyListType),
            .unbound = HRef(unboundType),
            .method = HRef(methodType),
            .closure = HRef(closureType),
            .multimethod = HRef(multimethodType),
            .continuation = HRef(continuationType),
            .var = HRef(varType),
            .knot = HRef(knotType),
            .ns = HRef(nsType),
            .end = HRef{endType},
            .inputFile = HRef{inputFileType},
            .fatalError = HRef(fatalErrorType),
            .unboundError = HRef(unboundErrorType),
            .typeError = HRef(typeErrorType),
            .arityError = HRef(arityErrorType),
            .inapplicableError = HRef(inapplicableErrorType)
        },
        {
            .end = HRef{end},
            .emptyList = HRef(emptyList),
            .unbound = HRef(unbound),
            .exit = HRef(exit),
            .quote = HRef<Symbol>::fromUnchecked(ORef{0}), // HACK
            .ofType = HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
        },
        ns,
        HRef{errorHandler}
    };
    if (!dest) { return nullptr; }

    dest->singletons.quote = intern(dest, strLit("quote"));
    dest->singletons.ofType = intern(dest, strLit(":"));

    for (size_t i = 0; i < BOOTSTRAP_TYPE_COUNT; ++i) {
        char const* const name = typeNames[i];
        size_t const nameLen = strlen(name);
        if (nameLen > 0) {
            Str const nameStr = Str{reinterpret_cast<uint8_t const*>(name), nameLen}; // HACK
            // `ORef const type = dest->types[i];` would not pay off since `nameType` may GC:
            nameType(dest, dest->typesArray[i], nameStr);
            installPrimordial(dest, nameStr, dest->typesArray[i].oref());
        }
    }

    HRef<Method> const abortMethod =
        createPrimopMethod(dest, strLit("abort"), (MethodCode)primopAbort,
                           false, Fixnum{1l}, dest->types.any);
    HRef<Closure> abortClosure = allocClosure(dest, abortMethod, Fixnum{0l});
    pushStackRoot(dest, (ORef*)&abortClosure);
    dest->errorHandler.ptr()->val = abortClosure.oref();

    installPrimordial(dest, strLit("abort"), abortClosure.oref());
    popStackRoots(dest, 1); // abortClosure
    installPrimordial(dest, strLit("end"), dest->singletons.end);
    installPrimordial(dest, strLit("standard-input"), createInputFile(dest, UTF8InputFile{stdin}));
    installPrimordial(dest, strLit("*command-line*"), createCommandLine(dest, argc, argv));

    installPrimop(dest, strLit("apply-array"), (MethodCode)primopApplyArray,
                  false, Fixnum{2l}, dest->types.any, dest->types.array);
    // TODO: `array!` -> `array-mut` (everywhere):
    installPrimop(dest, strLit("apply-array!"), (MethodCode)primopApplyArray,
                  false, Fixnum{2l}, dest->types.any, dest->types.arrayMut);
    installPrimop(dest, strLit("apply-list"), (MethodCode)primopApplyList,
                  false, Fixnum{2l}, dest->types.any, dest->types.any);
    installPrimop(dest, strLit("call-with-current-continuation"), (MethodCode)primopCallCC,
                  false, Fixnum{1l}, dest->types.closure);
    installPrimop(dest, strLit("continue"), (MethodCode)primopContinue,
                  false, Fixnum{2l}, dest->types.continuation, dest->types.any);
    installPrimop(dest, strLit("identical?"), (MethodCode)primopIdentical,
                  false, Fixnum{2l}, dest->types.any, dest->types.any);
    installPrimop(dest, strLit("type-of"), (MethodCode)primopTypeOf,
                  false, Fixnum{1l}, dest->types.any);
    installPrimop(dest, strLit("make-slots-type"), (MethodCode)primopMakeSlotsType,
                  true, Fixnum{3l}, dest->types.symbol, dest->types.fixnum, dest->types.booll);
    installPrimop(dest, strLit("make"), (MethodCode)primopMake,
                  true, Fixnum{2l}, dest->types.type, dest->types.any);
    installPrimop(dest, strLit("slot-get"), (MethodCode)primopSlotGet,
                  false, Fixnum{2l}, dest->types.any, dest->types.fixnum);
    installPrimop(dest, strLit("slot-set!"), (MethodCode)primopSlotSet,
                  false, Fixnum{3l}, dest->types.any, dest->types.fixnum, dest->types.any);
    installPrimop(dest, strLit("make-flex"), (MethodCode)primopMakeFlex,
                  true, Fixnum{2l}, dest->types.type, dest->types.fixnum);
    installPrimop(dest, strLit("flex-count"), (MethodCode)primopFlexCount,
                  false, Fixnum{1l}, dest->types.any);
    installPrimop(dest, strLit("flex-get"), (MethodCode)primopFlexGet,
                  false, Fixnum{2l}, dest->types.any, dest->types.fixnum);
    installPrimop(dest, strLit("flex-set!"), (MethodCode)primopFlexSet,
                  false, Fixnum{3l}, dest->types.any, dest->types.fixnum, dest->types.any);
    installPrimop(dest, strLit("flex-copy!"), (MethodCode)primopFlexCopy,
                  false, Fixnum{5l}, dest->types.any, dest->types.fixnum,
                  dest->types.any, dest->types.fixnum, dest->types.fixnum);
    installPrimop(dest, strLit("flex-copy"), (MethodCode)primopFlexClone,
                  false, Fixnum{3l}, dest->types.any, dest->types.fixnum, dest->types.fixnum);
    installPrimop(dest, strLit("fx+"), (MethodCode)primopFxAdd,
                  false, Fixnum{2l}, dest->types.fixnum, dest->types.fixnum);
    installPrimop(dest, strLit("fx-"), (MethodCode)primopFxSub,
                  false, Fixnum{2l}, dest->types.fixnum, dest->types.fixnum);
    installPrimop(dest, strLit("fx*"), (MethodCode)primopFxMul,
                  false, Fixnum{2l}, dest->types.fixnum, dest->types.fixnum);
    installPrimop(dest, strLit("fx-quot"), (MethodCode)primopFxQuot,
                  false, Fixnum{2l}, dest->types.fixnum, dest->types.fixnum);
    installPrimop(dest, strLit("fx<"), (MethodCode)primopFxLt,
                  false, Fixnum{2l}, dest->types.fixnum, dest->types.fixnum);
    installPrimop(dest, strLit("fixnum->flonum"), (MethodCode)primopFixnumToFlonum,
                  false, Fixnum{1l}, dest->types.fixnum);
    installPrimop(dest, strLit("fl+"), (MethodCode)primopFlAdd,
                  false, Fixnum{2l}, dest->types.flonum, dest->types.flonum);
    installPrimop(dest, strLit("fl-"), (MethodCode)primopFlSub,
                  false, Fixnum{2l}, dest->types.flonum, dest->types.flonum);
    installPrimop(dest, strLit("fl*"), (MethodCode)primopFlMul,
                  false, Fixnum{2l}, dest->types.flonum, dest->types.flonum);
    installPrimop(dest, strLit("fl/"), (MethodCode)primopFlDiv,
                  false, Fixnum{2l}, dest->types.flonum, dest->types.flonum);
    installPrimop(dest, strLit("char->integer"), (MethodCode)primopCharToInteger,
                  false, Fixnum{1l}, dest->types.charr);
    installPrimop(dest, strLit("char<"), (MethodCode)primopCharLt,
                  false, Fixnum{2l}, dest->types.charr, dest->types.charr);
    installPrimop(dest, strLit("char-alphabetic?"), (MethodCode)primopCharIsAlphabetic,
                  false, Fixnum{1l}, dest->types.charr);
    installPrimop(dest, strLit("char-numeric?"), (MethodCode)primopCharIsNumeric,
                  false, Fixnum{1l}, dest->types.charr);
    installPrimop(dest, strLit("char-whitespace?"), (MethodCode)primopCharIsWhitespace,
                  false, Fixnum{1l}, dest->types.charr);
    installPrimop(dest, strLit("array!->string"), (MethodCode)primopArrayMutToString,
                  false, Fixnum{1l}, dest->types.arrayMut);
    installPrimop(dest, strLit("string-iterator-peek"), (MethodCode)primopStringIteratorPeek,
                  false, Fixnum{1l}, dest->types.stringIterator);
    installPrimop(dest, strLit("string-iterator-next!"), (MethodCode)primopStringIteratorNext,
                  false, Fixnum{1l}, dest->types.stringIterator);
    installPrimop(dest, strLit("string->symbol"), (MethodCode)primopStringToSymbol,
                  false, Fixnum{1l}, dest->types.string);
    installPrimop(dest, strLit("open-input-file"), (MethodCode)primopOpenInputFile,
                  false, Fixnum{1l}, dest->types.string);
    installPrimop(dest, strLit("close-port"), (MethodCode)primopClosePort,
                  false, Fixnum{1l}, dest->types.inputFile);
    installPrimop(dest, strLit("peek-char"), (MethodCode)primopPeekChar,
                  false, Fixnum{1l}, dest->types.inputFile);
    installPrimop(dest, strLit("read-char"), (MethodCode)primopReadChar,
                  false, Fixnum{1l}, dest->types.inputFile);
    installPrimop(dest, strLit("write"), (MethodCode)primopWrite,
                  false, Fixnum{1l}, dest->types.any);
    installPrimop(dest, strLit("write-char"), (MethodCode)primopWriteChar,
                  false, Fixnum{1l}, dest->types.charr);
    installPrimop(dest, strLit("write-string"), (MethodCode)primopWriteString,
                  false, Fixnum{1l}, dest->types.string);
    installPrimop(dest, strLit("flush-output-port"), (MethodCode)primopFlushOutputPort,
                  false, Fixnum{0l});
    installPrimop(dest, strLit("current-second"), (MethodCode)primopCurrentSecond,
                  false, Fixnum{0l});
    installPrimop(dest, strLit("current-jiffy"), (MethodCode)primopCurrentJiffy, false, Fixnum{0l});
    installPrimop(dest, strLit("jiffies-per-second"), (MethodCode)primopJiffiesPerSecond,
                  false, Fixnum{0l});
    installPrimop(dest, strLit("resolve"), (MethodCode)primopResolve,
                  false, Fixnum{1l}, dest->types.symbol);
    installPrimop(dest, strLit("eval"), (MethodCode)primopEval,
                  false, Fixnum{3l}, dest->types.any, dest->types.loc, dest->types.booll);
    installPrimop(dest, strLit("exit"), (MethodCode)primopExit,
                  false, Fixnum{1l}, dest->types.any);

    return dest;
}

HRef<Type> typeOf(State const* state, ORef v) {
    TaggedType const tag = getTag(v);
    return tag == TaggedType::HEAPED
        ? HRef<Object>::fromUnchecked(v).ptr()->header()->type()
        : state->typesArray[(size_t)tag];
}

bool isa(State const* state, HRef<Type> type, ORef v) {
    if (eq(type.oref(), state->types.any.oref())) { return true; }

    return eq(typeOf(state, v).oref(), type.oref());
}

[[maybe_unused]]
void assertStateInTospace(State const* state) {
    if (isHeaped(state->method)) {
        assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(state->method)));
        assert(allocatedInSemispace(&state->heap.tospace, state->code));
        assert(allocatedInSemispace(&state->heap.tospace, state->consts));
    }

    // TODO: When we start only marking live regs, this has to only check those as well to avoid
    // false positives:
    for (size_t i = 0; i < REG_COUNT; ++i) {
        ORef const reg = state->regs[i];
        if (isHeaped(reg)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(reg)));
        }
    }

    assert(allocatedInSemispace(&state->heap.tospace, state->ns.ptr()));

    for (size_t i = 0; i < BOOTSTRAP_TYPE_COUNT; ++i) {
        assert(allocatedInSemispace(&state->heap.tospace, state->typesArray[i].ptr()));
    }

    for (size_t i = 0; i < state->symbols.cap; ++i) {
        ORef const v = state->symbols.entries[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    for (size_t i = 0; i < state->specializations.cap; ++i) {
        ORef const v = state->specializations.entries[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    for (size_t i = 0; i < BOOTSTRAP_SINGLETON_COUNT; ++i) {
        ORef const v = state->singletonsArray[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    assert(allocatedInSemispace(&state->heap.tospace, state->errorHandler.ptr()));

    {
        size_t const stackRootCount = state->shadowstack.count;
        for (size_t i = 0; i < stackRootCount; ++i) {
            ORef const v = *state->shadowstack.vals[i];
            if (isHeaped(v)) {
                assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
            }
        }
    }
}

void defaultPrepCollection(State* state) {
    flipSemispaces(&state->heap);
    markRoots(state);
}

void completeCollection(State* state) {
    state->heap.collect();

    updateWeakRefs(state);

    state->heap.fromspace.refurbish(&state->heap.tospace);
    initSpecialPurposeRegs(state);

#ifndef NDEBUG
    assertStateInTospace(state);
#endif
}

void collect(State* state) {
    defaultPrepCollection(state);

    completeCollection(state);
}

void markIRFn(State* state, struct IRFn* fn);
void assertIRFnInTospace(State const* state, struct IRFn const* fn);
void markMethodBuilder(State* state, struct MethodBuilder* builder);
void assertMethodBuilderInTospace(State const* state, struct MethodBuilder const* builder);

void collectTracingIR(State* state, struct IRFn* fn, struct MethodBuilder* builder) {
    defaultPrepCollection(state);
    markIRFn(state, fn);
    markMethodBuilder(state, builder);

    completeCollection(state);

#ifndef NDEBUG
    assertIRFnInTospace(state, fn);
    assertMethodBuilderInTospace(state, builder);
#endif
}

HRef<Type> createSlotsType(State* state, HRef<Symbol> name, Fixnum slotCount, Bool isFlex) {
    Type* ptr = static_cast<decltype(ptr)>(state->heap.tospace.tryAlloc(state->types.type.ptr()));
    if (mustCollect(ptr)) {
        pushStackRoot(state, &name);
        collect(state);
        popStackRoots(state, 1);
        ptr = static_cast<decltype(ptr)>(state->heap.tospace.allocOrDie(state->types.type.ptr()));
    }

    Fixnum const minSize = !isFlex.val()
        ? Fixnum{int64_t(size_t(slotCount.val()) * sizeof(ORef))}
        : Fixnum{int64_t(size_t(slotCount.val() - 1) * sizeof(ORef))};

    *ptr = Type{
        .minSize = minSize,
        .align = Fixnum((int64_t)objectMinAlign),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = isFlex,
        .hash = name.ptr()->hash,
        .name = name
    };

    return HRef{ptr};
}

String* allocString(State* state, Fixnum byteCount) {
    String* ptr =
        static_cast<String*>(state->heap.tospace.tryAllocFlex(state->types.string.ptr(),
                             byteCount));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = static_cast<String*>(state->heap.tospace.allocFlexOrDie(state->types.string.ptr(),
                                   byteCount));
    }

    return ptr;
}

HRef<String> createString(State* state, Str str) {
    String* const string = allocString(state, Fixnum((intptr_t)str.len));
    
    memcpy(const_cast<uint8_t*>(string->flexData()), str.data, str.len);
    
    return HRef{string};
}

HRef<Array> createArray(State* state, Fixnum count) {
    Array* ptr = tryAllocArray(state, count);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocArrayOrDie(state, count);
    }

    return HRef((Array*)ptr);
}

HRef<ArrayMut> createArrayMut(State* state, Fixnum count) {
    ArrayMut* ptr = tryAllocArrayMut(state, count);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocArrayMutOrDie(state, count);
    }

    return HRef((ArrayMut*)ptr);
}

HRef<ByteArrayMut> createByteArrayMut(State* state, Fixnum count) {
    ByteArrayMut* ptr = static_cast<ByteArrayMut*>(
        state->heap.tospace.tryAllocFlex(state->types.byteArrayMut.ptr(), count));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = static_cast<ByteArrayMut*>(
            state->heap.tospace.allocFlexOrDie(state->types.byteArrayMut.ptr(), count));
    }

    return HRef{ptr};
}

HRef<Symbol> createUninternedSymbol(State* state, Fixnum hash, Str name) {
    Symbol* ptr = (Symbol*)state->heap.tospace.tryAllocFlex(
        state->types.symbol.ptr(), Fixnum((intptr_t)name.len));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Symbol*)state->heap.tospace.allocFlexOrDie(
            state->types.symbol.ptr(), Fixnum((intptr_t)name.len));
    }

    ptr->hash = hash;
    memcpy((char*)ptr->flexData(), name.data, name.len);
    
    return HRef(ptr);
}

HRef<Symbol> createUninternedSymbolFromHeaped(State* state, Fixnum hash, HRef<String> name) {
    Symbol* ptr = (Symbol*)state->heap.tospace.tryAllocFlex(
        state->types.symbol.ptr(), name.ptr()->flexCount());
    if (mustCollect(ptr)) {
        pushStackRoot(state, &name);
        collect(state);
        popStackRoots(state, 1);
        ptr = (Symbol*)state->heap.tospace.allocFlexOrDie(
            state->types.symbol.ptr(), name.ptr()->flexCount());
    }

    ptr->hash = hash;
    memcpy((char*)ptr->flexData(), name.ptr()->flexData(), size_t(name.ptr()->flexCount().val()));

    return HRef(ptr);
}

inline Fixnum hashStr(Str s) { return Fixnum((intptr_t)fnv1aHash(s)); }

typedef struct IndexOfSymbolRes {
    size_t index;
    bool exists;
} IndexOfSymbolRes;

IndexOfSymbolRes indexOfSymbol(SymbolTable const* symbols, Fixnum hash, Str name) {
    uintptr_t const h = (uintptr_t)hash.val();
    
    size_t const maxIndex = symbols->cap - 1;
    for (size_t collisions = 0, i = h & maxIndex;; ++collisions, i = (i + collisions) & maxIndex) {
        ORef* const entry = symbols->entries + i;
        
        if (eq(*entry, Default)) { return IndexOfSymbolRes{i, false}; }

        if (isHeaped(*entry)) {
            HRef<Symbol> const symbol = HRef<Symbol>::fromUnchecked(*entry);
            Symbol const* const symbolPtr = symbol.ptr();
            if (eq(symbolPtr->hash.oref(), hash.oref())
                && strEq(symbol.ptr()->name(), name)
            ) {
                return IndexOfSymbolRes{i, true};
            }
        }
    }
}

// OPTMIZE: Do not grow if load factor is largely due to tombstones:
void rehashSymbols(State* state) {
    size_t const oldCap = state->symbols.cap;
    size_t const newCap = oldCap * 2;
    ORef* const newEntries = (ORef*)calloc(newCap, sizeof *newEntries);
    size_t newCount = 0;

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = state->symbols.entries[i];
        if (isHeaped(v)) {
            size_t const h = (uintptr_t)HRef<Symbol>::fromUnchecked(v).ptr()->hash.val();
        
            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const entry = newEntries + j;
                if (eq(*entry, Default)) {
                    *entry = v;
                    ++newCount;
                    break;
                }
            }
        }
    }
    
    free(state->symbols.entries);
    state->symbols.entries = newEntries;
    state->symbols.count = newCount;
    state->symbols.cap = newCap;
}

// `name` must not point into GC heap:
HRef<Symbol> intern(State* state, Str name) {
    Fixnum const hash = hashStr(name);
    
    IndexOfSymbolRes ires = indexOfSymbol(&state->symbols, hash, name);
    if (ires.exists) {
        return HRef<Symbol>::fromUnchecked(state->symbols.entries[ires.index]);;
    } else {
        size_t const newCount = state->symbols.count + 1;
        size_t const capacity = state->symbols.cap;
        if (capacity / 2 < newCount) {
            rehashSymbols(state);
            ires = indexOfSymbol(&state->symbols, hash, name);
        }
        
        HRef<Symbol> const symbol = createUninternedSymbol(state, hash, name);
        state->symbols.entries[ires.index] = symbol.oref();
        state->symbols.count = newCount;
        return symbol;
    }
}

// TODO: DRY wrt. `intern`
HRef<Symbol> internHeaped(State* state, HRef<String> name) {
    Str const nameStr = name.ptr()->str();
    Fixnum const hash = hashStr(nameStr);

    IndexOfSymbolRes ires = indexOfSymbol(&state->symbols, hash, nameStr);
    if (ires.exists) {
        return HRef<Symbol>::fromUnchecked(state->symbols.entries[ires.index]);;
    } else {
        size_t const newCount = state->symbols.count + 1;
        size_t const capacity = state->symbols.cap;
        if (capacity / 2 < newCount) {
            rehashSymbols(state);
            ires = indexOfSymbol(&state->symbols, hash, nameStr);
        }

        HRef<Symbol> const symbol = createUninternedSymbolFromHeaped(state, hash, name);
        state->symbols.entries[ires.index] = symbol.oref();
        state->symbols.count = newCount;
        return symbol;
    }
}

void pruneSymbols(SymbolTable* symbols) {
    size_t const cap = symbols->cap;
    for (size_t i = 0; i < cap; ++i) {
        ORef* const v = &symbols->entries[i];
        if (isHeaped(*v)) {
            Object* const fwdPtr = uncheckedORefToPtr(*v)->tryForwarded();
            *v = fwdPtr ? HRef(fwdPtr).oref() : Tombstone;
        }
    }
}

HRef<Loc> createLoc(State* state, HRef<String> filename, Fixnum byteIdx) {
    Loc* ptr = static_cast<Loc*>(state->heap.tospace.tryAlloc(state->types.loc.ptr()));
    if (mustCollect(ptr)) {
        pushStackRoot(state, &filename);
        collect(state);
        popStackRoots(state, 1);
        ptr = static_cast<Loc*>(state->heap.tospace.allocOrDie(state->types.loc.ptr()));
    }

    *ptr = Loc{
        .filename = filename,
        .byteIdx = byteIdx
    };

    return HRef{ptr};
}

HRef<Pair> allocPair(State* state) {
    Pair* ptr = (Pair*)state->heap.tospace.tryAlloc(state->types.pair.ptr());
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Pair*)state->heap.tospace.allocOrDie(state->types.pair.ptr());
    }
    
    return HRef(ptr);
}

HRef<Pair> createPair(State *state, ORef car, ORef cdr, ORef maybeLoc) {
    Pair* ptr = (Pair*)state->heap.tospace.tryAlloc(state->types.pair.ptr());
    if (mustCollect(ptr)) {
        pushStackRoot(state, &car);
        pushStackRoot(state, &cdr);
        pushStackRoot(state, &maybeLoc);
        collect(state);
        popStackRoots(state, 3);
        ptr = (Pair*)state->heap.tospace.allocOrDie(state->types.pair.ptr());
    }

    *ptr = Pair{.car = car, .cdr = cdr, .maybeLoc = maybeLoc};

    return HRef(ptr);
}

Method* tryAllocBytecodeMethod(
    State* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs
) {
    Method* ptr = (Method*)state->heap.tospace.tryAllocFlex(state->types.method.ptr(), arity);
    if (!ptr) { return ptr; }

    *ptr = Method{
        .nativeCode = (MethodCode)callBytecode,
        .code = code.oref(),
        .consts = consts.oref(),
        .hasVarArg = hasVarArg,
        .hash = hash,
        .maybeName = maybeName,
        .maybeFilenames = maybeFilenames,
        .maybeSrcByteIdxs = maybeSrcByteIdxs
    };

    return ptr;
}

Method* allocBytecodeMethodOrDie(
    State* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs
) {
    Method* ptr = (Method*)state->heap.tospace.allocFlexOrDie(state->types.method.ptr(), arity);

    *ptr = Method{
        .nativeCode = (MethodCode)callBytecode,
        .code = code.oref(),
        .consts = consts.oref(),
        .hasVarArg = hasVarArg,
        .hash = hash,
        .maybeName = maybeName,
        .maybeFilenames = maybeFilenames,
        .maybeSrcByteIdxs = maybeSrcByteIdxs
    };

    return ptr;
}

HRef<Method> allocBytecodeMethod(
    State* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs
) {
    Method* ptr = (Method*)state->heap.tospace.tryAllocFlex(state->types.method.ptr(), arity);
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&code);
        pushStackRoot(state, (ORef*)&consts);
        pushStackRoot(state, &maybeName);
        pushStackRoot(state, &maybeFilenames);
        pushStackRoot(state, &maybeSrcByteIdxs);
        collect(state);
        popStackRoots(state, 5);
        ptr = (Method*)state->heap.tospace.allocFlexOrDie(state->types.method.ptr(), arity);
    }

    *ptr = Method{
        .nativeCode = (MethodCode)callBytecode,
        .code = code.oref(),
        .consts = consts.oref(),
        .hasVarArg = hasVarArg,
        .hash = hash,
        .maybeName = maybeName,
        .maybeFilenames = maybeFilenames,
        .maybeSrcByteIdxs = maybeSrcByteIdxs
    };

    return HRef(ptr);
}

HRef<Method> vcreatePrimopMethod(
    State* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum fxArity,
    va_list va_domain
) {
    size_t const arity = (uintptr_t)fxArity.val();

    // Taking address of `va_arg(va_domain, HRef<Type>)` seems questionable so copy into fixed array to
    // allow GC:
    HRef<Type>* const domain = (HRef<Type>*)malloc(arity * sizeof *domain);
    for (size_t i = 0; i < arity; ++i) {
        domain[i] = HRef<Type>::fromUnchecked(va_arg(va_domain, ORef));
    }

    Method* ptr = (Method*)state->heap.tospace.tryAllocFlex(state->types.method.ptr(), fxArity);
    if (mustCollect(ptr)) {
        for (size_t i = 0; i < arity; ++i) {
            pushStackRoot(state, (ORef*)&domain[i]); // Not on stack but will not move either
        }
        collect(state);
        popStackRoots(state, arity);
        ptr = (Method*)state->heap.tospace.allocFlexOrDie(state->types.method.ptr(), fxArity);
    }

    uintptr_t const hash = fnv1aHash_n((uint8_t*)&nativeCode, sizeof nativeCode); // HACK

    *ptr = Method{
        .nativeCode = nativeCode,
        .code = Default,
        .consts = Default,
        .hasVarArg = Bool(hasVarArg),
        .hash = Fixnum((intptr_t)hash),
        .maybeName = Default,
        .maybeFilenames = Default,
        .maybeSrcByteIdxs = Default
    };
    memcpy(ptr->flexDataMut(), domain, arity * sizeof *domain); // Side benefit of the array: `memcpy`

    HRef<Method> method = HRef(ptr);
    pushStackRoot(state, (ORef*)&method);
    HRef<Symbol> const nameSym = intern(state, name);
    popStackRoots(state, 1);
    ptr = method.ptr(); // Post-GC reload
    ptr->maybeName = nameSym.oref();

    free(domain);
    return method;
}

HRef<Method> createPrimopMethod(
    State* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum arity, ...
) {
    va_list domain;
    va_start(domain, arity);
    HRef<Method> method = vcreatePrimopMethod(state, name, nativeCode, hasVarArg, arity, domain);
    va_end(domain);

    return method;
}

HRef<Closure> allocClosure(State* state, HRef<Method> method, Fixnum cloverCount) {
    Closure* ptr =
        (Closure*)state->heap.tospace.tryAllocFlex(state->types.closure.ptr(), cloverCount);
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&method);
        collect(state);
        popStackRoots(state, 1);
        ptr = (Closure*)state->heap.tospace.allocFlexOrDie(state->types.closure.ptr(), cloverCount);
    }

    ptr->method = method.oref();

    return HRef(ptr);
}

HRef<Continuation> allocContinuation(
    State* state, HRef<Method> method, Fixnum pc, Fixnum cloverCount
) {
    Continuation* ptr = (Continuation*)state->heap.tospace.tryAllocFlex(
        state->types.continuation.ptr(), cloverCount);
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&method);
        collect(state);
        popStackRoots(state, 1);
        ptr =(Continuation*)state->heap.tospace.allocFlexOrDie(
            state->types.continuation.ptr(), cloverCount);
    }

    ptr->method = method.oref();
    ptr->pc = pc;

    return HRef(ptr);
}

HRef<Knot> allocKnot(State* state) {
    Knot* ptr = (Knot*)state->heap.tospace.tryAlloc(state->types.knot.ptr());
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Knot*)state->heap.tospace.allocOrDie(state->types.knot.ptr());
    }

    return HRef(ptr);
}

HRef<InputFile> createInputFile(State* state, UTF8InputFile&& file) {
    InputFile* ptr = static_cast<decltype(ptr)>(
        state->heap.tospace.tryAlloc(state->types.inputFile.ptr()));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = static_cast<decltype(ptr)>(
            state->heap.tospace.allocOrDie(state->types.inputFile.ptr()));
    }

    *ptr = InputFile{std::move(file)};

    return HRef{ptr};
}

HRef<UnboundError> createUnboundError(State* state, HRef<Symbol> name) {
    UnboundError* ptr = (UnboundError*)state->heap.tospace.tryAlloc(state->types.unboundError.ptr());
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&name);
        collect(state);
        popStackRoots(state, 1);
        ptr = (UnboundError*)state->heap.tospace.allocOrDie(state->types.unboundError.ptr());
    }

    *ptr = UnboundError{.name = name};

    return HRef(ptr);
}

HRef<TypeError> createTypeError(State* state, HRef<Type> type, ORef val) {
    TypeError* ptr = (TypeError*)state->heap.tospace.tryAlloc(state->types.typeError.ptr());
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&type);
        pushStackRoot(state, &val);
        collect(state);
        popStackRoots(state, 2);
        ptr = (TypeError*)state->heap.tospace.allocOrDie(state->types.typeError.ptr());
    }

    *ptr = TypeError{.type = type, .val = val};

    return HRef(ptr);
}

HRef<ArityError> createArityError(State* state, HRef<Closure> callee, Fixnum callArgc) {
    ArityError* ptr = (ArityError*)state->heap.tospace.tryAlloc(state->types.arityError.ptr());
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&callee);
        collect(state);
        popStackRoots(state, 1);
        ptr = (ArityError*)state->heap.tospace.allocOrDie(state->types.arityError.ptr());
    }

    *ptr = ArityError{.callee = callee, .callArgc = callArgc};

    return HRef(ptr);
}

HRef<InapplicableError> createInapplicableError(State* state, HRef<Multimethod> callee) {
    InapplicableError* ptr =
        (InapplicableError*)state->heap.tospace.tryAlloc(state->types.inapplicableError.ptr());
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&callee);
        collect(state);
        popStackRoots(state, 1);
        ptr = (InapplicableError*)state->heap.tospace.allocOrDie(
            state->types.inapplicableError.ptr());
    }

    *ptr = InapplicableError{.callee = callee};

    return HRef(ptr);
}

HRef<FatalError> createOverflowError(
    State* state, HRef<Closure> callee, Fixnum x, Fixnum y
) {
    Fixnum const count = Fixnum{3l};

    FatalError* ptr =
        (FatalError*)state->heap.tospace.tryAllocFlex(state->types.fatalError.ptr(), count);
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&callee);
        collect(state);
        popStackRoots(state, 1);
        ptr = (FatalError*)state->heap.tospace.allocFlexOrDie(state->types.fatalError.ptr(), count);
    }
    HRef<FatalError> res = HRef(ptr);

    pushStackRoot(state, (ORef*)&callee);
    pushStackRoot(state, (ORef*)&res);
    HRef<Symbol> const name = intern(state, strLit("overflow"));
    popStackRoots(state, 2);
    ptr = res.ptr();

    *ptr = FatalError{.name = name};
    ORef* const irritantsMut = const_cast<ORef*>(ptr->flexData());
    irritantsMut[0] = callee.oref();
    irritantsMut[1] = x.oref();
    irritantsMut[2] = y.oref();

    return res;
}

HRef<FatalError> createDivByZeroError(
    State* state, HRef<Closure> callee, Fixnum x, Fixnum y
) {
    Fixnum const count = Fixnum{3l};

    FatalError* ptr =
        (FatalError*)state->heap.tospace.tryAllocFlex(state->types.fatalError.ptr(), count);
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&callee);
        collect(state);
        popStackRoots(state, 1);
        ptr = (FatalError*)state->heap.tospace.allocFlexOrDie(state->types.fatalError.ptr(), count);
    }
    HRef<FatalError> res = HRef(ptr);

    pushStackRoot(state, (ORef*)&callee);
    pushStackRoot(state, (ORef*)&res);
    HRef<Symbol> const name = intern(state, strLit("divide-by-zero"));
    popStackRoots(state, 2);
    ptr = res.ptr();

    *ptr = FatalError{.name = name};
    ORef* const irritantsMut = const_cast<ORef*>(ptr->flexData());
    irritantsMut[0] = callee.oref();
    irritantsMut[1] = x.oref();
    irritantsMut[2] = y.oref();

    return res;
}

} // namespace

extern "C" Vshs_State* tryCreateState(size_t heapSize, int argc, char const* argv[]) {
    return (Vshs_State*)State::tryCreate(heapSize, argc, argv);
}

extern "C" void freeState(Vshs_State* state) { freeState((State*)state); }

extern "C" void pushStackRoot(Vshs_State* state, ORef* stackLoc) {
    pushStackRoot((State*)state, stackLoc);
}

extern "C" void popStackRoots(Vshs_State* state, size_t count) {
    popStackRoots((State*)state, count);
}
