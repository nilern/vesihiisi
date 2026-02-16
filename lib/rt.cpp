#include "rt.hpp"

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <new>
#include <algorithm>
#include <vector>

#include "util/util.hpp"
#include "value.hpp"
#include "heap.hpp"
#include "flyweights.hpp"
#include "namespace.hpp"
#include "primops.hpp"
#include "compiler/bytecodegen.hpp"

namespace {

char const* const typeNames[] = {
    "",
    "<fixnum>",
    "<char>",
    "<bool>",
    "<flonum>",
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
    "<pointer>",
    "<input-file>",
    "<fatal-error>",
    "<unbound-error>",
    "<type-error>",
    "<arity-error>",
    "<inapplicable-error>"
};
static_assert(sizeof(typeNames) / sizeof(*typeNames) == BOOTSTRAP_TYPE_COUNT);

RootGuard::RootGuard(RT* t_state, ORef* handle) : state{t_state} {
    state->shadowstack.push_back(handle);
}

RootGuard::RootGuard(RootGuard&& that) {
    state = that.state;
    that.state = nullptr;
}

RootGuard& RootGuard::operator=(RootGuard&& that) {
    if (state) { state->shadowstack.pop_back(); }
    state = that.state;
    that.state = nullptr;

    return *this;
}

RootGuard::~RootGuard() { if (state) { state->shadowstack.pop_back(); } }

bool tryCreateNamespace(
    Heap& heap, HRef<Namespace>* dest, Type const* nsType, Type const* arrayType
) {
    Fixnum const count = Fixnum{2l};
    ArrayMut* const keys = (ArrayMut*)heap.tryAllocFlex(arrayType, count);
    if (!keys) { return false; }
    ArrayMut* const vals = (ArrayMut*)heap.tryAllocFlex(arrayType, count);
    if (!vals) { return false; }
    Namespace* const ptr = (Namespace*)heap.tryAlloc(nsType);
    if (!ptr) { return false; }

    *dest = HRef{new (ptr) Namespace{HRef{keys}, HRef{vals}, count}};
    return true;
}

void freeRT(RT* state) { delete(state); }

[[nodiscard]]
bool markRoots(RT* state) {
    state->method = TRY_NULLOPT_TO_FALSE(state->heap.mark(state->method));

    // OPTIMIZE: Only mark registers that are actually live (requires emitting liveness bitmaps for
    // safepoints:
    for (size_t i = 0; i < REG_COUNT; ++i) {
        state->regs[i] = TRY_NULLOPT_TO_FALSE(state->heap.mark(state->regs[i]));
    }

    state->ns = HRef<Namespace>::fromUnchecked(TRY_NULLOPT_TO_FALSE(state->heap.mark(state->ns)));

    for (size_t i = 0; i < BOOTSTRAP_TYPE_COUNT; ++i) {
        state->typesArray[i] =
            HRef<Type>::fromUnchecked(TRY_NULLOPT_TO_FALSE(state->heap.mark(state->typesArray[i])));
    }

    for (size_t i = 0; i < BOOTSTRAP_SINGLETON_COUNT; ++i) {
        state->singletonsArray[i] =
            TRY_NULLOPT_TO_FALSE(state->heap.mark(state->singletonsArray[i]));
    }

    state->debug = HRef<Var>::fromUnchecked(TRY_NULLOPT_TO_FALSE(state->heap.mark(state->debug)));
    state->errorHandler =
        HRef<Var>::fromUnchecked(TRY_NULLOPT_TO_FALSE(state->heap.mark(state->errorHandler)));

    for (ORef* const rootHandle : state->shadowstack) {
        *rootHandle = TRY_NULLOPT_TO_FALSE(state->heap.mark(*rootHandle));
    }

    return true;
}

void updateWeakRefs(RT* state) {
    state->symbols.prune(*state);
    state->specializations.prune(*state);
}

void initSpecialPurposeRegs(RT* state) {
    ORef const anyMethod = state->method;
    if (isHeaped(anyMethod)) {
        auto const methodPtr = HRef<Method>::fromUnchecked(anyMethod);
        state->code = HRef<ByteArray>::fromUnchecked(methodPtr->code)->flexData();
        state->consts = HRef<ArrayMut>::fromUnchecked(methodPtr->consts)->itemsMut().data();
    }
}

template<typename T, bool isBytes>
Type* tryCreateFixedType(Heap& heap, Type const* typeType) {
    Type* const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    return new (type) Type{
        Fixnum((int64_t)sizeof(T)), Fixnum((int64_t)alignof(T)), Bool{isBytes}, False, False,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

template<typename T> requires std::is_base_of_v<AnyIndexedObject<T, typename T::Item>, T>
Type* tryCreateIndexedType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    constexpr bool isBytes = !std::is_base_of_v<ORef, typename T::Item>;
    constexpr size_t align = isBytes
        ? std::max(alignof(typename T::Item), objectMinAlign)
        : alignof(ORef);

    return new (type) Type{
        Fixnum{0l}, Fixnum{int64_t(align)}, Bool{isBytes}, False, True,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

Type* tryCreateTypeType(Heap& heap) {
    auto const bootstrapTypeType = Type{
        Fixnum((intptr_t)sizeof(Type)), Fixnum((intptr_t)alignof(Type)), False, False, False,
        Fixnum{0l}, HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
    
    auto const typeType = static_cast<Type*>(heap.tryAlloc(&bootstrapTypeType));
    if (!typeType) { return nullptr; }

    *((Header*)typeType - 1) = Header{typeType}; // Init header, closing loop

    return new (typeType) Type{bootstrapTypeType};
}

Type* tryCreateAnyType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    return new (type) Type{ // TODO: Avoid requiring some nonsensical values like this:
        Fixnum{0l}, Fixnum((intptr_t)objectMinAlign), True, False, False,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

Type* tryCreateSymbolType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    return new (type) Type{
        Fixnum((intptr_t)sizeof(Symbol)), Fixnum((intptr_t)alignof(Symbol)), False, False, False,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

Type* tryCreateEmptyType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    return new (type) Type{
        Fixnum{0l}, Fixnum((intptr_t)objectMinAlign), True, False, False,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

Type* tryCreateMethodType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    return new (type) Type{
        Fixnum((int64_t)sizeof(Method)), Fixnum((int64_t)alignof(Method)), False, True, True,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

Type* tryCreateClosureType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    return new (type) Type{
        Fixnum((int64_t)sizeof(Closure)), Fixnum((int64_t)alignof(Closure)), False, False, True,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

Type* tryCreateContinuationType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    return new (type) Type{
        Fixnum((int64_t)sizeof(Continuation)), Fixnum((int64_t)alignof(Continuation)), False, False,
        True,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

Type* tryCreateFatalErrorType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }

    return new (type) Type{
        Fixnum((int64_t)sizeof(FatalError)), Fixnum((int64_t)alignof(FatalError)), False, False,
        True,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

Type* tryCreateImmType(Heap& heap, Type const* typeType) {
    auto const type = static_cast<Type*>(heap.tryAlloc(typeType));
    if (!type) { return nullptr; }
    
    return new (type) Type{ // TODO: Avoid requiring some nonsensical values like this:
        Fixnum{0l}, Fixnum((intptr_t)objectMinAlign), True, False, False,
        Fixnum::fromUnchecked(ORef{0}), HRef<Symbol>::fromUnchecked(ORef{0}) // HACK
    };
}

HRef<Method> vcreatePrimopMethod(
    RT* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum arity, va_list domain);
HRef<Method> createPrimopMethod(
    RT* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum arity, ...);

HRef<Closure> allocClosure(RT* state, HRef<Method> method, Fixnum cloverCount);

HRef<Var> getVar(RT* state, HRef<Namespace> nsRef, HRef<Symbol> name);

void installPrimordial(RT* state, Str name, ORef v) {
    auto const vG = state->pushRoot(&v);

    HRef<Symbol> const symbol = intern(state, name);
    HRef<Var> const var = getVar(state, state->ns, symbol);

    var->val().set(*state, v);
}

void installPrimop(
    RT* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum arity, ...
) {
    va_list domain;
    va_start(domain, arity);
    HRef<Method> const method =
        vcreatePrimopMethod(state, name, nativeCode, hasVarArg, arity, domain);
    va_end(domain);
    HRef<Closure> const closure = allocClosure(state, method, Fixnum{0l});
    installPrimordial(state, name, closure);
}

Var* tryCreateUnboundVar(
    Heap& heap, Type const* unboundType, HRef<Unbound> unbound);

void nameType(RT* state, HRef<Type> type, Str name) {
    auto const typeRefG = state->pushRoot(&type);
    HRef<Symbol> const nameSym = intern(state, name);

    const_cast<Fixnum&>(type->hash) = nameSym->hash; // HACK
    SlotMut{&*type, const_cast<HRef<Symbol>&>(type->name)}.set(*state, nameSym); // HACK
}

bool debugFromArgv(int argc, char const* argv[]) {
    char const* arg;
    for (int i = 1; i < argc && *(arg = argv[i]) == '-'; ++i) {
        ++arg;

        if (*arg != '\0' && *arg != '-') { // Short flag(s)
            for (; *arg != '\0'; ++arg) {
                if (*arg == 'd') {
                    return true;
                }
            }
        }
    }

    return false;
}

HRef<Array> createCommandLine(RT* state, int argc, char const* argv[]) {
    HRef<Array> commandLine = createArray(state, Fixnum{int64_t(argc)});
    auto const commandLineG = state->pushRoot(&commandLine);

    for (size_t i = 0; i < size_t(argc); ++i) {
        char const* const segCStr = argv[i];
        HRef<String> const seg =
            createString(state, Str{reinterpret_cast<uint8_t const*>(segCStr), strlen(segCStr)});
        const_cast<ORef*>(commandLine->flexData())[i] = seg; // Initializing store
    }

    return commandLine;
}

RT::RT(
    Heap heap, NamedTypes types, NamedSingletons singletons, HRef<Namespace> ns,
    HRef<Var> t_debug, HRef<Var> t_errorHandler
) :
    method{Default},
    code{nullptr},
    pc{0},
    regs{},
    consts{nullptr, nullptr},
    ns{ns},
    entryRegc{0}, // Intentionally invalid
    domainChecking{DomainChecking::CHECK},

    heap{std::move(heap)},
    types{types},

    symbols{},
    specializations{},

    singletons{singletons},
    debug{t_debug},
    errorHandler{t_errorHandler},

    shadowstack{}
{}

RT* RT::tryCreate(size_t heapSize, char const* vshsHome, int argc, char const* argv[]) {
    Heap heap = Heap::tryCreate(heapSize);
    if (!heap.isValid()) { return nullptr; }
    
    Type* const typeType = tryCreateTypeType(heap);
    if (!typeType) { return nullptr; }
    Type * const anyType = tryCreateAnyType(heap, typeType);
    if (!anyType) { return nullptr; }
    Type* const stringType = tryCreateIndexedType<String>(heap, typeType);
    if (!stringType) { return nullptr; }
    Type* const stringIteratorType =
        tryCreateFixedType<StringIterator, false>(heap, typeType);
    if (!stringIteratorType) { return nullptr; }
    Type* const arrayType = tryCreateIndexedType<Array>(heap, typeType);
    if (!arrayType) { return nullptr; }
    Type* const arrayMutType = tryCreateIndexedType<ArrayMut>(heap, typeType);
    if (!arrayMutType) { return nullptr; }
    Type* const byteArrayType = tryCreateIndexedType<ByteArray>(heap, typeType);
    if (!byteArrayType) { return nullptr; }
    Type* const byteArrayMutType = tryCreateIndexedType<ByteArrayMut>(heap, typeType);
    if (!byteArrayMutType) { return nullptr; }
    Type* const symbolType = tryCreateSymbolType(heap, typeType);
    if (!symbolType) { return nullptr; }
    Type* const locType = tryCreateFixedType<Loc, false>(heap, typeType);
    if (!locType) { return nullptr; }
    Type* const pairType = tryCreateFixedType<Pair, false>(heap, typeType);
    if (!pairType) { return nullptr; }
    Type* const emptyListType = tryCreateEmptyType(heap, typeType);
    if (!emptyListType) { return nullptr; }
    Type* const methodType = tryCreateMethodType(heap, typeType);
    if (!methodType) { return nullptr; }
    Type* const closureType = tryCreateClosureType(heap, typeType);
    if (!closureType) { return nullptr; }
    Type* const multimethodType = tryCreateFixedType<Multimethod, false>(heap, typeType);
    if (!multimethodType) { return nullptr; }
    Type* const continuationType = tryCreateContinuationType(heap, typeType);
    if (!continuationType) { return nullptr; }
    Type* const unboundType = tryCreateEmptyType(heap, typeType);
    if (!unboundType) { return nullptr; }
    Type* const varType = tryCreateFixedType<Var, false>(heap, typeType);
    if (!varType) { return nullptr; }
    Type* const knotType = tryCreateFixedType<Knot, false>(heap, typeType);
    if (!knotType) { return nullptr; }
    Type* const nsType = tryCreateFixedType<Namespace, false>(heap, typeType);
    if (!nsType) { return nullptr; }
    Type* const inputFileType = tryCreateFixedType<InputFile, true>(heap, typeType);
    if (!inputFileType) { return nullptr; }
    Type* const endType = tryCreateEmptyType(heap, typeType);
    if (!endType) { return nullptr; }
    Type* const pointerType = tryCreateFixedType<Pointer, true>(heap, typeType);
    if (!pointerType) { return nullptr; }
    Type* const fatalErrorType = tryCreateFatalErrorType(heap, typeType);
    if (!fatalErrorType) { return nullptr; }
    Type* const unboundErrorType = tryCreateFixedType<UnboundError, false>(heap, typeType);
    if (!unboundType) { return nullptr; }
    Type* const typeErrorType = tryCreateFixedType<TypeError, false>(heap, typeType);
    if (!typeErrorType) { return nullptr; }
    Type* const arityErrorType = tryCreateFixedType<ArityError, false>(heap, typeType);
    if (!arityErrorType) { return nullptr; }
    Type* const inapplicableErrorType =
        tryCreateFixedType<InapplicableError, false>(heap, typeType);
    if (!inapplicableErrorType) { return nullptr; }
    
    Type* const fixnumType = tryCreateImmType(heap, typeType);
    if (!fixnumType) { return nullptr; }
    Type* const charType = tryCreateImmType(heap, typeType);
    if (!charType) { return nullptr; }
    Type* const flonumType = tryCreateImmType(heap, typeType);
    if (!flonumType) { return nullptr; }
    Type* const boolType = tryCreateImmType(heap, typeType);
    if (!boolType) { return nullptr; }

    End* const end = (End*)heap.tryAlloc(endType);
    if (!end) { return nullptr; }
    EmptyList* const emptyList = (EmptyList*)heap.tryAlloc(emptyListType);
    if (!emptyList) { return nullptr; }
    Unbound* const unbound = (Unbound*)heap.tryAlloc(unboundType);
    if (!unbound) { return nullptr; }
    Continuation* const exit =
        (Continuation*)heap.tryAllocFlex(continuationType, Fixnum{0l});
    if (!exit) { return nullptr; }
    const_cast<Fixnum&>(exit->pc) = Fixnum{0l}; // HACK: Init

    // HACK:
    Var* const debugPlaceholder = tryCreateUnboundVar(heap, varType, HRef(unbound));
    if (!debugPlaceholder) { return nullptr; }
    Var* const errorHandlerPlaceholder = tryCreateUnboundVar(heap, varType, HRef(unbound));
    if (!errorHandlerPlaceholder) { return nullptr; }

    HRef<Namespace> ns = HRef<Namespace>::fromUnchecked(ORef{0}); // HACK;
    if (!tryCreateNamespace(heap, &ns, nsType, arrayType)) { return nullptr; }

    RT* const dest = new (std::nothrow) RT{
        std::move(heap),
        {
            .paddington = HRef{anyType}, // The closest thing, although it matters not
            .fixnum = HRef(fixnumType),
            .charr = HRef(charType),
            .booll = HRef(boolType),
            .flonum = HRef(flonumType),

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
            .pointer = HRef{pointerType},
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
        HRef{debugPlaceholder},
        HRef{errorHandlerPlaceholder}
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
            installPrimordial(dest, nameStr, dest->typesArray[i]);
        }
    }

    {
        Str const debugName = strLit("*vm-debug*");
        installPrimordial(dest, debugName, Bool{debugFromArgv(argc, argv)});
        HRef<Symbol> const debugNameSym = intern(dest, debugName); // Cannot (alloc => GC)
        FindVarRes const varRes = findVar(dest->ns, debugNameSym);
        assert(varRes.type == FindVarRes::NS_FOUND_VAR);
        dest->debug = varRes.var;
    }
    {
        HRef<Method> const abortMethod = // TODO: `PrimopAbort::install(*dest);`
            createPrimopMethod(dest, strLit("abort"), (MethodCode)primopAbort,
                               false, Fixnum{1l}, dest->types.any);
        HRef<Closure> abortClosure = allocClosure(dest, abortMethod, Fixnum{0l});

        Str const errorHandlerName = strLit("*error-handler*");
        installPrimordial(dest, errorHandlerName, abortClosure);
        HRef<Symbol> const errorHandlerSym = intern(dest, errorHandlerName); // Cannot (alloc => GC)
        FindVarRes const varRes = findVar(dest->ns, errorHandlerSym);
        assert(varRes.type == FindVarRes::NS_FOUND_VAR);
        dest->errorHandler = varRes.var;
    }
    installPrimordial(dest, strLit("end"), dest->singletons.end);
    installPrimordial(dest, strLit("standard-input"), createInputFile(dest, UTF8InputFile{stdin}));
    installPrimordial(dest, strLit("*vshs-home*"),
                      createString(dest, Str{reinterpret_cast<uint8_t const*>(vshsHome),
                                             strlen(vshsHome)}));
    installPrimordial(dest, strLit("*command-line*"), createCommandLine(dest, argc, argv));

    PrimopApplyArray::install(*dest);
    PrimopApplyArrayMut::install(*dest);
    PrimopApplyList::install(*dest);
    PrimopCallCC::install(*dest);
    PrimopContinue::install(*dest);
    PrimopIdentical::install(*dest);
    PrimopTypeOf::install(*dest);
    PrimopMakeSlotsType::install(*dest);
    PrimopMake::install(*dest);
    PrimopSlotGet::install(*dest);
    PrimopSlotSet::install(*dest);
    PrimopMakeFlex::install(*dest);
    PrimopFlexCount::install(*dest);
    PrimopFlexGet::install(*dest);
    PrimopFlexSet::install(*dest);
    PrimopFlexCopy::install(*dest);
    PrimopFlexClone::install(*dest);
    PrimopFxAdd::install(*dest);
    PrimopFxSub::install(*dest);
    PrimopFxMul::install(*dest);
    PrimopFxQuot::install(*dest);
    PrimopFxLt::install(*dest);
    PrimopFixnumToFlonum::install(*dest);
    PrimopFlAdd::install(*dest);
    PrimopFlSub::install(*dest);
    PrimopFlMul::install(*dest);
    PrimopFlDiv::install(*dest);
    PrimopCharLt::install(*dest);
    PrimopCharToInteger::install(*dest);
    PrimopCharIsAlphabetic::install(*dest);
    PrimopCharIsNumeric::install(*dest);
    PrimopCharIsWhitespace::install(*dest);
    PrimopArrayMutToString::install(*dest);
    PrimopStringIteratorPeek::install(*dest);
    PrimopStringIteratorNext::install(*dest);
    PrimopStringToSymbol::install(*dest);
    PrimopGensym::install(*dest);
    PrimopFileExists::install(*dest);
    PrimopOpenInputFile::install(*dest);
    PrimopClosePort::install(*dest);
    PrimopPeekChar::install(*dest);
    PrimopReadChar::install(*dest);
    PrimopWrite::install(*dest);
    PrimopWriteChar::install(*dest);
    PrimopWriteString::install(*dest);
    PrimopFlushOutputPort::install(*dest);
    PrimopCurrentSecond::install(*dest);
    PrimopCurrentJiffy::install(*dest);
    PrimopJiffiesPerSecond::install(*dest);
    PrimopResolve::install(*dest);
    PrimopEval::install(*dest);
    PrimopContinuationCallLoc::install(*dest);
    PrimopExit::install(*dest);
    PrimopOpenForeignLibrary::install(*dest);
    PrimopCloseForeignLibrary::install(*dest);
    PrimopGetForeign::install(*dest);

    return dest;
}

Object* RT::alloc(HRef<Type> type) {
    Object* obj = heap.tryAlloc(&*type);
    if (mustCollect(obj)) {
        auto const typeG = pushRoot(&type);
        collect(this);
        obj = heap.allocOrDie(&*type);
    }

    return obj;
}

uint64_t typeTag(RT& rt, HRef<Type> type) {
    assert(!eq(type, Flonum::reify(rt)));

    HRef<Type> const* typesBegin = rt.typesArray;
    HRef<Type> const* begin = typesBegin + ptrdiff_t(TaggedType::FIXNUM);
    HRef<Type> const* end = rt.typesArray + ptrdiff_t(TaggedType::FLONUM);
    auto const it = std::find(begin, end, type);
    return it != end ? uint64_t(std::distance(typesBegin, it)) : uint64_t(TaggedType::HEAPED);
}

HRef<Type> typeOf(RT const* state, ORef v) {
    TaggedType const tag = getTag(v);
    return tag == TaggedType::HEAPED
        ? HRef<Object>::fromUnchecked(v)->header()->type()
        : state->typesArray[(size_t)tag];
}

Type const* typePtrOf(RT const* state, ORef v) {
    TaggedType const tag = getTag(v);
    return tag == TaggedType::HEAPED
        ? HRef<Object>::fromUnchecked(v)->header()->typePtr()
        : &*state->typesArray[(size_t)tag];
}

bool isa(RT const* state, HRef<Type> type, ORef v) {
    if (eq(type, state->types.any)) { return true; }

    return eq(typeOf(state, v), type);
}

[[maybe_unused]]
void assertRTInTospace(RT const* state) {
    if (isHeaped(state->method)) {
        assert(state->heap.evacuated(&*HRef<Object>::fromUnchecked(state->method)));
        assert(state->heap.evacuated(reinterpret_cast<Object const*>(state->code)));
        assert(state->heap.evacuated(reinterpret_cast<Object const*>(&state->consts[0].get())));
    }

    // TODO: When we start only marking live regs, this has to only check those as well to avoid
    // false positives:
    for (size_t i = 0; i < REG_COUNT; ++i) {
        ORef const reg = state->regs[i];
        if (isHeaped(reg)) {
            assert(state->heap.evacuated(&*HRef<Object>::fromUnchecked(reg)));
        }
    }

    assert(state->heap.evacuated(&*state->ns));

    for (size_t i = 0; i < BOOTSTRAP_TYPE_COUNT; ++i) {
        assert(state->heap.evacuated(&*state->typesArray[i]));
    }

    for ([[maybe_unused]] HRef<Symbol> const& symbol : state->symbols) {
        assert(state->heap.evacuated(&*symbol));
    }

    for ([[maybe_unused]] HRef<Method> const& specialization : state->specializations) {
        assert(state->heap.evacuated(&*specialization));
    }

    for (size_t i = 0; i < BOOTSTRAP_SINGLETON_COUNT; ++i) {
        ORef const v = state->singletonsArray[i];
        if (isHeaped(v)) {
            assert(state->heap.evacuated(&*HRef<Object>::fromUnchecked(v)));
        }
    }

    assert(state->heap.evacuated(&*state->debug));
    assert(state->heap.evacuated(&*state->errorHandler));

    for (ORef* const v : state->shadowstack) {
        if (isHeaped(*v)) {
            assert(state->heap.evacuated(&*HRef<Object>::fromUnchecked(*v)));
        }
    }
}

[[nodiscard]]
bool defaultPrepCollection(RT* state) { return markRoots(state); }

[[nodiscard]]
bool completeCollection(RT* state) {
    if (!state->heap.collect()) { return false; }

    updateWeakRefs(state);

    state->heap.refurbish();
    initSpecialPurposeRegs(state);

#ifndef NDEBUG
    assertRTInTospace(state);
#endif

    return true;
}

void collect(RT* state) {
    do {
        while (!defaultPrepCollection(state)) {}
    } while (!completeCollection(state));
}

void collectTracingIR(RT* state, struct IRFn* fn, MethodBuilder* builder) {
    do {
        while (!(
            defaultPrepCollection(state)
            && markIRFn(state, fn)
            && builder->mark(*state)
        )) {}
    } while (!completeCollection(state));

#ifndef NDEBUG
    assertIRFnInTospace(state, fn);
    builder->assertInTospace(*state);
#endif
}

HRef<Type> createSlotsType(RT* state, HRef<Symbol> name, Fixnum slotCount, Bool isFlex) {
    Type* ptr = static_cast<decltype(ptr)>(state->heap.tryAlloc(&*state->types.type));
    if (mustCollect(ptr)) {
        auto const nameG = state->pushRoot(&name);
        collect(state);
        ptr = static_cast<decltype(ptr)>(state->heap.allocOrDie(&*state->types.type));
    }

    Fixnum const minSize = !isFlex.val()
        ? Fixnum{int64_t(size_t(slotCount.val()) * sizeof(ORef))}
        : Fixnum{int64_t(size_t(slotCount.val() - 1) * sizeof(ORef))};

    return HRef{new (ptr) Type{
        minSize, Fixnum((int64_t)objectMinAlign), False, False, isFlex, name->hash, name
    }};
}

String* allocString(RT* state, Fixnum byteCount) {
    String* ptr =
        static_cast<String*>(state->heap.tryAllocFlex(&*state->types.string,
                             byteCount));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = static_cast<String*>(state->heap.allocFlexOrDie(&*state->types.string,
                                   byteCount));
    }

    return ptr;
}

HRef<String> createString(RT* state, Str str) {
    String* const string = allocString(state, Fixnum((intptr_t)str.len));
    
    memcpy(const_cast<uint8_t*>(string->flexData()), str.data, str.len);
    
    return HRef{string};
}

HRef<Array> createArray(RT* state, Fixnum count) {
    Array* ptr = tryAllocArray(state, count);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocArrayOrDie(state, count);
    }

    return HRef((Array*)ptr);
}

HRef<ArrayMut> createArrayMut(RT* state, Fixnum count) {
    ArrayMut* ptr = tryAllocArrayMut(state, count);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocArrayMutOrDie(state, count);
    }

    return HRef((ArrayMut*)ptr);
}

HRef<ByteArrayMut> createByteArrayMut(RT* state, Fixnum count) {
    ByteArrayMut* ptr = static_cast<ByteArrayMut*>(
        state->heap.tryAllocFlex(&*state->types.byteArrayMut, count));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = static_cast<ByteArrayMut*>(
            state->heap.allocFlexOrDie(&*state->types.byteArrayMut, count));
    }

    return HRef{ptr};
}

HRef<Loc> createLoc(RT* state, HRef<String> filename, Fixnum byteIdx) {
    Loc* ptr = static_cast<Loc*>(state->heap.tryAlloc(&*state->types.loc));
    if (mustCollect(ptr)) {
        auto const filenameG = state->pushRoot(&filename);
        collect(state);
        ptr = static_cast<Loc*>(state->heap.allocOrDie(&*state->types.loc));
    }

    return HRef{new (ptr) Loc{filename, byteIdx}};
}

HRef<Pair> allocPair(RT* state) {
    Pair* ptr = (Pair*)state->heap.tryAlloc(&*state->types.pair);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Pair*)state->heap.allocOrDie(&*state->types.pair);
    }
    
    return HRef(ptr);
}

HRef<Pair> createPair(RT *state, ORef car, ORef cdr, ORef maybeLoc) {
    Pair* ptr = (Pair*)state->heap.tryAlloc(&*state->types.pair);
    if (mustCollect(ptr)) {
        auto const carG = state->pushRoot(&car);
        auto const cdrG = state->pushRoot(&cdr);
        auto const maybeLocG = state->pushRoot(&maybeLoc);
        collect(state);
        ptr = (Pair*)state->heap.allocOrDie(&*state->types.pair);
    }

    return HRef{new (ptr) Pair{car, cdr, maybeLoc}};
}

Method* tryAllocBytecodeMethod(
    RT* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs
) {
    auto const ptr =
        static_cast<Method*>(state->heap.tryAllocFlex(&*state->types.method, arity));
    if (!ptr) { return ptr; }

    return new (ptr) Method{
        reinterpret_cast<MethodCode>(callBytecode), code, consts, hasVarArg, hash, maybeName,
        maybeFilenames, maybeSrcByteIdxs, ORefSpan{} // leave `domain` to `Default`s
    };
}

Method* allocBytecodeMethodOrDie(
    RT* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs
) {
    auto const ptr =
        static_cast<Method*>(state->heap.allocFlexOrDie(&*state->types.method, arity));

    return new (ptr) Method{
        reinterpret_cast<MethodCode>(callBytecode), code, consts, hasVarArg, hash, maybeName,
        maybeFilenames, maybeSrcByteIdxs, ORefSpan{} // leave `domain` to `Default`s
    };
}

HRef<Method> allocBytecodeMethod(
    RT* state, HRef<ByteArray> code, HRef<ArrayMut> consts, Fixnum arity, Bool hasVarArg,
    Fixnum hash, ORef maybeName, ORef maybeFilenames, ORef maybeSrcByteIdxs
) {
    auto ptr = static_cast<Method*>(state->heap.tryAllocFlex(&*state->types.method, arity));
    if (mustCollect(ptr)) {
        auto const codeG = state->pushRoot(&code);
        auto const constsG = state->pushRoot(&consts);
        auto const maybeNameG = state->pushRoot(&maybeName);
        auto const maybeFilenamesG = state->pushRoot(&maybeFilenames);
        auto const maybeSrcByteIdxsG = state->pushRoot(&maybeSrcByteIdxs);
        collect(state);
        ptr =
            static_cast<Method*>(state->heap.allocFlexOrDie(&*state->types.method, arity));
    }

    return HRef{new (ptr) Method{
            reinterpret_cast<MethodCode>(callBytecode), code, consts, hasVarArg, hash, maybeName,
            maybeFilenames, maybeSrcByteIdxs, ORefSpan{} // leave `domain` to `Default`s
    }};
}

HRef<Method> vcreatePrimopMethod(
    RT* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum fxArity,
    va_list va_domain
) {
    size_t const arity = (uintptr_t)fxArity.val();

    // Taking address of `va_arg(va_domain, HRef<Type>)` seems questionable so copy into fixed array to
    // allow GC:
    HRef<Type>* const domain = (HRef<Type>*)malloc(arity * sizeof *domain);
    for (size_t i = 0; i < arity; ++i) {
        domain[i] = HRef<Type>::fromUnchecked(va_arg(va_domain, ORef));
    }

    Method* ptr = (Method*)state->heap.tryAllocFlex(&*state->types.method, fxArity);
    if (mustCollect(ptr)) {
        auto domainRoots = std::vector<RootGuard>{};
        domainRoots.reserve(arity);
        for (size_t i = 0; i < arity; ++i) {
            domainRoots.push_back(state->pushRoot(domain + i));
        }
        collect(state);
        ptr = (Method*)state->heap.allocFlexOrDie(&*state->types.method, fxArity);
    }

    uintptr_t const hash = fnv1aHash_n((uint8_t*)&nativeCode, sizeof nativeCode); // HACK

    new (ptr) Method{
        nativeCode, Default, Default, Bool{hasVarArg}, Fixnum{int64_t(hash)}, Default, Default,
        Default, ORefSpan{static_cast<ORef*>(domain), arity}
    };

    HRef<Method> method = HRef(ptr);
    auto const methodG = state->pushRoot(&method);
    HRef<Symbol> const nameSym = intern(state, name);
    ptr = &*method; // Post-GC reload
    SlotMut{ptr, const_cast<ORef&>(ptr->maybeName)}.set(*state, nameSym); // HACK

    free(domain);
    return method;
}

HRef<Method> createPrimopMethod(
    RT* state, Str name, MethodCode nativeCode, bool hasVarArg, Fixnum arity, ...
) {
    va_list domain;
    va_start(domain, arity);
    HRef<Method> method = vcreatePrimopMethod(state, name, nativeCode, hasVarArg, arity, domain);
    va_end(domain);

    return method;
}

HRef<Closure> allocClosure(RT* state, HRef<Method> method, Fixnum cloverCount) {
    Closure* ptr =
        (Closure*)state->heap.tryAllocFlex(&*state->types.closure, cloverCount);
    if (mustCollect(ptr)) {
        auto const methodG = state->pushRoot(&method);
        collect(state);
        ptr = (Closure*)state->heap.allocFlexOrDie(&*state->types.closure, cloverCount);
    }

    const_cast<ORef&>(ptr->method) = method; // Initing so `const_cast` and no write barrier

    return HRef(ptr);
}

HRef<Continuation> allocContinuation(
    RT* state, HRef<Method> method, Fixnum pc, Fixnum cloverCount
) {
    Continuation* ptr = (Continuation*)state->heap.tryAllocFlex(
        &*state->types.continuation, cloverCount);
    if (mustCollect(ptr)) {
        auto const methodG = state->pushRoot(&method);
        collect(state);
        ptr =(Continuation*)state->heap.allocFlexOrDie(
            &*state->types.continuation, cloverCount);
    }

    const_cast<ORef&>(ptr->method) = method; // Initing so `const_cast` and no write barrier
    const_cast<Fixnum&>(ptr->pc) = pc; // Initing so `const_cast`

    return HRef(ptr);
}

HRef<Knot> allocKnot(RT* state) {
    Knot* ptr = (Knot*)state->heap.tryAlloc(&*state->types.knot);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Knot*)state->heap.allocOrDie(&*state->types.knot);
    }

    return HRef(ptr);
}

HRef<InputFile> createInputFile(RT* state, UTF8InputFile&& file) {
    InputFile* ptr = static_cast<decltype(ptr)>(
        state->heap.tryAlloc(&*state->types.inputFile));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = static_cast<decltype(ptr)>(
            state->heap.allocOrDie(&*state->types.inputFile));
    }

    return HRef{new (ptr) InputFile{std::move(file)}};
}

HRef<UnboundError> createUnboundError(RT* state, HRef<Symbol> name) {
    UnboundError* ptr = (UnboundError*)state->heap.tryAlloc(&*state->types.unboundError);
    if (mustCollect(ptr)) {
        auto const nameG = state->pushRoot(&name);
        collect(state);
        ptr = (UnboundError*)state->heap.allocOrDie(&*state->types.unboundError);
    }

    return HRef{new (ptr) UnboundError{name}};
}

HRef<TypeError> createTypeError(RT* state, HRef<Type> type, ORef val) {
    TypeError* ptr = (TypeError*)state->heap.tryAlloc(&*state->types.typeError);
    if (mustCollect(ptr)) {
        auto const typeG = state->pushRoot(&type);
        auto const valG = state->pushRoot(&val);
        collect(state);
        ptr = (TypeError*)state->heap.allocOrDie(&*state->types.typeError);
    }

    return HRef{new (ptr) TypeError{type, val}};
}

HRef<ArityError> createArityError(RT* state, HRef<Closure> callee, Fixnum callArgc) {
    ArityError* ptr = (ArityError*)state->heap.tryAlloc(&*state->types.arityError);
    if (mustCollect(ptr)) {
        auto const calleeG = state->pushRoot(&callee);
        collect(state);
        ptr = (ArityError*)state->heap.allocOrDie(&*state->types.arityError);
    }

    return HRef{new (ptr) ArityError{callee, callArgc}};
}

HRef<InapplicableError> createInapplicableError(RT* state, HRef<Multimethod> callee) {
    InapplicableError* ptr =
        (InapplicableError*)state->heap.tryAlloc(&*state->types.inapplicableError);
    if (mustCollect(ptr)) {
        auto const calleeG = state->pushRoot(&callee);
        collect(state);
        ptr = (InapplicableError*)state->heap.allocOrDie(
            &*state->types.inapplicableError);
    }

    return HRef{new (ptr) InapplicableError{callee}};
}

HRef<FatalError> createOverflowError(
    RT* state, HRef<Closure> callee, Fixnum x, Fixnum y
) {
    Fixnum const count = Fixnum{3l};

    FatalError* ptr =
        (FatalError*)state->heap.tryAllocFlex(&*state->types.fatalError, count);
    if (mustCollect(ptr)) {
        auto const calleeG = state->pushRoot(&callee);
        collect(state);
        ptr = (FatalError*)state->heap.allocFlexOrDie(&*state->types.fatalError, count);
    }
    HRef<FatalError> res = HRef(ptr);

    auto const calleeG = state->pushRoot(&callee);
    auto const resG = state->pushRoot(&res);
    HRef<Symbol> const name = intern(state, strLit("overflow"));
    ptr = &*res; // Post-GC reload

    return HRef{new (ptr) FatalError{name, ORefSpan{{callee, x, y}}}};
}

HRef<FatalError> createDivByZeroError(
    RT* state, HRef<Closure> callee, Fixnum x, Fixnum y
) {
    Fixnum const count = Fixnum{3l};

    FatalError* ptr =
        (FatalError*)state->heap.tryAllocFlex(&*state->types.fatalError, count);
    if (mustCollect(ptr)) {
        auto const calleeG = state->pushRoot(&callee);
        collect(state);
        ptr = (FatalError*)state->heap.allocFlexOrDie(&*state->types.fatalError, count);
    }
    HRef<FatalError> res = HRef(ptr);

    auto const calleeG = state->pushRoot(&callee);
    auto const resG = state->pushRoot(&res);
    HRef<Symbol> const name = intern(state, strLit("divide-by-zero"));
    ptr = &*res; // Post-GC reload

    return HRef{new (ptr) FatalError{name, ORefSpan{{callee, x, y}}}};
}

} // namespace
