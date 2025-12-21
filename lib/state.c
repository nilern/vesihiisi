typedef struct SymbolTable {
    ORef* entries;
    size_t count;
    size_t cap;
} SymbolTable;

static ORef const SymbolTableTombstone = {((uintptr_t)false << tag_width) | (uintptr_t)TAG_BOOL};

inline static void freeSymbols(SymbolTable* symbols) { free(symbols->entries); }

static SymbolTable newSymbolTable(void) {
    size_t const cap = 2;
    ORef* const entries = calloc(cap, sizeof *entries);
    return (SymbolTable){.entries = entries, .count = 0, .cap = cap};
}

static void pruneSymbols(SymbolTable* symbols);

static bool tryCreateNamespace(
    Semispace* semispace, NamespaceRef* dest, Type const* nsType, Type const* arrayType
) {
    Fixnum const count = tagInt(2);
    ORef* const keys = tryAllocFlex(semispace, arrayType, count);
    if (!keys) { return false; }
    ORef* const vals = tryAllocFlex(semispace, arrayType, count);
    if (!vals) { return false; }
    Namespace* const ptr = tryAlloc(semispace, nsType);
    if (!ptr) { return false; }

    *ptr = (Namespace){
        .keys = tagArray(keys),
        .vals = tagArray(vals),
        .count = count
    };

    *dest = tagNamespace(ptr);
    return true;
}

typedef struct Shadowstack {
    ORef** vals;
    size_t count;
    size_t cap;
} Shadowstack;

inline static void freeShadowstack(Shadowstack* shadowstack) {
    free(shadowstack->vals);
}

static Shadowstack newShadowstack(void) {
    size_t const cap = 2;
    ORef** const vals = malloc(cap * sizeof *vals);
    return (Shadowstack){.vals = vals, .count = 0, .cap = cap};
}

#define REG_COUNT 256

#define BOOTSTRAP_TYPE_COUNT 22
#define BOOTSTRAP_SINGLETON_COUNT 3

typedef struct State {
    ORef method;
    uint8_t const* code;
    size_t pc;
    ORef regs[REG_COUNT];
    ORef const* consts;
    NamespaceRef ns;
    size_t entryRegc;

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

static void freeState(State* state) {
    freeHeap(&state->heap);
    freeSymbols(&state->symbols);
    freeShadowstack(&state->shadowstack);
}

static void pushStackRoot(State* state, ORef* stackLoc) {
    if (state->shadowstack.count == state->shadowstack.cap) {
        size_t const newCap = state->shadowstack.cap + state->shadowstack.cap / 2;
        state->shadowstack.vals =
            realloc(state->shadowstack.vals, newCap * sizeof * state->shadowstack.vals);
        state->shadowstack.cap = newCap;
    }

    state->shadowstack.vals[state->shadowstack.count++] = stackLoc;
}

inline static void popStackRoots(State* state, size_t count) { state->shadowstack.count -= count; }

static void markRoots(State* state) {
    state->method = mark(&state->heap, state->method);

    for (size_t i = 0; i < REG_COUNT; ++i) {
        state->regs[i] = mark(&state->heap, state->regs[i]);
    }

    state->ns = uncheckedORefToNamespace(mark(&state->heap, namespaceToORef(state->ns)));

    for (size_t i = 0; i < BOOTSTRAP_TYPE_COUNT; ++i) {
        state->types[i] = mark(&state->heap, state->types[i]);
    }

    for (size_t i = 0; i < BOOTSTRAP_SINGLETON_COUNT; ++i) {
        state->singletons[i] = mark(&state->heap, state->singletons[i]);
    }

    state->errorHandler = uncheckedORefToVar(mark(&state->heap, varToORef(state->errorHandler)));

    {
        size_t const stackRootCount = state->shadowstack.count;
        for (size_t i = 0; i < stackRootCount; ++i) {
            *state->shadowstack.vals[i] = mark(&state->heap, *state->shadowstack.vals[i]);
        }
    }
}

inline static void updateWeakRefs(State* state) { pruneSymbols(&state->symbols); }

static void initSpecialPurposeRegs(State* state) {
    ORef const anyMethod = state->method;
    if (isHeaped(anyMethod)) {
        Method* const methodPtr = methodToPtr(uncheckedORefToMethod(anyMethod));
        state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
        state->consts = arrayToPtr(uncheckedORefToArray(methodPtr->consts));
    }
}

static Type* tryCreateTypeType(Semispace* semispace) {
    Type const bootstrapTypeType = {
        .minSize = tagInt((intptr_t)sizeof(Type)),
        .align = tagInt((intptr_t)alignof(Type)),
        .isBytes = False,
        .isFlex = False
    };
    
    void* const maybeTypeType = tryAlloc(semispace, &bootstrapTypeType);
    if (!maybeTypeType) { return nullptr; }
    
    Type* const typeType = (Type*)maybeTypeType;
    *((Header*)maybeTypeType - 1) = fixedHeader(typeType); // Init header, closing loop
    *typeType = bootstrapTypeType; // Init data
    
    return typeType;
}

static Type* tryCreateStringType(Semispace* semispace, Type const* typeType) {
    void* const maybeStringType = tryAlloc(semispace, typeType);
    if (!maybeStringType) { return nullptr; }
    
    Type* const stringType = (Type*)maybeStringType;
    *stringType = (Type){
        .minSize = tagInt(0),
        .align = tagInt((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = True
    };
    
    return stringType;
}

static Type* tryCreateSymbolType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt((intptr_t)sizeof(Symbol)),
        .align = tagInt((intptr_t)alignof(Symbol)),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = True
    };
    
    return type;
}

static Type* tryCreateArrayType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(0),
        .align = tagInt((intptr_t)alignof(ORef)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = True
    };
    
    return type;
}

static Type* tryCreateByteArrayType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(0),
        .align = tagInt((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = True
    };

    return type;
}

static Type* tryCreatePairType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(sizeof(Pair)),
        .align = tagInt(alignof(Pair)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False
    };
    
    return type;
}

static Type* tryCreateEmptyListType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(0),
        .align = tagInt((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False
    };
    
    return type;
}

static Type* tryCreateUnboundType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(0),
        .align = tagInt((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False
    };

    return type;
}

static Type* tryCreateMethodType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(sizeof(Method)),
        .align = tagInt(alignof(Method)),
        .isBytes = False,
        .hasCodePtr = True,
        .isFlex = False
    };

    return type;
}

static Type* tryCreateClosureType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(sizeof(Closure)),
        .align = tagInt(alignof(Closure)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = True
    };

    return type;
}

static Type* tryCreateContinuationType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(sizeof(Continuation)),
        .align = tagInt(alignof(Continuation)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = True
    };

    return type;
}

static Type* tryCreateVarType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(sizeof(Var)),
        .align = tagInt(alignof(Var)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False
    };

    return type;
}

static Type* tryCreateNamespaceType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(sizeof(Namespace)),
        .align = tagInt(alignof(Namespace)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False
    };

    return type;
}

static Type* tryCreateTypeErrorType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }

    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(sizeof(TypeError)),
        .align = tagInt(alignof(TypeError)),
        .isBytes = False,
        .hasCodePtr = False,
        .isFlex = False
    };

    return type;
}

static Type* tryCreateImmType(Semispace* semispace, Type const* typeType) {
    void* const maybeType = tryAlloc(semispace, typeType);
    if (!maybeType) { return nullptr; }
    
    Type* const type = (Type*)maybeType;
    *type = (Type){
        .minSize = tagInt(0),
        .align = tagInt((intptr_t)objectMinAlign),
        .isBytes = True,
        .hasCodePtr = False,
        .isFlex = False
    };
    
    return type;
}

inline static Type* tryCreateFixnumType(Semispace* semispace, Type const* typeType) {
    return tryCreateImmType(semispace, typeType);
}

inline static Type* tryCreateFlonumType(Semispace* semispace, Type const* typeType) {
    return tryCreateImmType(semispace, typeType);
}

inline static Type* tryCreateCharType(Semispace* semispace, Type const* typeType) {
    return tryCreateImmType(semispace, typeType);
}

inline static Type* tryCreateBoolType(Semispace* semispace, Type const* typeType) {
    return tryCreateImmType(semispace, typeType);
}

static PrimopRes callBytecode(State* state);
static PrimopRes primopAbort(State* state);
static PrimopRes primopIdentical(State* state);
static PrimopRes primopFxAdd(State* state);
static PrimopRes primopFxSub(State* state);
static PrimopRes primopFxMul(State* state);
static PrimopRes primopFxDiv(State* state);

static MethodRef createPrimopMethod(State* state, MethodCode nativeCode);

static ClosureRef allocClosure(State* state, MethodRef method, Fixnum cloverCount);

static SymbolRef intern(State* state, Str name);

static VarRef getVar(State* state, NamespaceRef nsRef, SymbolRef name);

static void installPrimopClosure(State* state, Str name, ClosureRef closure) {
    pushStackRoot(state, (ORef*)&closure);

    SymbolRef const symbol = intern(state, name);
    VarRef const var = getVar(state, state->ns, symbol);

    varToPtr(var)->val = closureToORef(closure);

    popStackRoots(state, 1);
}

static void installPrimop(State* state, Str name, MethodCode nativeCode) {
    MethodRef const method = createPrimopMethod(state, nativeCode);
    ClosureRef const closure = allocClosure(state, method, Zero);
    installPrimopClosure(state, name, closure);
}

static Var* tryCreateUnboundVar(Semispace* semispace, Type const* unboundType, UnboundRef unbound);

static bool tryCreateState(State* dest, size_t heapSize) {
    Heap heap = tryCreateHeap(heapSize);
    if (!heapIsValid(&heap)) { return false; }
    
    Type const* const typeTypePtr = tryCreateTypeType(&heap.tospace);
    if (!typeTypePtr) { return false; }
    Type const* const stringTypePtr = tryCreateStringType(&heap.tospace, typeTypePtr);
    if (!stringTypePtr) { return false; }
    Type const* const arrayTypePtr = tryCreateArrayType(&heap.tospace, typeTypePtr);
    if (!arrayTypePtr) { return false; }
    Type const* const byteArrayType = tryCreateByteArrayType(&heap.tospace, typeTypePtr);
    if (!byteArrayType) { return false; }
    Type const* const symbolTypePtr = tryCreateSymbolType(&heap.tospace, typeTypePtr);
    if (!symbolTypePtr) { return false; }
    Type const* const pairTypePtr = tryCreatePairType(&heap.tospace, typeTypePtr);
    if (!pairTypePtr) { return false; }
    Type const* const emptyListTypePtr = tryCreateEmptyListType(&heap.tospace, typeTypePtr);
    if (!emptyListTypePtr) { return false; }
    Type const* const methodType = tryCreateMethodType(&heap.tospace, typeTypePtr);
    if (!methodType) { return false; }
    Type const* const closureType = tryCreateClosureType(&heap.tospace, typeTypePtr);
    if (!closureType) { return false; }
    Type const* const continuationType = tryCreateContinuationType(&heap.tospace, typeTypePtr);
    if (!continuationType) { return false; }
    Type const* const unboundType = tryCreateUnboundType(&heap.tospace, typeTypePtr);
    if (!unboundType) { return false; }
    Type const* const varType = tryCreateVarType(&heap.tospace, typeTypePtr);
    if (!varType) { return false; }
    Type const* const nsType = tryCreateNamespaceType(&heap.tospace, typeTypePtr);
    if (!nsType) { return false; }
    Type const* const typeErrorType = tryCreateTypeErrorType(&heap.tospace, typeTypePtr);
    if (!nsType) { return false; }
    
    Type const* const fixnumType = tryCreateFixnumType(&heap.tospace, typeTypePtr);
    if (!fixnumType) { return false; }
    Type const* const charType = tryCreateCharType(&heap.tospace, typeTypePtr);
    if (!charType) { return false; }
    Type const* const flonumType = tryCreateFlonumType(&heap.tospace, typeTypePtr);
    if (!flonumType) { return false; }
    Type const* const boolType = tryCreateBoolType(&heap.tospace, typeTypePtr);
    if (!boolType) { return false; }
    
    void const* const emptyListPtr = tryAlloc(&heap.tospace, emptyListTypePtr);
    if (!emptyListPtr) { return false; }
    void const* const unbound = tryAlloc(&heap.tospace, unboundType);
    if (!unbound) { return false; }
    void* const exitPtr = tryAllocFlex(&heap.tospace, continuationType, Zero);
    if (!exitPtr) { return false; }

    Var* const errorHandler = tryCreateUnboundVar(&heap.tospace, varType, tagUnbound(unbound));
    if (!errorHandler) { return false; }

    NamespaceRef ns;
    if (!tryCreateNamespace(&heap.tospace, &ns, nsType, arrayTypePtr)) { return false; }

    *dest = (State){
        .method = fixnumToORef(Zero),
        .code = nullptr,
        .pc = 0,
        .consts = nullptr,
        .ns = ns,
        .entryRegc = 0, // Intentionally invalid

        .heap = heap,
        
        .typeType = tagType(typeTypePtr),
        .stringType = tagType(stringTypePtr),
        .arrayType = tagType(arrayTypePtr),
        .byteArrayType = tagType(byteArrayType),
        .symbolType = tagType(symbolTypePtr),
        .pairType = tagType(pairTypePtr),
        .emptyListType = tagType(emptyListTypePtr),
        .methodType = tagType(methodType),
        .closureType = tagType(closureType),
        .continuationType = tagType(continuationType),
        .unboundType = tagType(unboundType),
        .varType = tagType(varType),
        .nsType = tagType(nsType),
        .typeErrorType = tagType(typeErrorType),
        
        .fixnumType = tagType(fixnumType),
        .charType = tagType(charType),
        .flonumType = tagType(flonumType),
        .boolType = tagType(boolType),
        
        .symbols = newSymbolTable(),
        .emptyList = tagEmptyList(emptyListPtr),
        .unbound = tagUnbound(unbound),
        .exit = tagClosure(exitPtr),

        .errorHandler = tagVar(errorHandler),

        .shadowstack = newShadowstack()
    };

    MethodRef const abortMethod = createPrimopMethod(dest, primopAbort);
    ClosureRef const abortClosure = allocClosure(dest, abortMethod, Zero);
    varToPtr(dest->errorHandler)->val = closureToORef(abortClosure);

    installPrimopClosure(dest, (Str){"abort", /*FIXME:*/ 5}, abortClosure);
    installPrimop(dest, (Str){"identical?", /*FIXME:*/ 10}, primopIdentical);
    installPrimop(dest, (Str){"fx+", /*FIXME:*/ 3}, primopFxAdd);
    installPrimop(dest, (Str){"fx-", /*FIXME:*/ 3}, primopFxSub);
    installPrimop(dest, (Str){"fx*", /*FIXME:*/ 3}, primopFxMul);
    installPrimop(dest, (Str){"fx-quot", /*FIXME:*/ 7}, primopFxDiv);

    return true;
}

static TypeRef typeOf(State const* state, ORef v) {
    Tag const tag = getTag(v);
    return tag == TAG_HEAPED
        ? headerType(*((Header*)uncheckedORefToPtr(v) - 1))
        : uncheckedORefToTypeRef(state->types[tag]);
}

// OPTMIZE: If we already know that `isHeaped(v)`, the calls `typeOf` recheck that redundantly:

inline static bool isString(State const* state, ORef v) {
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->stringType));
}

inline static bool isSymbol(State const* state, ORef v) {
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->symbolType));
}

inline static bool isPair(State const* state, ORef v) {
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->pairType));
}

inline static bool isEmptyList(State const* state, ORef v) {
    return eq(v, emptyListToORef(state->emptyList));
}

inline static bool isMethod(State const* state, ORef v) {
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->methodType));
}

inline static bool isClosure(State const* state, ORef v) {
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->closureType));
}

inline static bool isContinuation(State const* state, ORef v) {
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->continuationType));
}

inline static bool isType(State const* state, ORef v) {
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->typeType));
}

inline static bool isTypeError(State const* state, ORef v) {
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->typeErrorType));
}

[[maybe_unused]]
static void assertStateInTospace(State const* state) {
    if (isHeaped(state->method)) {
        assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(state->method)));
        assert(allocatedInSemispace(&state->heap.tospace, state->code));
        assert(allocatedInSemispace(&state->heap.tospace, state->consts));
    }


    for (size_t i = 0; i < REG_COUNT; ++i) {
        ORef const reg = state->regs[i];
        if (isHeaped(reg)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(reg)));
        }
    }

    assert(allocatedInSemispace(&state->heap.tospace, namespaceToPtr(state->ns)));

    for (size_t i = 0; i < BOOTSTRAP_TYPE_COUNT; ++i) {
        ORef const v = state->types[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    for (size_t i = 0; i < state->symbols.cap; ++i) {
        ORef const v = state->symbols.entries[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    for (size_t i = 0; i < BOOTSTRAP_SINGLETON_COUNT; ++i) {
        ORef const v = state->singletons[i];
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }

    assert(allocatedInSemispace(&state->heap.tospace, varToPtr(state->errorHandler)));

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

static void defaultPrepCollection(State* state) {
    flipSemispaces(&state->heap);
    markRoots(state);
}

static void completeCollection(State* state) {
    collectHeap(&state->heap);

    updateWeakRefs(state);

    refurbishSemispace(&state->heap.fromspace, &state->heap.tospace);
    initSpecialPurposeRegs(state);

#ifndef NDEBUG
    assertStateInTospace(state);
#endif
}

static void collect(State* state) {
    defaultPrepCollection(state);

    completeCollection(state);
}

struct IRFn;
static void markIRFn(State* state, struct IRFn* fn);
static void assertIRFnInTospace(State const* state, struct IRFn const* fn);

static void collectTracingIR(State* state, struct IRFn* fn) {
    defaultPrepCollection(state);
    markIRFn(state, fn);

    completeCollection(state);

#ifndef NDEBUG
    assertIRFnInTospace(state, fn);
#endif
}

static StringRef createString(State* state, Str str) {
    char* stringPtr =
        tryAllocFlex(&state->heap.tospace, typeToPtr(state->stringType), tagInt((intptr_t)str.len));
    if (mustCollect(stringPtr)) {
        collect(state);
        stringPtr = allocFlexOrDie(&state->heap.tospace, typeToPtr(state->stringType),
                                tagInt((intptr_t)str.len));
    }
    
    memcpy(stringPtr, str.data, str.len);
    
    return tagString(stringPtr);
}

inline static ORef* tryAllocArray(State* state, Fixnum count) {
    return (ORef*)tryAllocFlex(&state->heap.tospace, typeToPtr(state->arrayType), count);
}

inline static ORef* allocArrayOrDie(State* state, Fixnum count) {
    return (ORef*)allocFlexOrDie(&state->heap.tospace, typeToPtr(state->arrayType), count);
}

static ArrayRef createArray(State* state, Fixnum count) {
    ORef* ptr = tryAllocArray(state, count);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocArrayOrDie(state, count);
    }
    
    return tagArray(ptr);
}

inline static uint8_t* tryAllocByteArray(State* state, Fixnum count) {
    return (uint8_t*)tryAllocFlex(&state->heap.tospace, typeToPtr(state->byteArrayType), count);
}

inline static uint8_t* allocByteArrayOrDie(State* state, Fixnum count) {
    return (uint8_t*)allocFlexOrDie(&state->heap.tospace, typeToPtr(state->byteArrayType), count);
}

static SymbolRef createUninternedSymbol(State* state, Fixnum hash, Str name) {
    Symbol* ptr = tryAllocFlex(
        &state->heap.tospace, typeToPtr(state->symbolType), tagInt((intptr_t)name.len));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocFlexOrDie(
            &state->heap.tospace, typeToPtr(state->symbolType), tagInt((intptr_t)name.len));
    }

    ptr->hash = hash;
    memcpy(ptr->name, name.data, name.len);
    
    return tagSymbol(ptr);
}

inline static Fixnum hashStr(Str s) { return tagInt((intptr_t)fnv1aHash(s)); }

typedef struct IndexOfSymbolRes {
    size_t index;
    bool exists;
} IndexOfSymbolRes;

static IndexOfSymbolRes indexOfSymbol(SymbolTable const* symbols, Fixnum hash, Str name) {
    uintptr_t const h = (uintptr_t)fixnumToInt(hash);
    
    size_t const maxIndex = symbols->cap - 1;
    for (size_t collisions = 0, i = h & maxIndex;; ++collisions, i = (i + collisions) & maxIndex) {
        ORef* const entry = symbols->entries + i;
        
        if (eq(*entry, fixnumToORef(Zero))) { return (IndexOfSymbolRes){i, false}; }

        if (isHeaped(*entry)) {
            SymbolRef const symbol = uncheckedORefToSymbol(*entry);
            Symbol const* const symbolPtr = symbolToPtr(symbol);
            if (eq(fixnumToORef(symbolPtr->hash), fixnumToORef(hash))
                && strEq(symbolName(symbol), name)
            ) {
                return (IndexOfSymbolRes){i, true};
            }
        }
    }
}

// OPTMIZE: Do not grow if load factor is largely due to tombstones:
static void rehashSymbols(State* state) {
    size_t const oldCap = state->symbols.cap;
    size_t const newCap = oldCap * 2;
    ORef* const newEntries = calloc(newCap, sizeof *newEntries);
    size_t newCount = 0;

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = state->symbols.entries[i];
        if (isHeaped(v)) {
            size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(uncheckedORefToSymbol(v))->hash);
        
            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const entry = newEntries + j;
                if (eq(*entry, fixnumToORef(Zero))) {
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
static SymbolRef intern(State* state, Str name) {
    Fixnum const hash = hashStr(name);
    
    IndexOfSymbolRes ires = indexOfSymbol(&state->symbols, hash, name);
    if (ires.exists) {
        return uncheckedORefToSymbol(state->symbols.entries[ires.index]);;
    } else {
        size_t const newCount = state->symbols.count + 1;
        size_t const capacity = state->symbols.cap;
        if (capacity / 2 < newCount) {
            rehashSymbols(state);
            ires = indexOfSymbol(&state->symbols, hash, name);
        }
        
        SymbolRef const symbol = createUninternedSymbol(state, hash, name);
        state->symbols.entries[ires.index] = symbolToORef(symbol);
        state->symbols.count = newCount;
        return symbol;
    }
}

static void pruneSymbols(SymbolTable* symbols) {
    size_t const cap = symbols->cap;
    for (size_t i = 0; i < cap; ++i) {
        ORef* const v = &symbols->entries[i];
        if (isHeaped(*v)) {
            void* const fwdPtr = tryForwarded(uncheckedORefToPtr(*v));
            *v = fwdPtr ? tagHeaped(fwdPtr) : SymbolTableTombstone;
        }
    }
}

static PairRef allocPair(State* state) {
    Pair* ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->pairType));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocOrDie(&state->heap.tospace, typeToPtr(state->pairType));
    }
    
    return tagPair(ptr);
}

static Method* tryCreateBytecodeMethod(State* state, ByteArrayRef code, ArrayRef consts) {
    Method* ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->methodType));
    if (!ptr) { return ptr; }

    *ptr = (Method){
        .nativeCode = callBytecode,
        .code = byteArrayToORef(code),
        .consts = arrayToORef(consts)
    };

    return ptr;
}

static Method* createBytecodeMethodOrDie(State* state, ByteArrayRef code, ArrayRef consts) {
    Method* ptr = allocOrDie(&state->heap.tospace, typeToPtr(state->methodType));

    *ptr = (Method){
        .nativeCode = callBytecode,
        .code = byteArrayToORef(code),
        .consts = arrayToORef(consts)
    };

    return ptr;
}

static MethodRef createPrimopMethod(State* state, MethodCode nativeCode) {
    Method* ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->methodType));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocOrDie(&state->heap.tospace, typeToPtr(state->methodType));
    }

    *ptr = (Method){
        .nativeCode = nativeCode,
        .code = fixnumToORef(Zero),
        .consts = fixnumToORef(Zero)
    };

    return tagMethod(ptr);
}

static ClosureRef allocClosure(State* state, MethodRef method, Fixnum cloverCount) {
    Closure* ptr = tryAllocFlex(&state->heap.tospace, typeToPtr(state->closureType), cloverCount);
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&method);
        collect(state);
        popStackRoots(state, 1);
        ptr = allocFlexOrDie(&state->heap.tospace, typeToPtr(state->closureType), cloverCount);
    }

    ptr->method = methodToORef(method);

    return tagClosure(ptr);
}

static ContinuationRef allocContinuation(
    State* state, MethodRef method, Fixnum pc, Fixnum cloverCount
) {
    Continuation* ptr =
        tryAllocFlex(&state->heap.tospace, typeToPtr(state->continuationType), cloverCount);
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&method);
        collect(state);
        popStackRoots(state, 1);
        ptr = allocFlexOrDie(&state->heap.tospace, typeToPtr(state->continuationType), cloverCount);
    }

    ptr->method = methodToORef(method);
    ptr->pc = pc;

    return tagContinuation(ptr);
}

static TypeErrorRef createTypeError(State* state, TypeRef type, ORef val) {
    TypeError* ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->typeErrorType));
    if (mustCollect(ptr)) {
        pushStackRoot(state, (ORef*)&type);
        pushStackRoot(state, &val);
        collect(state);
        popStackRoots(state, 2);
        ptr = allocOrDie(&state->heap.tospace, typeToPtr(state->typeErrorType));
    }

    *ptr = (TypeError){.type = type, .val = val};

    return tagTypeError(ptr);
}
