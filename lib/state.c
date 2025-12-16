typedef struct SymbolTable {
    ArrayRef entries;
    Fixnum count;
} SymbolTable;

static bool tryCreateSymbolTable(SymbolTable* dest, Heap* heap, Type const* arrayType) {
    Fixnum const count = tagInt(2);
    ORef* const entries = tryAllocFlex(&heap->tospace, arrayType, count);
    if (!entries) { return false; }
    
    *dest = (SymbolTable){tagArray(entries), count};
    return true;
}

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

#define REG_COUNT 256

typedef struct State {
    ORef method;
    uint8_t const* code;
    size_t pc;
    ORef regs[REG_COUNT];
    ORef const* consts;
    NamespaceRef ns;
    uint8_t scratchCount;

    Heap heap;

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
        };
        ORef immTypes[8];
    };
    
    SymbolTable symbols;
    EmptyListRef emptyList;
    UnboundRef unbound;
    ClosureRef exit;
} State;

// Returns handle
inline static ORef* pushTmp(State* state, ORef v) {
    assert(state->scratchCount < REG_COUNT);
    
    ORef* const handle = &state->regs[state->scratchCount];
    *handle = v;
    ++state->scratchCount;
    return handle;
}

inline static void popTmps(State* state, uint8_t count) {
    assert(state->scratchCount >= count);

    state->scratchCount -= count;
}

inline static ORef popTmp(State* state) {
    assert(state->scratchCount > 0);

    return state->regs[--state->scratchCount];
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
static PrimopRes primopIdentical(State* state);
static PrimopRes primopFxAdd(State* state);
static PrimopRes primopFxSub(State* state);
static PrimopRes primopFxMul(State* state);

static MethodRef createPrimopMethod(State* state, MethodCode nativeCode);

static ClosureRef allocClosure(State* state, MethodRef method, Fixnum cloverCount);

static SymbolRef intern(State* state, Str name);

static VarRef getVar(State* state, NamespaceRef nsRef, SymbolRef name);

static void installPrimop(State* state, Str name, MethodCode nativeCode) {
    MethodRef* const method =
        (MethodRef*)pushTmp(state, methodToORef(createPrimopMethod(state, nativeCode)));
    ClosureRef* const closure =
        (ClosureRef*)pushTmp(state, closureToORef(allocClosure(state, *method, Zero)));
    SymbolRef* const symbol = (SymbolRef*)pushTmp(state, symbolToORef(intern(state, name)));
    VarRef const var = getVar(state, state->ns, *symbol);
    varToPtr(var)->val = closureToORef(*closure);
    popTmps(state, 3);
}

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
    
    Type const* const fixnumType = tryCreateFixnumType(&heap.tospace, typeTypePtr);
    if (!fixnumType) { return false; }
    Type const* const charType = tryCreateCharType(&heap.tospace, typeTypePtr);
    if (!charType) { return false; }
    Type const* const flonumType = tryCreateFlonumType(&heap.tospace, typeTypePtr);
    if (!flonumType) { return false; }
    Type const* const boolType = tryCreateBoolType(&heap.tospace, typeTypePtr);
    if (!boolType) { return false; }
    
    SymbolTable symbols;
    if (!tryCreateSymbolTable(&symbols, &heap, arrayTypePtr)) { return false; }
    void const* const emptyListPtr = tryAlloc(&heap.tospace, emptyListTypePtr);
    if (!emptyListPtr) { return false; }
    void const* const unbound = tryAlloc(&heap.tospace, unboundType);
    if (!unbound) { return false; }
    void* const exitPtr = tryAllocFlex(&heap.tospace, continuationType, Zero);
    if (!exitPtr) { return false; }

    NamespaceRef ns;
    if (!tryCreateNamespace(&heap.tospace, &ns, nsType, arrayTypePtr)) { return false; }

    *dest = (State){
        .method = fixnumToORef(Zero),
        .code = nullptr,
        .pc = 0,
        .consts = nullptr,
        .ns = ns,
        .scratchCount = 128, // FIXME: Reserves 128 regs for VM, should use register windows instead

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
        
        .fixnumType = tagType(fixnumType),
        .charType = tagType(charType),
        .flonumType = tagType(flonumType),
        .boolType = tagType(boolType),
        
        .symbols = symbols,
        .emptyList = tagEmptyList(emptyListPtr),
        .unbound = tagUnbound(unbound),
        .exit = tagClosure(exitPtr)
    };

    installPrimop(dest, (Str){"identical?", /*FIXME:*/ 10}, primopIdentical);
    installPrimop(dest, (Str){"fx+", /*FIXME:*/ 3}, primopFxAdd);
    installPrimop(dest, (Str){"fx-", /*FIXME:*/ 3}, primopFxSub);
    installPrimop(dest, (Str){"fx*", /*FIXME:*/ 3}, primopFxMul);

    return true;
}

inline static void freeState(State* state) { freeHeap(&state->heap); }

static TypeRef typeOf(State const* state, ORef v) {
    Tag const tag = getTag(v);
    return tag == TAG_HEAPED
        ? headerType(*((Header*)uncheckedORefToPtr(v) - 1))
        : uncheckedORefToTypeRef(state->immTypes[tag]);
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

static StringRef createString(State* state, Str str) {
    char* const stringPtr =
        tryAllocFlex(&state->heap.tospace, typeToPtr(state->stringType), tagInt((intptr_t)str.len));
    if (!stringPtr) { assert(false); } // TODO: Collect garbage here
    
    memcpy(stringPtr, str.data, str.len);
    
    return tagString(stringPtr);
}

static ArrayRef createArray(State* state, Fixnum count) {
    ORef* const ptr = tryAllocFlex(&state->heap.tospace, typeToPtr(state->arrayType), count);
    if (!ptr) { assert(false); } // TODO: Collect garbage here
    
    return tagArray(ptr);
}

static ByteArrayRef createByteArray(State* state, Fixnum count) {
    uint8_t* const ptr = tryAllocFlex(&state->heap.tospace, typeToPtr(state->byteArrayType), count);
    if (!ptr) { assert(false); } // TODO: Collect garbage here

    return tagByteArray(ptr);
}

static SymbolRef createUninternedSymbol(State* state, Fixnum hash, Str name) {
    Symbol* const ptr = tryAllocFlex(
        &state->heap.tospace, typeToPtr(state->symbolType), tagInt((intptr_t)name.len));
    if (!ptr) { assert(false); } // TODO: Collect garbage here
    
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
    
    size_t const maxIndex = (uintptr_t)fixnumToInt(arrayCount(symbols->entries)) - 1;
    for (size_t collisions = 0, i = h & maxIndex;; ++collisions, i = (i + collisions) & maxIndex) {
        ORef* const entry = arrayToPtr(symbols->entries) + i;
        
        if (eq(*entry, fixnumToORef(Zero))) { return (IndexOfSymbolRes){i, false}; }
        SymbolRef const symbol = uncheckedORefToSymbol(*entry);
        
        Symbol const* const symbolPtr = symbolToPtr(symbol);
        if (eq(fixnumToORef(symbolPtr->hash), fixnumToORef(hash))
            && strEq(symbolName(symbol), name)
        ) {
            return (IndexOfSymbolRes){i, true};
        }
    }
}

static void rehashSymbols(State* state) {
    Fixnum const oldCapRef = arrayCount(state->symbols.entries);
    size_t const oldCap = (uintptr_t)fixnumToInt(oldCapRef);
    size_t const newCap = oldCap << 1;
    ArrayRef const newEntries = createArray(state, tagInt((intptr_t)newCap));

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = arrayToPtr(state->symbols.entries)[i];
        if (!eq(v, fixnumToORef(Zero))) {
            size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(uncheckedORefToSymbol(v))->hash);
        
            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const entry = arrayToPtr(newEntries) + j;
                if (eq(*entry, fixnumToORef(Zero))) {
                    *entry = v;
                    break;
                }
            }
        }
    }
    
    state->symbols.entries = newEntries;
}

// `name` must not point into GC heap:
static SymbolRef intern(State* state, Str name) {
    Fixnum const hash = hashStr(name);
    
    IndexOfSymbolRes ires = indexOfSymbol(&state->symbols, hash, name);
    if (ires.exists) {
        return uncheckedORefToSymbol(arrayToPtr(state->symbols.entries)[ires.index]);;
    } else {
        size_t const newCount = (uintptr_t)fixnumToInt(state->symbols.count) + 1;
        size_t const capacity = (uintptr_t)fixnumToInt(arrayCount(state->symbols.entries));
        if (capacity >> 1 < newCount) {
            rehashSymbols(state);
            ires = indexOfSymbol(&state->symbols, hash, name);
        }
        
        SymbolRef const symbol = createUninternedSymbol(state, hash, name);
        arrayToPtr(state->symbols.entries)[ires.index] = symbolToORef(symbol);
        state->symbols.count = tagInt((intptr_t)newCount);
        return symbol;
    }
}

static PairRef allocPair(State* state) {
    Pair* const ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->pairType));
    if (!ptr) { assert(false); } // TODO: Collect garbage here
    
    return tagPair(ptr);
}

static MethodRef createBytecodeMethod(State* state, ByteArrayRef code, ArrayRef consts) {
    Method* const ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->methodType));
    if (!ptr) { assert(false); } // TODO: Collect garbage here

    *ptr = (Method){
        .nativeCode = callBytecode,
        .code = byteArrayToORef(code),
        .consts = arrayToORef(consts)
    };

    return tagMethod(ptr);
}

static MethodRef createPrimopMethod(State* state, MethodCode nativeCode) {
    Method* const ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->methodType));
    if (!ptr) { assert(false); } // TODO: Collect garbage here

    *ptr = (Method){
        .nativeCode = nativeCode,
        .code = fixnumToORef(Zero),
        .consts = fixnumToORef(Zero)
    };

    return tagMethod(ptr);
}

static ClosureRef allocClosure(State* state, MethodRef method, Fixnum cloverCount) {
    Closure* const ptr =
        tryAllocFlex(&state->heap.tospace, typeToPtr(state->closureType), cloverCount);
    if (!ptr) { assert(false); } // TODO: Collect garbage here

    ptr->method = methodToORef(method);

    return tagClosure(ptr);
}

static ContinuationRef allocContinuation(
    State* state, MethodRef method, Fixnum pc, Fixnum cloverCount
) {
    Continuation* const ptr =
        tryAllocFlex(&state->heap.tospace, typeToPtr(state->continuationType), cloverCount);
    if (!ptr) { assert(false); } // TODO: Collect garbage here

    ptr->method = methodToORef(method);
    ptr->pc = pc;

    return tagContinuation(ptr);
}
