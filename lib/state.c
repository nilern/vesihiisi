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

#define REG_COUNT 128

typedef struct State {
    Heap heap;

    TypeRef typeType;
    TypeRef stringType;
    TypeRef arrayType;
    TypeRef byteArrayType;
    TypeRef symbolType;
    TypeRef pairType;
    TypeRef emptyListType;
    TypeRef methodType;
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
    
    uint8_t scratchCount;
    ORef regs[REG_COUNT];
} State;

// Returns handle
inline static ORef* pushTmp(State* state, ORef v) {
    assert(state->scratchCount < REG_COUNT);
    
    ORef* const handle = &state->regs[state->scratchCount];
    *handle = v;
    ++state->scratchCount;
    return handle;
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
    
    *dest = (State){
        .heap = heap,
        
        .typeType = tagType(typeTypePtr),
        .stringType = tagType(stringTypePtr),
        .arrayType = tagType(arrayTypePtr),
        .byteArrayType = tagType(byteArrayType),
        .symbolType = tagType(symbolTypePtr),
        .pairType = tagType(pairTypePtr),
        .emptyListType = tagType(emptyListTypePtr),
        .methodType = tagType(methodType),
        
        .fixnumType = tagType(fixnumType),
        .charType = tagType(charType),
        .flonumType = tagType(flonumType),
        .boolType = tagType(boolType),
        
        .symbols = symbols,
        .emptyList = tagEmptyList(emptyListPtr),
        
        .scratchCount = 0
    };
    return true;
}

inline static void freeState(State* state) { freeHeap(&state->heap); }

static TypeRef typeOf(State const* state, ORef v) {
    Tag const tag = getTag(v);
    return tag == TAG_HEAPED
        ? headerType(*((Header*)uncheckedORefToPtr(v) - 1))
        : uncheckedORefToTypeRef(state->immTypes[tag]);
}

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
    return isHeaped(v)
        && eq(typeToORef(typeOf(state, v)), typeToORef(state->emptyListType));
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
        return symbol;
    }
}

static PairRef allocPair(State* state) {
    Pair* const ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->pairType));
    if (!ptr) { assert(false); } // TODO: Collect garbage here
    
    return tagPair(ptr);
}

static MethodRef createMethod(State* state, ByteArrayRef code, ArrayRef consts) {
    Method* const ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->methodType));
    if (!ptr) { assert(false); } // TODO: Collect garbage here

    *ptr = (Method){.code = code, .consts = consts};

    return tagMethod(ptr);
}
