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

static StringRef createString(Heap* heap, Type const* stringType, Str str) {
    char* const stringPtr = tryAllocFlex(&heap->tospace, stringType, tagInt((intptr_t)str.len));
    if (!stringPtr) { assert(false); } // TODO: Collect garbage here
    
    memcpy(stringPtr, str.data, str.len);
    
    return tagString(stringPtr);
}

static ArrayRef createArray(Heap* heap, Type const* arrayType, Fixnum count) {
    ORef* const ptr = tryAllocFlex(&heap->tospace, arrayType, count);
    if (!ptr) { assert(false); } // TODO: Collect garbage here
    
    return tagArray(ptr);
}

static SymbolRef createUninternedSymbol(Heap* heap, Type const* symbolType, Fixnum hash, Str name) {
    Symbol* const ptr = tryAllocFlex(&heap->tospace, symbolType, tagInt((intptr_t)name.len));
    if (!ptr) { assert(false); } // TODO: Collect garbage here
    
    ptr->hash = hash;
    memcpy(ptr->name, name.data, name.len);
    
    return tagSymbol(ptr);
}

typedef struct SymbolTable {
    ArrayRef entries;
    Fixnum count;
} SymbolTable;

static SymbolTable createSymbolTable(Heap* heap, Type const* arrayType) {
    Fixnum const count = tagInt(2);
    ArrayRef const entries = createArray(heap, arrayType, count);
    
    return (SymbolTable){entries, count};
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

static void rehashSymbols(Heap* heap, Type const* arrayType, SymbolTable* symbols) {
    Fixnum const oldCapRef = arrayCount(symbols->entries);
    size_t const oldCap = (uintptr_t)fixnumToInt(oldCapRef);
    size_t const newCap = oldCap << 1;
    ArrayRef const newEntries = createArray(heap, arrayType, tagInt((intptr_t)newCap));

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = arrayToPtr(symbols->entries)[i];
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
    
    symbols->entries = newEntries;
}

// `name` must not point into GC heap:
static SymbolRef intern(
    Heap* heap, Type const* arrayType, Type const* symbolType, SymbolTable* symbols, Str name
) {
    Fixnum const hash = hashStr(name);
    
    IndexOfSymbolRes ires = indexOfSymbol(symbols, hash, name);
    if (ires.exists) {
        return uncheckedORefToSymbol(arrayToPtr(symbols->entries)[ires.index]);;
    } else {
        size_t const newCount = (uintptr_t)fixnumToInt(symbols->count) + 1;
        size_t const capacity = (uintptr_t)fixnumToInt(arrayCount(symbols->entries));
        if (capacity >> 1 < newCount) {
            rehashSymbols(heap, arrayType, symbols);
            ires = indexOfSymbol(symbols, hash, name);
        }
        
        SymbolRef const symbol = createUninternedSymbol(heap, symbolType, hash, name);
        arrayToPtr(symbols->entries)[ires.index] = symbolToORef(symbol);
        return symbol;
    }
}

