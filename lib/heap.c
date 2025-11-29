typedef struct Semispace {
    char* free;
    char* limit;
    char* start;
} Semispace;

typedef struct Heap {
    Semispace tospace;
    Semispace fromspace;
} Heap;

static Semispace tryCreateSemispace(size_t size) {
    char* const start = malloc(size);

    return (Semispace){
        .free = start,
        .limit = start + size,
        .start = start
    };
}

static Heap tryCreateHeap(size_t size) {
    size_t const semispaceSize = size / 2;

    return (Heap){
        .tospace = tryCreateSemispace(semispaceSize),
        .fromspace = tryCreateSemispace(semispaceSize),
    };
}

static bool semispaceIsValid(Semispace const* semispace) { return semispace->free != nullptr; }

static bool heapIsValid(Heap const* heap) {
    return semispaceIsValid(&heap->tospace)
        && semispaceIsValid(&heap->fromspace);
}

static void freeSemispace(Semispace* semispace) {
    free(semispace->start);
}

static void freeHeap(Heap* heap) {
    freeSemispace(&heap->fromspace);
    freeSemispace(&heap->tospace);
}

static void* tryAlloc(Semispace* semispace, Type const* type) {
    assert(semispaceIsValid(semispace));
    assert(!unwrapBool(type->isFlex));

    uintptr_t address = (uintptr_t)(void*)semispace->free;
    
    address += sizeof(Header); // Reserve header
    // Align oref:
    uintptr_t const align = (uintptr_t)fixnumToInt(type->align);
    address = (address + align - 1) & ~(align - 1);
    
    // Check bound and commit reservation:
    uintptr_t const size = (uintptr_t)fixnumToInt(type->minSize);
    char* const free = (char*)(void*)(address + size);
    if (free >= semispace->limit) { return nullptr; }
    semispace->free = free;
    
    char* const ptr = (char*)(void*)address;
    *((Header*)(void*)address - 1) = fixedHeader(type); // Init header
    memset(ptr, 0, size); // Zero-init data
    
    return ptr;
}

[[maybe_unused]] // FIXME
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

