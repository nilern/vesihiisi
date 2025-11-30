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
    // OPTIMIZE: in bulk on GC and `tryCreateSemispace` instead (with `memset_explicit`)?:
    memset(ptr, 0, size); // Zero-init data
    
    return ptr;
}

static void* tryAllocFlex(Semispace* semispace, Type const* type, Fixnum length) {
    assert(semispaceIsValid(semispace));
    assert(unwrapBool(type->isFlex));

    uintptr_t address = (uintptr_t)(void*)semispace->free;
    
    address += sizeof(FlexHeader); // Reserve header
    // Align oref:
    uintptr_t const align = (uintptr_t)fixnumToInt(type->align);
    address = (address + align - 1) & ~(align - 1);
    
    // Check bound and commit reservation:
    uintptr_t len = (uintptr_t)fixnumToInt(length);
    uintptr_t const flexSize = unwrapBool(type->isBytes) ? len : len * sizeof(ORef);
    uintptr_t const size = (uintptr_t)fixnumToInt(type->minSize) + flexSize;
    char* const free = (char*)(void*)(address + size);
    if (free >= semispace->limit) { return nullptr; }
    semispace->free = free;
    
    char* const ptr = (char*)(void*)address;
    *((FlexHeader*)(void*)address - 1) = flexHeader(length, type); // Init header
    // OPTIMIZE: in bulk on GC and `tryCreateSemispace` instead (with `memset_explicit`)?:
    memset(ptr, 0, size); // Zero-init data
    
    return ptr;
}

