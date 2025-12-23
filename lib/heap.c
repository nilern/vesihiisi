#include "heap.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static Semispace tryCreateSemispace(size_t size) {
    char* const start = calloc(size, sizeof *start);

    return (Semispace){
        .free = start,
        .limit = start + size,
        .start = start
    };
}

inline static size_t semispaceSize(Semispace const* space) {
    return (size_t)(space->limit - space->start);
}

inline static bool semispaceContains(Semispace const* space, void const* ptr) {
    return space->start <= (char const*)ptr && (char const*)ptr < space->limit;
}

static bool semispaceShouldGrow(Semispace const* space, Semispace const* other) {
    size_t const size = semispaceSize(space);
    size_t const otherSize = semispaceSize(other);
    if (otherSize > size) { return true; } // Catch up

    size_t const otherSlack = (size_t)(other->limit - other->free);
    return otherSlack < otherSize / 5; // `other` is > 80% full
}

static void refurbishSemispace(Semispace* space, Semispace const* other) {
    size_t const size = semispaceSize(space);

    if (semispaceShouldGrow(space, other)) {
        size_t const newSize = size + size / 2;
        char* const newStart = calloc(newSize, sizeof *newStart);
        free(space->start);
        space->limit = newStart + newSize;
        space->start = newStart;
    } else {
        memset(space->start, 0, size);
    }

    space->free = space->start;
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

[[nodiscard]]
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
    
    return ptr;
}

[[nodiscard]]
static void* allocOrDie(Semispace* semispace, Type const* type) {
    void* const res = tryAlloc(semispace, type);
    if (!res) { exit(EXIT_FAILURE); } // OOM
    return res;
}

[[nodiscard]]
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
    
    return ptr;
}

[[nodiscard]]
static void* allocFlexOrDie(Semispace* semispace, Type const* type, Fixnum length) {
    void* const res = tryAllocFlex(semispace, type, length);
    if (!res) { exit(EXIT_FAILURE); } // OOM
    return res;
}

[[nodiscard]]
static void* tryShallowCopy(Heap* heap, void* ptr) {
    assert(allocatedInSemispace(&heap->tospace, ptr)        // Normal cloning
           || allocatedInSemispace(&heap->fromspace, ptr)); // GC

    Header const header = *((Header*)ptr - 1);
    Type const* const type = typeToPtr(headerType(header)); // OPTIMIZE: tag-untag

    void* copy = nullptr;
    size_t size = (uintptr_t)fixnumToInt(type->minSize);
    if (!unwrapBool(type->isFlex)) {
        copy = tryAlloc(&heap->tospace, type);
        if (!copy) { return nullptr; }

        *((Header*)copy - 1) = header;
    } else {
        FlexHeader const flexHeader = *((FlexHeader*)ptr - 1);

        Fixnum const fxLen = flexHeader.length;
        copy = tryAllocFlex(&heap->tospace, type, fxLen);
        if (!copy) { return nullptr; }

        *((FlexHeader*)copy - 1) = flexHeader;

        size_t const len = (uintptr_t)fixnumToInt(fxLen);
        size += unwrapBool(type->isBytes) ? len : len * sizeof(ORef);
    }

    memcpy(copy, ptr, size);

    return copy;
}

[[nodiscard]]
static ORef mark(Heap* heap, ORef oref) {
    if (!isHeaped(oref)) { return oref; }

    void* const ptr = uncheckedORefToPtr(oref);

    void* const fwdPtr = tryForwarded(ptr);
    if (fwdPtr) { return tagHeaped(fwdPtr); }

    void* const copy = tryShallowCopy(heap, ptr);
    assert(copy); // Copying should always succeed since tospace is at least as big as fromspace.
    forwardTo(ptr, copy);
    return tagHeaped(copy);
}

[[nodiscard]]
static Header markHeader(Heap* heap, Header header) {
    TypeRef const type = uncheckedORefToTypeRef(mark(heap, typeToORef(headerType(header))));
    return fixedHeader(typeToPtr(type));
}

[[nodiscard]]
static void* nextGrey(void* const scan) {
    uintptr_t address = (uintptr_t)scan;
    uintptr_t const align = alignof(ORef);
    address = (address + align - 1) & ~(align - 1);

    ORef* orefScan = (ORef*)(void*)address;

    while (eq(*orefScan, AlignmentHole)) { ++orefScan; } // Skip <alignmentHole>*

    if (isFixnum(*orefScan)) { ++orefScan; } // Skip <flexCount>?

    ++orefScan; // Skip <header>

    return (void*)orefScan;
}

[[nodiscard]]
static void* scanObj(Heap* heap, void* const scan) {
    assert(heap->tospace.start <= (char*)scan && (char*)scan < heap->tospace.limit);
    assert((uintptr_t)scan % alignof(ORef) == 0);

    Header* const header = (Header*)scan - 1;
    *header = markHeader(heap, *header);
    Type* const type = typeToPtr(headerType(*header));

    char* byteScan = (char*)scan;

    if (eq(boolToORef(type->isBytes), boolToORef(True))) {
        byteScan += (uintptr_t)fixnumToInt(type->minSize); // Skip fixed portion

        if (eq(boolToORef(type->isFlex), boolToORef(True))) {
            FlexHeader const flexHeader = *((FlexHeader*)scan - 1);
            byteScan += (uintptr_t)fixnumToInt(flexHeader.length); // Skip flex portion
        }

        return (void*)byteScan;
    } else {
        size_t slotCount = (uintptr_t)fixnumToInt(type->minSize) / sizeof(ORef); // Fixed slot count
        if (eq(boolToORef(type->isFlex), boolToORef(True))) {
            FlexHeader const flexHeader = *((FlexHeader*)scan - 1);
            slotCount += (uintptr_t)fixnumToInt(flexHeader.length); // Add flex slot count
        }

        if (eq(boolToORef(type->hasCodePtr), boolToORef(True))) {
            // Skip code pointer:
            byteScan += sizeof(MethodCode);
            --slotCount; // Assuming that code pointer is ORef-sized...
        }
        // Assuming that we are still at least ORef-aligned even if we skipped a code pointer...:
        ORef* orefScan = (ORef*)byteScan;

        // Finally, actually scan slots:
        for (size_t i = 0; i < slotCount; ++i, ++orefScan) {
            *orefScan = mark(heap, *orefScan);
        }

        return (void*)orefScan;
    }
}

static void collectHeap(Heap* heap) {
    for (void* scan = heap->tospace.start; (char*)scan < heap->tospace.free;) {
        scan = nextGrey(scan);
        scan = scanObj(heap, scan);
    }
}
