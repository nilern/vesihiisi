#include "heap.hpp"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

namespace {

Semispace::Semispace(size_t size) {
    start = (char*)calloc(size, sizeof *start);
    free = start;
    limit = start + size;
}

bool Semispace::shouldGrow(Semispace const* other) const {
    size_t const otherSize = other->size();
    if (otherSize > size()) { return true; } // Catch up

    size_t const otherSlack = (size_t)(other->limit - other->free);
    return otherSlack < otherSize / 5; // `other` is > 80% full
}

void Semispace::refurbish(Semispace const* other) {
    size_t const mySize = size();

    if (shouldGrow(other)) {
        size_t const newSize = mySize + mySize / 2;
        char* const newStart = (char*)calloc(newSize, sizeof *newStart);
        std::free(start);
        limit = newStart + newSize;
        start = newStart;
    } else {
        memset(start, 0, mySize);
    }

    free = start;
}

[[nodiscard]]
Object* Semispace::tryAlloc(Type const* type) {
    assert(isValid());
    assert(!type->isFlex.val());

    uintptr_t address = (uintptr_t)(void*)free;
    
    address += sizeof(Header); // Reserve header
    // Align oref:
    uintptr_t const align = (uintptr_t)type->align.val();
    address = (address + align - 1) & ~(align - 1);
    
    // Check bound and commit reservation:
    uintptr_t const size = (uintptr_t)type->minSize.val();
    char* const free = (char*)(void*)(address + size);
    if (free >= limit) { return nullptr; }
    this->free = free;
    
    Object* const ptr = (Object*)(void*)address;
    *((Header*)(void*)address - 1) = Header{type}; // Init header
    
    return ptr;
}

[[nodiscard]]
Object* Semispace::allocOrDie(Type const* type) {
    Object* const res = tryAlloc(type);
    if (!res) { exit(EXIT_FAILURE); } // OOM
    return res;
}

[[nodiscard]]
Object* Semispace::tryAllocFlex(Type const* type, Fixnum length) {
    assert(isValid());
    assert(type->isFlex.val());

    uintptr_t address = (uintptr_t)(void*)free;
    
    address += sizeof(FlexHeader); // Reserve header
    // Align oref:
    uintptr_t const align = (uintptr_t)type->align.val();
    address = (address + align - 1) & ~(align - 1);
    
    // Check bound and commit reservation:
    uintptr_t len = (uintptr_t)length.val();
    uintptr_t const flexSize = type->isBytes.val() ? len : len * sizeof(ORef);
    uintptr_t const size = (uintptr_t)type->minSize.val() + flexSize;
    char* const free = (char*)(void*)(address + size);
    if (free >= limit) { return nullptr; }
    this->free = free;
    
    Object* const ptr = (Object*)(void*)address;
    *((FlexHeader*)(void*)address - 1) = FlexHeader{length, type}; // Init header
    
    return ptr;
}

[[nodiscard]]
Object* Semispace::allocFlexOrDie(Type const* type, Fixnum length) {
    Object* const res = tryAllocFlex(type, length);
    if (!res) { exit(EXIT_FAILURE); } // OOM
    return res;
}

[[nodiscard]]
Object* Heap::tryShallowCopy(void* ptr) {
    assert(allocatedInSemispace(&tospace, ptr)        // Normal cloning
           || allocatedInSemispace(&fromspace, ptr)); // GC

    Header const header = *((Header*)ptr - 1);
    Type const* const type = header.type().ptr(); // OPTIMIZE: tag-untag

    Object* copy = nullptr;
    size_t size = (uintptr_t)type->minSize.val();
    if (!type->isFlex.val()) {
        copy = tospace.tryAlloc(type);
        if (!copy) { return nullptr; }

        *((Header*)copy - 1) = header;
    } else {
        FlexHeader const flexHeader = *((FlexHeader*)ptr - 1);

        Fixnum const fxLen = flexHeader.count;
        copy = tospace.tryAllocFlex(type, fxLen);
        if (!copy) { return nullptr; }

        *((FlexHeader*)copy - 1) = flexHeader;

        size_t const len = (uintptr_t)fxLen.val();
        size += type->isBytes.val() ? len : len * sizeof(ORef);
    }

    memcpy(copy, ptr, size);

    return copy;
}

Heap Heap::tryCreate(size_t size) { return Heap{size}; }

[[nodiscard]]
ORef Heap::mark(ORef oref) {
    if (!isHeaped(oref)) { return oref; }

    Object* const ptr = uncheckedORefToPtr(oref);

    Object* const fwdPtr = ptr->tryForwarded();
    if (fwdPtr) { return tagHeaped(fwdPtr); }

    Object* const copy = tryShallowCopy(ptr);
    assert(copy); // Copying should always succeed since tospace is at least as big as fromspace.
    ptr->forwardTo(copy);
    return tagHeaped(copy);
}

[[nodiscard]]
Header Heap::markHeader(Header header) {
    HRef<Type> const type = HRef<Type>::fromUnchecked(mark(header.type().oref()));
    return Header{type.ptr()};
}

[[nodiscard]]
void* nextGrey(void* const scan) {
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
void* Heap::scanObj(void* const scan) {
    assert(tospace.start <= (char*)scan && (char*)scan < tospace.limit);
    assert((uintptr_t)scan % alignof(ORef) == 0);

    Header* const header = (Header*)scan - 1;
    *header = markHeader(*header);
    Type* const type = header->type().ptr();

    char* byteScan = (char*)scan;

    if (type->isBytes.val()) {
        byteScan += (uintptr_t)type->minSize.val(); // Skip fixed portion

        if (type->isFlex.val()) {
            FlexHeader const flexHeader = *((FlexHeader*)scan - 1);
            byteScan += (uintptr_t)flexHeader.count.val(); // Skip flex portion
        }

        return (void*)byteScan;
    } else {
        size_t slotCount = (uintptr_t)type->minSize.val() / sizeof(ORef); // Fixed slot count
        if (type->isFlex.val()) {
            FlexHeader const flexHeader = *((FlexHeader*)scan - 1);
            slotCount += (uintptr_t)flexHeader.count.val(); // Add flex slot count
        }

        if (type->hasCodePtr.val()) {
            // Skip code pointer:
            byteScan += sizeof(MethodCode);
            --slotCount; // Assuming that code pointer is ORef-sized...
        }
        // Assuming that we are still at least ORef-aligned even if we skipped a code pointer...:
        ORef* orefScan = (ORef*)byteScan;

        // Finally, actually scan slots:
        for (size_t i = 0; i < slotCount; ++i, ++orefScan) {
            *orefScan = mark(*orefScan);
        }

        return (void*)orefScan;
    }
}

void Heap::collect() {
    for (void* scan = tospace.start; (char*)scan < tospace.free;) {
        scan = nextGrey(scan);
        scan = scanObj(scan);
    }
}

} // namespace
