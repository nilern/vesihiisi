#include "heap.hpp"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

namespace {

// Semispace
// =================================================================================================

Heap::Semispace::Semispace(size_t size) {
    start = (char*)calloc(size, sizeof *start);
    free = start;
    scan = start;
    limit = start + size;
}

Heap::Semispace& Heap::Semispace::operator=(Semispace&& that) {
    this->free = that.free;
    this->scan = that.scan;
    this->limit = that.limit;
    this->start = that.start;

    that.start = nullptr; // Prevent double free

    return *this;
}

void Heap::Semispace::refurbish(Semispace const& other) {
    size_t const mySize = size();

    if (shouldGrow(other)) {
        size_t const newSize = mySize + mySize / 2;
        char* const newStart = static_cast<char*>(calloc(newSize, sizeof *newStart));
        std::free(start);
        limit = newStart + newSize;
        start = newStart;
    } else {
        memset(start, 0, mySize);
    }

    free = start;
    scan = start;
}

Object* Heap::Semispace::tryAlloc(Type const* type) {
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
    if (free > limit) { return nullptr; }
    this->free = free;

    Object* const ptr = (Object*)(void*)address;
    *((Header*)(void*)address - 1) = Header{type}; // Init header

    assert(allocatedIn(ptr));
    return ptr;
}

Object* Heap::Semispace::tryAllocFlex(Type const* type, Fixnum length) {
    assert(isValid());
    assert(type->isFlex.val());
    assert(length.val() >= 0);

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
    if (free > limit) { return nullptr; }
    this->free = free;

    Object* const ptr = (Object*)(void*)address;
    *((FlexHeader*)(void*)address - 1) = FlexHeader{length, type}; // Init header

    assert(allocatedIn(ptr));
    return ptr;
}

// Nursery
// =================================================================================================

Heap::Nursery::Nursery(size_t size) {
    start = (char*)calloc(size, sizeof *start);
    free = start;
    end = start + size;
    remembered = reinterpret_cast<Object**>(end);
}

// TODO: DRY wrt. `Heap::Semispace::tryAlloc`:
Object* Heap::Nursery::tryAlloc(Type const* type) {
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
    if (free > reinterpret_cast<char const*>(remembered)) { return nullptr; }
    this->free = free;

    Object* const ptr = (Object*)(void*)address;
    *((Header*)(void*)address - 1) = Header{type}; // Init header

    assert(allocatedIn(ptr));
    return ptr;
}

Object* Heap::Nursery::allocOrDie(Type const* type) {
    Object* const res = tryAlloc(type);
    if (!res) { PANIC("Out of memory"); }
    return res;
}

// TODO: DRY wrt. `Heap::Semispace::tryAllocFlex`:
Object* Heap::Nursery::tryAllocFlex(Type const* type, Fixnum length) {
    assert(isValid());
    assert(type->isFlex.val());
    assert(length.val() >= 0);

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
    if (free > reinterpret_cast<char const*>(remembered)) { return nullptr; }
    this->free = free;

    Object* const ptr = (Object*)(void*)address;
    *((FlexHeader*)(void*)address - 1) = FlexHeader{length, type}; // Init header

    assert(allocatedIn(ptr));
    return ptr;
}

Object* Heap::Nursery::allocFlexOrDie(Type const* type, Fixnum length) {
    Object* const res = tryAllocFlex(type, length);
    if (!res) { PANIC("Out of memory"); }
    return res;
}

bool Heap::Nursery::tryToRemember([[maybe_unused]] Object* obj) {
#ifndef GC_ALOT
    Object** const newRemembered = remembered - 1;
    if (reinterpret_cast<char const*>(newRemembered) < free) {
        return false;
    }

    *newRemembered = obj;
    remembered = newRemembered;
    return true;
#else
    return false;
#endif
}

// Heap
// =================================================================================================

[[nodiscard]]
Object* Heap::tryEvacuate(Object* obj) {
    assert(nursery.allocatedIn(obj) // Any GC
           || fromspace.allocatedIn(obj) // Major or expanding GC
           || (insufficientTospace && insufficientTospace->allocatedIn(obj))); // Expanding GC

    Header const header = *((Header*)obj - 1);
    Type const* const type = header.typePtr();

    Object* copy = nullptr;
    size_t size = (uintptr_t)type->minSize.val();
    if (!type->isFlex.val()) {
        copy = tospace.tryAlloc(type);
        if (!copy) { return nullptr; }

        *((Header*)copy - 1) = header;
    } else {
        FlexHeader const flexHeader = *((FlexHeader*)obj - 1);

        Fixnum const fxLen = flexHeader.count;
        copy = tospace.tryAllocFlex(type, fxLen); // OPTIMIZE: This also computes `size` internally
        if (!copy) { return nullptr; }

        *((FlexHeader*)copy - 1) = flexHeader;

        size_t const len = (uintptr_t)fxLen.val();
        size += type->isBytes.val() ? len : len * sizeof(ORef);
    }

    memcpy(copy, obj, size);

    assert(evacuated(copy));
    return copy;
}

Heap Heap::tryCreate(size_t size) { return Heap{size}; }

[[nodiscard]]
Object* Heap::mark(Object* obj) {
    obj = obj->canonical();
    if (evacuated(obj)) { return obj; }

    Object* const copy = tryEvacuate(obj);
    if (!copy) {
        escalate();
        return nullptr;
    }

    obj->forwardTo(copy);
    return copy;
}

std::optional<ORef> Heap::mark(ORef oref) {
    if (!isHeaped(oref)) { return std::optional{oref}; }

    Object* const ptr = &*HRef<Object>::fromUnchecked(oref);
    Object* const copy = mark(ptr);
    return copy ? std::optional{tagHeaped(copy)} : std::nullopt;
}

std::optional<Header> Heap::markHeader(Header header) {
    auto const type = static_cast<Type*>(mark(header.typePtr()));
    if (!type) { return std::nullopt; }
    return std::optional{Header{type}};
}

Object* Heap::Semispace::nextGrey(char* scan) const {
    assert(start <= (char*)scan && (char*)scan < limit);

    uintptr_t address = (uintptr_t)scan;
    uintptr_t const align = alignof(ORef);
    address = (address + align - 1) & ~(align - 1);

    ORef* orefScan = (ORef*)(void*)address;

    while (eq(*orefScan, AlignmentHole)) { ++orefScan; } // Skip <alignmentHole>*

    if (Fixnum::contains(*orefScan)) { ++orefScan; } // Skip <flexCount>?

    ++orefScan; // Skip <header>

    return reinterpret_cast<Object*>(orefScan);
}

char* Heap::scanObj(Object* const obj) {
    assert(tospace.start <= (char*)obj && (char*)obj < tospace.limit);
    assert((uintptr_t)obj % alignof(ORef) == 0);

    Header* const header = obj->header();
    auto const markedHeader = markHeader(*header);
    if (!markedHeader) { return nullptr; }
    *header = *markedHeader;
    Type* const type = header->typePtr();

    char* byteScan = (char*)obj;

    if (type->isBytes.val()) {
        byteScan += (uintptr_t)type->minSize.val(); // Skip fixed portion

        if (type->isFlex.val()) {
            FlexHeader const flexHeader = *((FlexHeader*)obj - 1);
            byteScan += (uintptr_t)flexHeader.count.val(); // Skip flex portion
        }

        return byteScan;
    } else {
        size_t slotCount = (uintptr_t)type->minSize.val() / sizeof(ORef); // Fixed slot count
        if (type->isFlex.val()) {
            FlexHeader const flexHeader = *((FlexHeader*)obj - 1);
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
            std::optional<ORef> const marked = mark(*orefScan);
            if (!marked) { return nullptr; }
            *orefScan = *marked;
        }

        return reinterpret_cast<char*>(orefScan);
    }
}

bool Heap::collect() {
    if (mode == MINOR) {
        for (auto const remembered : nursery.remembereds()) {
            if (tospace.allocatedIn(remembered)) {
                if (!scanObj(remembered)) { return false; }
            }
        }
    }

    while (tospace.scan < tospace.free) {
        auto const scan = scanObj(tospace.nextGrey(tospace.scan));
        if (!scan) { return false; }

        tospace.scan = scan;
    }

    return true;
}

void Heap::growTospace() {
    insufficientTospace = std::optional{std::move(tospace)};
    // `2 * (fromspace.size() + nursery.size())` should accomodate everything from both nursery
    // and fromspace even if there were no alignment holes before the collection and the collection
    // produces the maximum possible amount of alignment holes:
    tospace = Semispace{2 * (fromspace.size() + nursery.size())};
}

void Heap::escalate() {
    switch (mode) {
    case MINOR: {
        flipSemispaces();
        mode = MAJOR;
    }; break;

    case MAJOR: {
        growTospace();
        mode = EXPANDING;
    }; break;

    case EXPANDING: PANIC("Out of memory");
    }
}

void Heap::refurbish() {
    nursery.refurbish();
    if (mode >= MAJOR) { fromspace.refurbish(tospace); }
    if (mode >= EXPANDING) { insufficientTospace = std::nullopt; }

    mode = MINOR;
}

} // namespace
