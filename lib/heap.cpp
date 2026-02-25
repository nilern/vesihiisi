#include "heap.hpp"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

namespace {

// Semispace
// =================================================================================================

Heap::Semispace::Semispace(size_t size) {
    start = static_cast<decltype(start)>(calloc(size, sizeof *start));
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

    auto address = std::bit_cast<uintptr_t>(free);

    address += sizeof(Header); // Reserve header
    // Align oref:
    auto const align = uintptr_t(type->align.val());
    address = (address + align - 1) & ~(align - 1);

    // Check bound and commit reservation:
    auto const size = uintptr_t(type->minSize.val());
    auto const free = std::bit_cast<char*>(address + size);
    if (free > limit) { return nullptr; }
    this->free = free;

    auto const ptr = std::bit_cast<Object*>(address);
    *(std::bit_cast<Header*>(address) - 1) = Header{type}; // Init header

    assert(allocatedIn(ptr));
    return ptr;
}

Object* Heap::Semispace::tryAllocFlex(Type const* type, Fixnum length) {
    assert(isValid());
    assert(type->isFlex.val());
    assert(length.val() >= 0);

    auto address = std::bit_cast<uintptr_t>(free);

    address += sizeof(FlexHeader); // Reserve header
    // Align oref:
    auto const align = uintptr_t(type->align.val());
    address = (address + align - 1) & ~(align - 1);

    // Check bound and commit reservation:
    auto const len = uintptr_t(length.val());
    uintptr_t const size = type->flexSize(len);
    auto const free = std::bit_cast<char*>(address + size);
    if (free > limit) { return nullptr; }
    this->free = free;

    auto const ptr = std::bit_cast<Object*>(address);
    *(std::bit_cast<FlexHeader*>(address) - 1) = FlexHeader{length, type}; // Init header

    assert(allocatedIn(ptr));
    return ptr;
}

// Nursery
// =================================================================================================

Heap::Nursery::Nursery(size_t size) {
    start = static_cast<decltype(start)>(calloc(size, sizeof *start));
    free = start;
    end = start + size;
    remembered = std::bit_cast<Object**>(end);
}

// TODO: DRY wrt. `Heap::Semispace::tryAlloc`:
Object* Heap::Nursery::tryAlloc(Type const* type) {
    assert(isValid());
    assert(!type->isFlex.val());

    auto address = std::bit_cast<uintptr_t>(free);

    address += sizeof(Header); // Reserve header
    // Align oref:
    auto const align = uintptr_t(type->align.val());
    address = (address + align - 1) & ~(align - 1);

    // Check bound and commit reservation:
    auto const size = uintptr_t(type->minSize.val());
    auto const free =  std::bit_cast<char*>(address + size);
    if (free > std::bit_cast<char const*>(remembered)) { return nullptr; }
    this->free = free;

    auto const ptr = std::bit_cast<Object*>(address);
    *(std::bit_cast<Header*>(address) - 1) = Header{type}; // Init header

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

    auto address =  std::bit_cast<uintptr_t>(free);

    address += sizeof(FlexHeader); // Reserve header
    // Align oref:
    auto const align = uintptr_t(type->align.val());
    address = (address + align - 1) & ~(align - 1);

    // Check bound and commit reservation:
    auto const len = uintptr_t(length.val());
    uintptr_t const size = type->flexSize(len);
    auto const free = std::bit_cast<char*>(address + size);
    if (free > std::bit_cast<char const*>(remembered)) { return nullptr; }
    this->free = free;

    auto const ptr = std::bit_cast<Object*>(address);
    *(std::bit_cast<FlexHeader*>(address) - 1) = FlexHeader{length, type}; // Init header

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
    if (std::bit_cast<char const*>(newRemembered) < free) {
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

    Header const header = *obj->header();
    Type const* const type = header.typePtr();

    Object* copy = nullptr;
    size_t size = uint64_t(type->minSize.val());
    if (!type->isFlex.val()) {
        copy = tospace.tryAlloc(type);
        if (!copy) { return nullptr; }

        *copy->header() = header;
    } else {
        FlexHeader const flexHeader = *(std::bit_cast<FlexHeader*>(obj) - 1);

        Fixnum const fxLen = flexHeader.count;
        copy = tospace.tryAllocFlex(type, fxLen); // OPTIMIZE: This also computes `size` internally
        if (!copy) { return nullptr; }

        *(std::bit_cast<FlexHeader*>(copy) - 1) = flexHeader;

        size_t const len = uint64_t(fxLen.val());
        size += type->isBytes.val() ? len : len * sizeof(ORef);
    }

    memcpy(copy, obj, size);

    assert(evacuated(copy));
    return copy;
}

Heap Heap::tryCreate(size_t size) { return Heap{size}; }

Object* Heap::tryAlloc(Type const* type) {
    Object* obj = nursery.tryAlloc(type);
    if (!obj) {
        if (uint64_t(type->minSize.val()) >= nursery.size() / 2) {
            obj = tospace.tryAlloc(type);
            if (!obj) {
                escalate(); // FIXME: OOM when even tospace after GC is too small.
            }
        }
    }
    return obj;
}

Object* Heap::tryAllocFlex(Type const* type, Fixnum length) {
    Object* obj = nursery.tryAllocFlex(type, length);
    if (!obj) {
        if (uint64_t(type->flexSize(size_t(length.val()))) >= nursery.size() / 2) {
            obj = tospace.tryAllocFlex(type, length);
            if (!obj) {
                escalate(); // FIXME: OOM when even tospace after GC is too small.
            }
        }
    }
    return obj;
}

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
    assert(start <= scan && scan < limit);

    auto address = std::bit_cast<uintptr_t>(scan);
    uintptr_t const align = alignof(ORef);
    address = (address + align - 1) & ~(align - 1);

    auto orefScan = std::bit_cast<ORef*>(address);

    while (eq(*orefScan, AlignmentHole)) { ++orefScan; } // Skip <alignmentHole>*

    if (Fixnum::contains(*orefScan)) { ++orefScan; } // Skip <flexCount>?

    ++orefScan; // Skip <header>

    assert(start <= std::bit_cast<char*>(orefScan) && std::bit_cast<char*>(orefScan) < limit);
    return std::bit_cast<Object*>(orefScan);
}

char* Heap::scanObj(Object* const obj) {
    assert(tospace.start <= std::bit_cast<char*>(obj) && std::bit_cast<char*>(obj) < tospace.limit);
    assert(std::bit_cast<uintptr_t>(obj) % alignof(ORef) == 0);

    Header* const header = obj->header();
    auto const markedHeader = markHeader(*header);
    if (!markedHeader) { return nullptr; }
    *header = *markedHeader;
    Type* const type = header->typePtr();

    auto byteScan = std::bit_cast<char*>(obj);

    if (type->isBytes.val()) {
        byteScan += uint64_t(type->minSize.val()); // Skip fixed portion

        if (type->isFlex.val()) {
            FlexHeader const flexHeader = *(std::bit_cast<FlexHeader*>(obj) - 1);
            byteScan += uint64_t(flexHeader.count.val()); // Skip flex portion
        }

        return byteScan;
    } else {
        size_t slotCount = uint64_t(type->minSize.val()) / sizeof(ORef); // Fixed slot count
        if (type->isFlex.val()) {
            FlexHeader const flexHeader = *(std::bit_cast<FlexHeader*>(obj) - 1);
            slotCount += uint64_t(flexHeader.count.val()); // Add flex slot count
        }

        auto orefScan = std::bit_cast<ORef*>(byteScan);

        // Finally, actually scan slots:
        for (size_t i = 0; i < slotCount; ++i, ++orefScan) {
            std::optional<ORef> const marked = mark(*orefScan);
            if (!marked) { return nullptr; }
            *orefScan = *marked;
        }

        return std::bit_cast<char*>(orefScan);
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
