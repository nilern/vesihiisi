#pragma once

#include <utility>

#include "object.hpp"

namespace {

struct Semispace {
    char* free;
    char* limit;
    char* start;

    ~Semispace() { std::free(start); } // Need a destructor

    Semispace(Semispace&& that) : free{that.free}, limit{that.limit}, start{that.start} {
        that.start = nullptr; // Prevent double free
    }
    Semispace& operator=(Semispace&& that) {
        this->free = that.free;
        this->limit = that.limit;
        this->start = that.start;

        that.start = nullptr;

        return *this;
    }

    // Deep copying these would be a farce and shallow copy causes use-after-free:ing destructions:
    Semispace(Semispace const&) = delete;
    Semispace& operator=(Semispace const&) = delete;

    [[nodiscard]]
    bool isValid() const { return free != nullptr; }

    size_t size() const { return (size_t)(limit - start); }

    bool contains(void const* ptr) const {
        return start <= (char const*)ptr && (char const*)ptr < limit;
    }

    [[nodiscard]]
    Object* tryAlloc(Type const* type);

    [[nodiscard]]
    Object* allocOrDie(Type const* type);

    [[nodiscard]]
    Object* tryAllocFlex(Type const* type, Fixnum length);

    [[nodiscard]]
    Object* allocFlexOrDie(Type const* type, Fixnum length);

    void refurbish(Semispace const* other);

private:
    friend struct Heap;

    Semispace(size_t size);

    bool shouldGrow(Semispace const* other) const;
};

inline bool allocatedInSemispace(Semispace const* space, void const* ptr) {
    return space->start <= (char const*)ptr && (char const*)ptr < space->free;
}

typedef struct Heap {
    Semispace tospace;
    Semispace fromspace;

    static Heap tryCreate(size_t size);

    [[nodiscard]]
    bool isValid() const { return tospace.isValid() && fromspace.isValid(); }

    [[nodiscard]]
    ORef mark(ORef oref);

    void collect();

private:
    Heap(size_t size) : tospace{size / 2}, fromspace{size / 2} {}

    Object* tryShallowCopy(void* ptr);

    [[nodiscard]]
    Header markHeader(Header header);

    void* scanObj(void* const scan);
} Heap;

inline bool mustCollect(void const* ptr) {
#ifdef GC_ALOT
    return !ptr || true;
#else
    return !ptr;
#endif
}

inline void flipSemispaces(Heap* heap) {
    /*Semispace tmp = std::move(heap->tospace);
    heap->tospace = std::move(heap->fromspace);
    heap->fromspace = std::move(tmp);*/
    std::swap(heap->tospace, heap->fromspace);
}

} // namespace
