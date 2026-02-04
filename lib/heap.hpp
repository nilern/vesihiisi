#pragma once

#include <utility>

#include "object.hpp"

namespace {

class Heap {
    struct Semispace {
        char* free;
        char* limit;
        char* start;

        explicit Semispace(size_t size);

        ~Semispace() { std::free(start); }

        Semispace(Semispace&& that) : free{that.free}, limit{that.limit}, start{that.start} {
            that.start = nullptr; // Prevent double free
        }
        Semispace& operator=(Semispace&& that);

        // Deep copying these would be a farce and shallow copy causes use-after-free:ing
        // destructions:
        Semispace(Semispace const&) = delete;
        Semispace& operator=(Semispace const&) = delete;

        [[nodiscard]]
        bool isValid() const { return free != nullptr; }

        size_t size() const { return size_t(limit - start); }

        [[nodiscard]]
        bool allocatedIn(Object const* obj) const {
            auto const data = reinterpret_cast<char const*>(obj);
            return start <= data && data < free;
        }

        [[nodiscard]]
        Object* tryAlloc(Type const* type);

        [[nodiscard]]
        Object* allocOrDie(Type const* type);

        [[nodiscard]]
        Object* tryAllocFlex(Type const* type, Fixnum length);

        [[nodiscard]]
        Object* allocFlexOrDie(Type const* type, Fixnum length);

        void refurbish(Semispace const& other);

    private:
        bool shouldGrow(Semispace const& other) const;
    };

    struct Nursery {
        char* free;
        Object** remembered;
        char* start;
        char* end;

        explicit Nursery(size_t size);

        ~Nursery() { std::free(start); }

        Nursery(Nursery&& that) :
            free{that.free}, remembered{that.remembered}, start{that.start}, end{that.end}
        {
            that.start = nullptr; // Prevent double free
        }
        Nursery& operator=(Nursery&& that) = delete; // Unused

        // Deep copying these would be a farce and shallow copy causes use-after-free:ing
        // destructions:
        Nursery(Nursery const&) = delete;
        Nursery& operator=(Nursery const&) = delete;

        [[nodiscard]]
        bool isValid() const { return free != nullptr; }

        size_t size() const { return size_t(end - start); }

        [[maybe_unused]]
        [[nodiscard]]
        Object* tryAlloc(Type const* type);

        [[maybe_unused]]
        [[nodiscard]]
        Object* tryAllocFlex(Type const* type, Fixnum length);

        [[nodiscard]]
        bool tryToRemember(Object* obj);
    };

    Nursery nursery;
    Semispace tospace;
    Semispace fromspace;

    explicit Heap(size_t size) :
        nursery{size / 9}, tospace{nursery.size() * 4}, fromspace{tospace.size()}
    {}

    Object* tryShallowCopy(Object* obj);

    [[nodiscard]]
    Header markHeader(Header header);

    void* scanObj(void* const scan);

public:
    static Heap tryCreate(size_t size);

    [[nodiscard]]
    bool isValid() const { return tospace.isValid() && fromspace.isValid() && nursery.isValid(); }

    // FIXME: Allocating objects that do not fit in nursery (or even tospace!):

    [[nodiscard]]
    Object* tryAlloc(Type const* type) { return tospace.tryAlloc(type); }

    [[nodiscard]]
    Object* allocOrDie(Type const* type) { return tospace.allocOrDie(type); }

    [[nodiscard]]
    Object* tryAllocFlex(Type const* type, Fixnum length) {
        return tospace.tryAllocFlex(type, length);
    }

    [[nodiscard]]
    Object* allocFlexOrDie(Type const* type, Fixnum length) {
        return tospace.allocFlexOrDie(type, length);
    }

    [[nodiscard]]
    bool writeBarrier(Object* dest) { return nursery.tryToRemember(dest); }

    [[nodiscard]]
    Object* mark(Object* obj);
    [[nodiscard]]
    ORef mark(ORef oref);

    void flipSemispaces() { std::swap(tospace, fromspace); }

    void collect();

    void refurbish() { fromspace.refurbish(tospace); }

    [[nodiscard]]
    bool evacuated(Object const* obj) const { return tospace.allocatedIn(obj); }
};

inline bool mustCollect([[maybe_unused]] void const* ptr) {
#ifndef GC_ALOT
    return !ptr;
#else
    return true;
#endif
}

} // namespace
