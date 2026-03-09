#pragma once

#include <optional>
#include <utility>

#include "value.hpp"

namespace {

class Heap {
    struct Semispace {
        char* free;
        char* scan;
        char* limit;
        char* start;

        explicit Semispace(size_t size);

        ~Semispace() { std::free(start); }

        Semispace(Semispace&& that) :
            free{that.free}, scan{that.scan}, limit{that.limit}, start{that.start}
        {
            that.start = nullptr; // Prevent double free
        }
        Semispace& operator=(Semispace&& that);

        // Deep copying these would be a farce and shallow copy causes use-after-free:ing
        // destructions:
        Semispace(Semispace const&) = delete;
        Semispace& operator=(Semispace const&) = delete;

        [[nodiscard]]
        bool isValid() const { return start != nullptr; }

        size_t size() const { return size_t(limit - start); }

        [[nodiscard]]
        bool allocatedIn(Object const* obj) const {
            auto const data = std::bit_cast<char const*>(obj);
            return start <= data && data <= free; // `data == free` can hold for zero-sized objs
        }

        [[nodiscard]]
        Object* tryAlloc(Type const* type);

        [[nodiscard]]
        Object* tryAllocFlex(Type const* type, Fixnum length);

        [[nodiscard]]
        Object* nextGrey(char* scan) const;

        void refurbish(Semispace const& other);

    private:
        bool shouldGrow(Semispace const& other) const {
            return other.size() > size(); // Catch up?
        }
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
        bool isValid() const { return start != nullptr; }

        size_t size() const { return size_t(end - start); }

        [[nodiscard]]
        bool allocatedIn(Object const* obj) const {
            auto const data = std::bit_cast<char const*>(obj);
            return start <= data && data <= free; // `data == free` can hold for zero-sized objs
        }

        [[nodiscard]]
        Object* tryAlloc(Type const* type);

        [[nodiscard]]
        Object* allocOrDie(Type const* type);

        [[nodiscard]]
        Object* tryAllocFlex(Type const* type, Fixnum length);

        [[nodiscard]]
        Object* allocFlexOrDie(Type const* type, Fixnum length);

        std::span<Object* const> remembereds() const {
            return std::span{remembered, std::bit_cast<Object**>(end)};
        }

        [[nodiscard]]
        bool tryToRemember(Object* obj);

        void refurbish() {
            memset(start, 0, size());
            free = start;
            remembered = std::bit_cast<Object**>(end);
        }
    };

public:
    enum CollectionMode { MINOR, MAJOR, EXPANDING };

    Nursery nursery;
    Semispace tospace;
    Semispace fromspace;
    std::optional<Semispace> insufficientTospace = std::nullopt;
    CollectionMode mode = MINOR;

    explicit Heap(size_t size) :
        nursery{size / 9}, tospace{nursery.size() * 4}, fromspace{tospace.size()}
    {}

    Object* tryEvacuate(Object* obj);

    [[nodiscard]]
    std::optional<Header> markHeader(Header header);

    [[nodiscard]]
    char* scanObj(Object* obj);

    [[nodiscard]]
    Object* mark(Object* obj);

    void flipSemispaces() { std::swap(tospace, fromspace); }

    void growTospace();

    void escalate();

public:
    static Heap tryCreate(size_t size);

    [[nodiscard]]
    bool isValid() const { return tospace.isValid() && fromspace.isValid() && nursery.isValid(); }

    [[nodiscard]]
    Object* tryAlloc(Type const* type);

    [[nodiscard]]
    Object* allocOrDie(Type const* type) { return nursery.allocOrDie(type); }

    [[nodiscard]]
    Object* tryAllocFlex(Type const* type, Fixnum length);

    [[nodiscard]]
    Object* allocFlexOrDie(Type const* type, Fixnum length) {
        return nursery.allocFlexOrDie(type, length);
    }

    // OPTIMIZE: Do all/some filtering here instead of leaving it all to collection time:
    [[nodiscard]]
    bool writeBarrier(Object* dest) { return nursery.tryToRemember(dest); }
    [[nodiscard]]
    static bool writeBarrier(Heap* heap, Object* dest) { return heap->writeBarrier(dest); }
    using writeBarrier_t = bool (*)(Heap* heap, Object* dest);

    [[nodiscard]]
    std::optional<ORef> mark(ORef oref);

    [[nodiscard]]
    bool collect();

    void refurbish();

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
