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

    Semispace tospace;
    Semispace fromspace;

    explicit Heap(size_t size) : tospace{size / 2}, fromspace{size / 2} {}

    Object* tryShallowCopy(Object* obj);

    [[nodiscard]]
    Header markHeader(Header header);

    void* scanObj(void* const scan);

public:
    static Heap tryCreate(size_t size);

    [[nodiscard]]
    bool isValid() const { return tospace.isValid() && fromspace.isValid(); }

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

    void writeBarrier(Object* /*dest*/) { /*TODO*/ }

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

inline bool mustCollect(void const* ptr) {
#ifdef GC_ALOT
    return !ptr || true;
#else
    return !ptr;
#endif
}

} // namespace
