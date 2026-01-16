#pragma once

#include <assert.h>
#include <type_traits>

#include "arena.hpp"
#include "util.hpp"

namespace {

template<typename T> requires std::is_trivially_copyable_v<T>
class AVec {
    T* start;
    T* end;
    T* capEnd;
    Arena* arena;

public:
    AVec(Arena* t_arena, size_t capacity) {
        assert(capacity >= 2);

        start = static_cast<decltype(start)>(amalloc(t_arena, capacity * sizeof *start));
        end = start;
        capEnd = start + capacity;
        arena = t_arena;
    }

    AVec(Arena* t_arena) : AVec{t_arena, 2} {}

    // We do not have a destructor but nevertheless e.g. pushing and popping could cause bad stuff
    // if we do not deep copy. And implicit deep copies are terrible, so just prevent copying:
    AVec(AVec<T> const&) = delete;
    AVec<T>& operator=(AVec<T> const&) = delete;

    AVec(AVec<T>&&) = default;
    AVec<T>& operator=(AVec<T>&&) = default;

    Slice<T const> slice() const { return Slice{static_cast<T const*>(start), count()}; }

    size_t count() const { return end - start; }

    size_t cap() const { return capEnd - start; }

    T const& operator[](size_t i) const {
        assert(start + i < end);
        return start[i];
    }

    T& operator[](size_t i) {
        assert(start + i < end);
        return start[i];
    }

    void push(T v) {
        if (end == capEnd) {
            grow();
        }

        *end++ = v;
    }

private:
    void grow() {
        size_t const currCount = count();
        size_t const currCap = cap();
        size_t const newCap = currCap + currCap / 2;

        start = static_cast<decltype(start)>(
            arealloc(arena, start, currCap * sizeof *start, newCap * sizeof *start));
        end = start + currCount;
        capEnd = start + newCap;
    }
};

}
