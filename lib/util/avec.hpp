#pragma once

#include <assert.h>
#include <type_traits>
#include <algorithm>
#include <optional>

#include "arena.hpp"
#include "util.hpp"

namespace {

template<typename T> requires
    (std::is_trivially_destructible<T>{}())
    && (std::is_trivially_move_constructible<T>{}()) && (std::is_trivially_move_assignable<T>{}())
class AVec {
    T* start_;
    T* end_;
    T* capEnd_;
    Arena* arena_;

    AVec(T* start, T* end, T* capEnd, Arena* arena) :
        start_{start}, end_{end}, capEnd_{capEnd}, arena_{arena}
    {}

public:
    AVec(Arena* t_arena, size_t capacity) {
        if (capacity < 2) { capacity = 2; }

        start_ = static_cast<decltype(start_)>(amalloc(t_arena, capacity * sizeof *start_));
        end_ = start_;
        capEnd_ = start_ + capacity;
        arena_ = t_arena;
    }

    explicit AVec(Arena* t_arena) : AVec{t_arena, 2} {}

    AVec(Arena* t_arena, size_t count, T const& v) : AVec{t_arena, count} {
        end_ = start_ + count;
        std::fill(start_, end_, v);
    }

    AVec<T> clone() const {
        static_assert(std::is_trivially_copyable<T>{}());

        size_t const cloneCap = cap();
        auto const data = static_cast<decltype(start_)>(amalloc(arena_, cloneCap * sizeof *start_));
        std::copy(start_, end_, data);

        return AVec{data, data + count(), data + cloneCap, arena_};
    }

    // We do not have a destructor but nevertheless e.g. pushing and popping could cause bad stuff
    // if we do not deep copy. And implicit deep copies are terrible, so just prevent copying:
    AVec(AVec<T> const&) = delete;
    AVec<T>& operator=(AVec<T> const&) = delete;

    AVec(AVec<T>&&) = default;
    AVec<T>& operator=(AVec<T>&&) = default;

    Slice<T const> slice() const { return Slice{static_cast<T const*>(start_), count()}; }

    size_t count() const { return end_ - start_; }

    size_t cap() const { return capEnd_ - start_; }

    T const& operator[](size_t i) const {
        assert(start_ + i < end_);
        return start_[i];
    }

    T& operator[](size_t i) {
        assert(start_ + i < end_);
        return start_[i];
    }

    void push(T&& v) {
        if (end_ == capEnd_) {
            grow();
        }

        *end_++ = std::move(v);
    }

    void push(T const& v) {
        static_assert(std::is_trivially_copyable<T>{}());

        if (end_ == capEnd_) {
            grow();
        }

        *end_++ = v;
    }

    std::optional<T const*> peek() const {
        return count() > 0 ? std::optional{end_ - 1} : std::nullopt;
    }

    std::optional<T> pop() {
        if (count() == 0) { return std::nullopt; }
        return std::optional{std::move(*--end_)};
    }

    using iterator = T*;
    using const_iterator = T const*;

    iterator begin() { return start_; }
    iterator end() { return end_; }

    const_iterator begin() const { return start_; }
    const_iterator end() const { return end_; }

private:
    void grow() {
        size_t const currCount = count();
        size_t const currCap = cap();
        size_t const newCap = currCap + currCap / 2;

        auto newData = static_cast<decltype(start_)>(amalloc(arena_, newCap * sizeof *start_));
        std::move(start_, end_, newData);
        start_ = newData;
        end_ = start_ + currCount;
        capEnd_ = start_ + newCap;
    }
};

}
