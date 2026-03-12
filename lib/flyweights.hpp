#pragma once

#include <utility>

#include "value.hpp"

namespace {

// Symbols
// =================================================================================================

class SymbolTable {
    ORef* entries;
    size_t count = 0;
    size_t cap = 2;

public:
    SymbolTable();

    ~SymbolTable();

    SymbolTable(SymbolTable&& that) {
        entries = that.entries;
        that.entries = nullptr;
    }

    SymbolTable& operator=(SymbolTable&& that) {
        if (entries) { free(entries); }
        entries = that.entries;
        that.entries = nullptr;

        return *this;
    }

    SymbolTable(SymbolTable const& that) = delete;
    SymbolTable& operator=(SymbolTable const& that) = delete;

    struct IndexOfRes {
        size_t index;
        bool exists;
    };
    IndexOfRes indexOf(Fixnum hash, Str name) const;

    HRef<Symbol> atIndexUnchecked(size_t i) const;

    HRef<Symbol> createAtUnchecked(RT* state, size_t i, Fixnum hash, Str name);
    HRef<Symbol> createFromHeapedAtUnchecked(
        RT* state, size_t i, Fixnum hash, HRef<String> name);

    void prune(RT const& state);

    class const_iterator {
        ORef const* ptr;
        ORef const* limit;

        const_iterator(ORef const* t_ptr, ORef const* t_limit) : ptr{t_ptr}, limit{t_limit} {
            for (; this->ptr < limit && !isHeaped(*this->ptr); ++this->ptr) {}
        }

        friend SymbolTable;

    public:
        bool operator==(const_iterator const& that) const { return this->ptr == that.ptr; }

        const_iterator& operator++() {
            ++this->ptr;
            for (; this->ptr < limit && !isHeaped(*this->ptr); ++this->ptr) {}

            return *this;
        }

        HRef<Symbol> const& operator*() const {
            return *static_cast<HRef<Symbol> const*>(this->ptr);
        }
    };

    const_iterator begin() const { return const_iterator{entries, entries + count}; }
    const_iterator end() const { return const_iterator{entries + count, entries + count}; }

private:
    void rehash();
};

HRef<Symbol> intern(RT* state, Str name);

// Specializations
// =================================================================================================

class Specializations {
    ORef* entries;
    size_t count = 0;
    size_t cap = 2;

public:
    Specializations();

    ~Specializations();

    Specializations(Specializations&& that) {
        entries = that.entries;
        that.entries = nullptr;
    }

    Specializations& operator=(Specializations&& that) {
        if (entries) { free(entries); }
        entries = that.entries;
        that.entries = nullptr;

        return *this;
    }

    Specializations(Specializations const& that) = delete;
    Specializations& operator=(Specializations const& that) = delete;

    // TODO: DRY wrt. `SymbolTable::IndexOfRes`:
    struct IndexOfRes {
        size_t index;
        bool exists;
    };
    IndexOfRes indexOf(uintptr_t h, HRef<Method> generic, HRef<Array> types) const;

    HRef<Method> atIndexUnchecked(size_t i) const;

    HRef<Method> createAtUnchecked(
        RT* state, size_t i, Fixnum fxHash, HRef<Method> generic, HRef<Array> types);

    void prune(RT const& state);

    class const_iterator {
        ORef const* ptr;
        ORef const* limit;

        const_iterator(ORef const* t_ptr, ORef const* t_limit) : ptr{t_ptr}, limit{t_limit} {
            for (; this->ptr < limit && !isHeaped(*this->ptr); ++this->ptr) {}
        }

        friend Specializations;

    public:
        bool operator==(const_iterator const& that) const { return this->ptr == that.ptr; }

        const_iterator& operator++() {
            ++this->ptr;
            for (; this->ptr < limit && !isHeaped(*this->ptr); ++this->ptr) {}

            return *this;
        }

        HRef<Method> const& operator*() const {
            return *static_cast<HRef<Method> const*>(this->ptr);
        }
    };

    const_iterator begin() const { return const_iterator{entries, entries + count}; }
    const_iterator end() const { return const_iterator{entries + count, entries + count}; }

private:
    void rehash();
};

HRef<Method> specialize(RT* state, HRef<Method> generic, HRef<Array> types);

} // namespace
