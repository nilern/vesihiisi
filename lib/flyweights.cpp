#include "flyweights.hpp"

#include <stdlib.h>

#include "util/util.hpp"
#include "state.hpp"

namespace {

// Symbols
// =================================================================================================

SymbolTable::SymbolTable() { entries = (ORef*)calloc(cap, sizeof *entries); }

SymbolTable::~SymbolTable() { free(entries); }

void SymbolTable::prune() {
    for (size_t i = 0; i < cap; ++i) {
        ORef* const v = &entries[i];
        if (isHeaped(*v)) {
            Object* const fwdPtr = uncheckedORefToPtr(*v)->tryForwarded();
            *v = fwdPtr ? HRef(fwdPtr).oref() : Tombstone;
        }
    }
}

HRef<Symbol> createUninternedSymbol(State* state, Fixnum hash, Str name) {
    Symbol* ptr = (Symbol*)state->heap.tospace.tryAllocFlex(
        state->types.symbol.ptr(), Fixnum((intptr_t)name.len));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Symbol*)state->heap.tospace.allocFlexOrDie(
            state->types.symbol.ptr(), Fixnum((intptr_t)name.len));
    }

    ptr->hash = hash;
    memcpy((char*)ptr->flexData(), name.data, name.len);

    return HRef(ptr);
}

HRef<Symbol> createUninternedSymbolFromHeaped(State* state, Fixnum hash, HRef<String> name) {
    Symbol* ptr = (Symbol*)state->heap.tospace.tryAllocFlex(
        state->types.symbol.ptr(), name.ptr()->flexCount());
    if (mustCollect(ptr)) {
        auto const nameG = state->pushRoot(&name);
        collect(state);
        ptr = (Symbol*)state->heap.tospace.allocFlexOrDie(
            state->types.symbol.ptr(), name.ptr()->flexCount());
    }

    ptr->hash = hash;
    memcpy((char*)ptr->flexData(), name.ptr()->flexData(), size_t(name.ptr()->flexCount().val()));

    return HRef(ptr);
}

Fixnum hashStr(Str s) { return Fixnum((intptr_t)fnv1aHash(s)); }

SymbolTable::IndexOfRes SymbolTable::indexOf(Fixnum hash, Str name) const {
    uintptr_t const h = (uintptr_t)hash.val();

    size_t const maxIndex = cap - 1;
    for (size_t collisions = 0, i = h & maxIndex;; ++collisions, i = (i + collisions) & maxIndex) {
        ORef* const entry = entries + i;

        if (eq(*entry, Default)) { return IndexOfRes{i, false}; }

        if (isHeaped(*entry)) {
            HRef<Symbol> const symbol = HRef<Symbol>::fromUnchecked(*entry);
            Symbol const* const symbolPtr = symbol.ptr();
            if (eq(symbolPtr->hash.oref(), hash.oref())
                && strEq(symbol.ptr()->name(), name)
            ) {
                return IndexOfRes{i, true};
            }
        }
    }
}

HRef<Symbol> SymbolTable::atIndexUnchecked(size_t i) const {
    return HRef<Symbol>::fromUnchecked(entries[i]);;
}

HRef<Symbol> SymbolTable::createAtUnchecked(State* state, size_t i, Fixnum hash, Str name) {
    size_t const newCount = count + 1;
    size_t const capacity = cap;
    if (capacity / 2 < newCount) {
        rehash();
        i = indexOf(hash, name).index;
    }

    HRef<Symbol> const symbol = createUninternedSymbol(state, hash, name);
    entries[i] = symbol.oref();
    count = newCount;
    return symbol;
}

// TODO: DRY wrt. `SymbolTable::createAtUnchecked`
HRef<Symbol> SymbolTable::createFromHeapedAtUnchecked(
    State* state, size_t i, Fixnum hash, HRef<String> name
) {
    size_t const newCount = count + 1;
    size_t const capacity = cap;
    if (capacity / 2 < newCount) {
        rehash();
        i = indexOf(hash, name.ptr()->str()).index;
    }

    HRef<Symbol> const symbol = createUninternedSymbolFromHeaped(state, hash, name);
    entries[i] = symbol.oref();
    count = newCount;
    return symbol;
}

// OPTMIZE: Do not grow if load factor is largely due to tombstones:
void SymbolTable::rehash() {
    size_t const oldCap = cap;
    size_t const newCap = oldCap * 2;
    ORef* const newEntries = (ORef*)calloc(newCap, sizeof *newEntries);
    size_t newCount = 0;

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = entries[i];
        if (isHeaped(v)) {
            size_t const h = (uintptr_t)HRef<Symbol>::fromUnchecked(v).ptr()->hash.val();

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                 ++collisions, j = (j + collisions) & maxIndex
                 ) {
                ORef* const entry = newEntries + j;
                if (eq(*entry, Default)) {
                    *entry = v;
                    ++newCount;
                    break;
                }
            }
        }
    }

    free(entries);
    entries = newEntries;
    count = newCount;
    cap = newCap;
}

// `name` must not point into GC heap:
HRef<Symbol> intern(State* state, Str name) {
    Fixnum const hash = hashStr(name);

    SymbolTable::IndexOfRes ires = state->symbols.indexOf(hash, name);
    if (ires.exists) {
        return state->symbols.atIndexUnchecked(ires.index);
    } else {
        return state->symbols.createAtUnchecked(state, ires.index, hash, name);
    }
}

// TODO: DRY wrt. `intern`
HRef<Symbol> internHeaped(State* state, HRef<String> name) {
    Str const nameStr = name.ptr()->str();
    Fixnum const hash = hashStr(nameStr);

    SymbolTable::IndexOfRes ires = state->symbols.indexOf(hash, nameStr);
    if (ires.exists) {
        return state->symbols.atIndexUnchecked(ires.index);
    } else {
        return state->symbols.createFromHeapedAtUnchecked(state, ires.index, hash, name);
    }
}

// Specializations
// =================================================================================================

Specializations::Specializations() { entries = (ORef*)calloc(cap, sizeof *entries); }

Specializations::~Specializations() { free(entries); }

void Specializations::prune() {
    for (size_t i = 0; i < cap; ++i) {
        ORef* const v = &entries[i];
        if (isHeaped(*v)) {
            Object* const fwdPtr = HRef<Object>::fromUnchecked(*v).ptr()->tryForwarded();
            *v = fwdPtr ? HRef<Object>(fwdPtr).oref() : Tombstone;
        }
    }
}

// Is `method` the specialization of `generic` with `types`?
bool isSpecialized(HRef<Method> methodRef, HRef<Method> genericRef, HRef<ArrayMut> typesRef) {
    Method const* const method = methodRef.ptr();
    Method const* const generic = genericRef.ptr();

    if (!eq(method->code, generic->code)) { return false; }

    ORef const* const types = typesRef.ptr()->flexData();
    size_t const arity = (uint64_t)generic->flexCount().val();
    for (size_t i = 0, typeIdx = 0; i < arity; ++i) {
        ORef const maybeType = generic->domain()[i];
        if (!isHeaped(maybeType)) {
            ORef const replacement = types[typeIdx++];
            if (!eq(method->domain()[i], replacement)) { return false; }
        }
    }

    return true;
}

Fixnum hashSpecialization(HRef<Method> generic, HRef<ArrayMut> typesRef) {
    uintptr_t hash = (uintptr_t)generic.ptr()->hash.val();

    Slice<ORef const> const typesSlice = typesRef.ptr()->items();
    size_t const typeCount = typesSlice.count;
    for (size_t i = 0; i < typeCount; ++i) {
        Type const* const type = HRef<Type>::fromUnchecked(typesSlice[i]).ptr();
        hash = hashCombine(hash, (uintptr_t)type->hash.val());
    }

    return Fixnum((intptr_t)hash);
}

Fixnum hashSpecialized(HRef<Method> specialization) { return specialization.ptr()->hash; }

HRef<Method> createSpecialization(
    State* state, HRef<Method> genericRef, HRef<ArrayMut> typesRef, Fixnum hash
) {
    Method const* generic = genericRef.ptr();
    Fixnum const fxArity = generic->flexCount();
    auto const genericRefG = state->pushRoot(&genericRef);
    auto const typesRefG = state->pushRoot(&typesRef);
    HRef<Method> const specializationRef =
        allocBytecodeMethod(state, HRef<ByteArray>::fromUnchecked(generic->code),
                            HRef<ArrayMut>::fromUnchecked(generic->consts), fxArity,
                            generic->hasVarArg, hash, generic->maybeName, generic->maybeFilenames,
                            generic->maybeSrcByteIdxs);
    Method* const specialization = specializationRef.ptr();

    generic = genericRef.ptr(); // Reload after potential GC
    ORef const* const types = typesRef.ptr()->flexData();
    size_t const arity = (uintptr_t)fxArity.val();
    for (size_t i = 0, typeIdx = 0; i < arity; ++i) {
        ORef const maybeType = generic->domain()[i];
        specialization->domain()[i] = isHeaped(maybeType) ? maybeType : types[typeIdx++];
    }

    return specializationRef;
}

Specializations::IndexOfRes Specializations::indexOf(
    uintptr_t h, HRef<Method> generic, HRef<ArrayMut> types
) const {
    size_t const maxIndex = cap - 1;
    for (size_t collisions = 0, i = h & maxIndex;; ++collisions, i = (i + collisions) & maxIndex) {
        ORef* const entry = entries + i;

        if (eq(*entry, Default)) { return IndexOfRes{i, false}; }

        if (isHeaped(*entry)) {
            HRef<Method> const methodRef = HRef<Method>::fromUnchecked(*entry);

            if (isSpecialized(methodRef, generic, types)) { return IndexOfRes{i, true}; }
        }
    }
}

HRef<Method> Specializations::atIndexUnchecked(size_t i) const {
    return HRef<Method>::fromUnchecked(entries[i]);
}

HRef<Method> Specializations::createAtUnchecked(
    State* state, size_t i, Fixnum fxHash, HRef<Method> generic, HRef<ArrayMut> types
) {
    size_t const newCount = count + 1;
    size_t const capacity = cap;
    if (capacity / 2 < newCount) {
        rehash();
        i = indexOf(uint64_t(fxHash.val()), generic, types).index;
    }

    HRef<Method> const specialization = createSpecialization(state, generic, types, fxHash);
    entries[i] = specialization.oref();
    count = newCount;
    return specialization;
}

// OPTMIZE: Do not grow if load factor is largely due to tombstones:
void Specializations::rehash() {
    size_t const oldCap = cap;
    size_t const newCap = oldCap * 2;
    ORef* const newEntries = (ORef*)calloc(newCap, sizeof *newEntries);
    size_t newCount = 0;

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = entries[i];
        if (isHeaped(v)) {
            size_t const h = (uintptr_t)hashSpecialized(HRef<Method>::fromUnchecked(v)).val();

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                 ++collisions, j = (j + collisions) & maxIndex
                 ) {
                ORef* const entry = newEntries + j;
                if (eq(*entry, Default)) {
                    *entry = v;
                    ++newCount;
                    break;
                }
            }
        }
    }

    free(entries);
    entries = newEntries;
    count = newCount;
    cap = newCap;
}

HRef<Method> specialize(State* state, HRef<Method> generic, HRef<ArrayMut> types) {
#ifndef NDEBUG
    assert(isHeaped(generic.ptr()->code));

    {
        Slice<ORef const> const typesSlice = types.ptr()->items();
        size_t const typeCount = typesSlice.count;
        for (size_t i = 0; i < typeCount; ++i) {
            assert(isType(state, typesSlice[i]));
        }
    }
#endif

    Fixnum const fxHash = hashSpecialization(generic, types);
    uintptr_t const hash = (uint64_t)fxHash.val();

    Specializations::IndexOfRes ires = state->specializations.indexOf(hash, generic, types);
    if (ires.exists) {
        return state->specializations.atIndexUnchecked(ires.index);
    } else {
        return state->specializations.createAtUnchecked(state, ires.index, fxHash, generic, types);
    }
}

}
