#include "flyweights.hpp"

#include <stdlib.h>

#include "util/util.hpp"

namespace {

// Symbols
// =================================================================================================

void freeSymbols(SymbolTable* symbols) { free(symbols->entries); }

SymbolTable newSymbolTable() {
    size_t const cap = 2;
    ORef* const entries = (ORef*)calloc(cap, sizeof *entries);
    return SymbolTable{.entries = entries, .count = 0, .cap = cap};
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

typedef struct IndexOfSymbolRes {
    size_t index;
    bool exists;
} IndexOfSymbolRes;

IndexOfSymbolRes indexOfSymbol(SymbolTable const* symbols, Fixnum hash, Str name) {
    uintptr_t const h = (uintptr_t)hash.val();

    size_t const maxIndex = symbols->cap - 1;
    for (size_t collisions = 0, i = h & maxIndex;; ++collisions, i = (i + collisions) & maxIndex) {
        ORef* const entry = symbols->entries + i;

        if (eq(*entry, Default)) { return IndexOfSymbolRes{i, false}; }

        if (isHeaped(*entry)) {
            HRef<Symbol> const symbol = HRef<Symbol>::fromUnchecked(*entry);
            Symbol const* const symbolPtr = symbol.ptr();
            if (eq(symbolPtr->hash.oref(), hash.oref())
                && strEq(symbol.ptr()->name(), name)
                ) {
                return IndexOfSymbolRes{i, true};
            }
        }
    }
}

// OPTMIZE: Do not grow if load factor is largely due to tombstones:
void rehashSymbols(State* state) {
    size_t const oldCap = state->symbols.cap;
    size_t const newCap = oldCap * 2;
    ORef* const newEntries = (ORef*)calloc(newCap, sizeof *newEntries);
    size_t newCount = 0;

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = state->symbols.entries[i];
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

    free(state->symbols.entries);
    state->symbols.entries = newEntries;
    state->symbols.count = newCount;
    state->symbols.cap = newCap;
}

// `name` must not point into GC heap:
HRef<Symbol> intern(State* state, Str name) {
    Fixnum const hash = hashStr(name);

    IndexOfSymbolRes ires = indexOfSymbol(&state->symbols, hash, name);
    if (ires.exists) {
        return HRef<Symbol>::fromUnchecked(state->symbols.entries[ires.index]);;
    } else {
        size_t const newCount = state->symbols.count + 1;
        size_t const capacity = state->symbols.cap;
        if (capacity / 2 < newCount) {
            rehashSymbols(state);
            ires = indexOfSymbol(&state->symbols, hash, name);
        }

        HRef<Symbol> const symbol = createUninternedSymbol(state, hash, name);
        state->symbols.entries[ires.index] = symbol.oref();
        state->symbols.count = newCount;
        return symbol;
    }
}

// TODO: DRY wrt. `intern`
HRef<Symbol> internHeaped(State* state, HRef<String> name) {
    Str const nameStr = name.ptr()->str();
    Fixnum const hash = hashStr(nameStr);

    IndexOfSymbolRes ires = indexOfSymbol(&state->symbols, hash, nameStr);
    if (ires.exists) {
        return HRef<Symbol>::fromUnchecked(state->symbols.entries[ires.index]);;
    } else {
        size_t const newCount = state->symbols.count + 1;
        size_t const capacity = state->symbols.cap;
        if (capacity / 2 < newCount) {
            rehashSymbols(state);
            ires = indexOfSymbol(&state->symbols, hash, nameStr);
        }

        HRef<Symbol> const symbol = createUninternedSymbolFromHeaped(state, hash, name);
        state->symbols.entries[ires.index] = symbol.oref();
        state->symbols.count = newCount;
        return symbol;
    }
}

void pruneSymbols(SymbolTable* symbols) {
    size_t const cap = symbols->cap;
    for (size_t i = 0; i < cap; ++i) {
        ORef* const v = &symbols->entries[i];
        if (isHeaped(*v)) {
            Object* const fwdPtr = uncheckedORefToPtr(*v)->tryForwarded();
            *v = fwdPtr ? HRef(fwdPtr).oref() : Tombstone;
        }
    }
}

// Specializations
// =================================================================================================

Specializations newSpecializations() {
    size_t const cap = 2;
    ORef* const entries = (ORef*)calloc(cap, sizeof *entries);
    return Specializations{.entries = entries, .count = 0, .cap = cap};
}

void freeSpecializations(Specializations* specializations) {
    free(specializations->entries);
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

// TODO: DRY wrt. `IndexOfSymbolRes`:
typedef struct IndexOfSpecializationRes {
    size_t index;
    bool exists;
} IndexOfSpecializationRes;

IndexOfSpecializationRes indexOfSpecialization(
    Specializations const* specializations, uintptr_t h, HRef<Method> generic, HRef<ArrayMut> types
) {
    size_t const maxIndex = specializations->cap - 1;
    for (size_t collisions = 0, i = h & maxIndex;; ++collisions, i = (i + collisions) & maxIndex) {
        ORef* const entry = specializations->entries + i;

        if (eq(*entry, Default)) { return IndexOfSpecializationRes{i, false}; }

        if (isHeaped(*entry)) {
            HRef<Method> const methodRef = HRef<Method>::fromUnchecked(*entry);

            if (isSpecialized(methodRef, generic, types)) {
                return IndexOfSpecializationRes{i, true};
            }
        }
    }
}

// OPTMIZE: Do not grow if load factor is largely due to tombstones:
void rehashSpecializations(Specializations* specializations) {
    size_t const oldCap = specializations->cap;
    size_t const newCap = oldCap * 2;
    ORef* const newEntries = (ORef*)calloc(newCap, sizeof *newEntries);
    size_t newCount = 0;

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = specializations->entries[i];
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

    free(specializations->entries);
    specializations->entries = newEntries;
    specializations->count = newCount;
    specializations->cap = newCap;
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

    IndexOfSpecializationRes ires =
        indexOfSpecialization(&state->specializations, hash, generic, types);
    if (ires.exists) {
        return HRef<Method>::fromUnchecked(state->specializations.entries[ires.index]);;
    } else {
        size_t const newCount = state->specializations.count + 1;
        size_t const capacity = state->specializations.cap;
        if (capacity / 2 < newCount) {
            rehashSpecializations(&state->specializations);
            ires = indexOfSpecialization(&state->specializations, hash, generic, types);
        }

        HRef<Method> const specialization =
            createSpecialization(state, generic, types, fxHash);
        state->specializations.entries[ires.index] = specialization.oref();
        state->specializations.count = newCount;
        return specialization;
    }
}

void pruneSpecializations(Specializations* specializations) {
    size_t const cap = specializations->cap;
    for (size_t i = 0; i < cap; ++i) {
        ORef* const v = &specializations->entries[i];
        if (isHeaped(*v)) {
            Object* const fwdPtr = HRef<Object>::fromUnchecked(*v).ptr()->tryForwarded();
            *v = fwdPtr ? HRef<Object>(fwdPtr).oref() : Tombstone;
        }
    }
}

}
