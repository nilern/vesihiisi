#include "flyweights.h"

static Specializations newSpecializations(void) {
    size_t const cap = 2;
    ORef* const entries = calloc(cap, sizeof *entries);
    return (Specializations){.entries = entries, .count = 0, .cap = cap};
}

static void freeSpecializations(Specializations* specializations) {
    free(specializations->entries);
}

// Is `method` the specialization of `generic` with `types`?
static bool isSpecialized(MethodRef methodRef, MethodRef genericRef, ArrayRef typesRef) {
    Method const* const method = toPtr(methodRef);
    Method const* const generic = toPtr(genericRef);

    if (!eq(method->code, generic->code)) { return false; }

    ORef const* const types = toPtr(typesRef);
    size_t const arity = (uintptr_t)fixnumToInt(flexLength(toORef(genericRef)));
    for (size_t i = 0, typeIdx = 0; i < arity; ++i) {
        ORef const maybeType = generic->domain[i];
        if (!isHeaped(maybeType)) {
            ORef const replacement = types[typeIdx++];
            if (!eq(method->domain[i], replacement)) { return false; }
        }
    }

    return true;
}

static Fixnum hashSpecialization(MethodRef generic, ArrayRef typesRef) {
    uintptr_t hash = (uintptr_t)fixnumToInt(toPtr(generic)->hash);

    ORef const* const types = toPtr(typesRef);
    size_t const typeCount = (uintptr_t)fixnumToInt(flexLength(toORef(typesRef)));
    for (size_t i = 0; i < typeCount; ++i) {
        Type const* const type = toPtr(uncheckedORefToTypeRef(types[i]));
        hash = hashCombine(hash, (uintptr_t)fixnumToInt(type->hash));
    }

    return tagInt((intptr_t)hash);
}

static Fixnum hashSpecialized(MethodRef specialization) { return toPtr(specialization)->hash; }

static MethodRef createSpecialization(
    State* state, MethodRef genericRef, ArrayRef typesRef, Fixnum hash
) {
    Method const* generic = toPtr(genericRef);
    Fixnum const fxArity = flexLength(toORef(genericRef));
    pushStackRoot(state, (ORef*)&genericRef);
    pushStackRoot(state, (ORef*)&typesRef);
    MethodRef const specializationRef =
        allocBytecodeMethod(state, uncheckedORefToByteArray(generic->code),
                            uncheckedORefToArray(generic->consts), fxArity, generic->hasVarArg,
                            hash);
    popStackRoots(state, 2);
    Method* const specialization = toPtr(specializationRef);

    generic = toPtr(genericRef); // Reload after potential GC
    ORef const* const types = toPtr(typesRef);
    size_t const arity = (uintptr_t)fixnumToInt(fxArity);
    for (size_t i = 0, typeIdx = 0; i < arity; ++i) {
        ORef const maybeType = generic->domain[i];
        specialization->domain[i] = isHeaped(maybeType) ? maybeType : types[typeIdx++];
    }

    return specializationRef;
}

// TODO: DRY wrt. `IndexOfSymbolRes`:
typedef struct IndexOfSpecializationRes {
    size_t index;
    bool exists;
} IndexOfSpecializationRes;

static IndexOfSpecializationRes indexOfSpecialization(
    Specializations const* specializations, uintptr_t h, MethodRef generic, ArrayRef types
) {
    size_t const maxIndex = specializations->cap - 1;
    for (size_t collisions = 0, i = h & maxIndex;; ++collisions, i = (i + collisions) & maxIndex) {
        ORef* const entry = specializations->entries + i;

        if (eq(*entry, fixnumToORef(Zero))) { return (IndexOfSpecializationRes){i, false}; }

        if (isHeaped(*entry)) {
            MethodRef const methodRef = uncheckedORefToMethod(*entry);

            if (isSpecialized(methodRef, generic, types)) {
                return (IndexOfSpecializationRes){i, true};
            }
        }
    }
}

// OPTMIZE: Do not grow if load factor is largely due to tombstones:
static void rehashSpecializations(Specializations* specializations) {
    size_t const oldCap = specializations->cap;
    size_t const newCap = oldCap * 2;
    ORef* const newEntries = calloc(newCap, sizeof *newEntries);
    size_t newCount = 0;

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const v = specializations->entries[i];
        if (isHeaped(v)) {
            size_t const h = (uintptr_t)fixnumToInt(hashSpecialized(uncheckedORefToMethod(v)));

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                 ++collisions, j = (j + collisions) & maxIndex
                 ) {
                ORef* const entry = newEntries + j;
                if (eq(*entry, fixnumToORef(Zero))) {
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

static MethodRef specialize(State* state, MethodRef generic, ArrayRef types) {
#ifndef NDEBUG
    assert(isHeaped(toPtr(generic)->code));

    {
        size_t const typeCount = (uintptr_t)fixnumToInt(flexLength(toORef(types)));
        for (size_t i = 0; i < typeCount; ++i) {
            assert(isType(state, toPtr(types)[i]));
        }
    }
#endif

    Fixnum const fxHash = hashSpecialization(generic, types);
    uintptr_t const hash = (uintptr_t)fixnumToInt(fxHash);

    IndexOfSpecializationRes ires =
        indexOfSpecialization(&state->specializations, hash, generic, types);
    if (ires.exists) {
        return uncheckedORefToMethod(state->specializations.entries[ires.index]);;
    } else {
        size_t const newCount = state->specializations.count + 1;
        size_t const capacity = state->specializations.cap;
        if (capacity / 2 < newCount) {
            rehashSpecializations(&state->specializations);
            ires = indexOfSpecialization(&state->specializations, hash, generic, types);
        }

        MethodRef const specialization = createSpecialization(state, generic, types, fxHash);
        state->specializations.entries[ires.index] = toORef(specialization);
        state->specializations.count = newCount;
        return specialization;
    }
}

static void pruneSpecializations(Specializations* specializations) {
    size_t const cap = specializations->cap;
    for (size_t i = 0; i < cap; ++i) {
        ORef* const v = &specializations->entries[i];
        if (isHeaped(*v)) {
            void* const fwdPtr = tryForwarded(uncheckedORefToPtr(*v));
            *v = fwdPtr ? tagHeaped(fwdPtr) : SymbolTableTombstone; // TODO: Not Symbol-specific
        }
    }
}
