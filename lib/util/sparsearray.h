#ifndef T
#error `SparseArray` type argument T missing
#else

#ifndef SUFFIX
#define SUFFIX T
#endif

#include <stdbit.h>

#include "util.h"
#include "arena.h"

#define G(name) CAT(name, SUFFIX)

// Interface
// =================================================================================================

typedef struct {
    size_t preCount;
    size_t presents;
} G(SparseArrayKeyEntry);

typedef struct {
    G(SparseArrayKeyEntry)* keyEntries;
    size_t keyEntryCount;
    size_t keyEntryCap;

    T* vals;
    size_t valCount;
    size_t valCap;

    Arena* arena;
} G(SparseArray);

static G(SparseArray) G(newSparseArray)(Arena* arena, size_t capacity);

typedef struct {
    T const* val;
    bool found;
} G(SparseArrayGot);

static G(SparseArrayGot) G(sparseArrayGet)(G(SparseArray) const* sparr, size_t idx);

static void G(sparseArraySet)(G(SparseArray)* sparr, size_t idx, T val);

static void G(sparseArrayRemove)(G(SparseArray)* sparr, size_t idx);

// Iterators
// -------------------------------------------------------------------------------------------------

// TODO: Use Java-style iterator instead?

typedef struct {
    G(SparseArrayKeyEntry) const* keyEntry;
    G(SparseArrayKeyEntry) const* keyEntriesEnd;
    size_t presenceMask;
    G(SparseArrayKeyEntry) const* keyEntries;
    T const* vals;
} G(SparseArrayIter);

static G(SparseArrayIter) G(sparseArrayBegin)(G(SparseArray) const* sparr);
static G(SparseArrayIter) G(sparseArrayEnd)(G(SparseArray) const* sparr);

static bool G(sparseArrayIterEq)(G(SparseArrayIter) it1, G(SparseArrayIter) it2);

static void G(sparseArrayIterInc)(G(SparseArrayIter)* it);

typedef struct {
    size_t idx;
    T const* val;
} G(SparseArrayEntry);

static G(SparseArrayEntry) G(sparseArrayIterDeref)(G(SparseArrayIter) it);

#ifdef SPARSE_ARRAY_IMPLEMENTATION
// =================================================================================================

static G(SparseArray) G(newSparseArray)(Arena* arena, size_t cap) {
    if (cap < 2) { cap = 2; }
    size_t keyCap = cap / SIZE_WIDTH;
    if (keyCap < 2) { keyCap = 2; }

    G(SparseArrayKeyEntry)* const keyEntries = acalloc(arena, keyCap, sizeof *keyEntries);
    T* const vals = amalloc(arena, cap * sizeof *vals);

    return (G(SparseArray)){
        .keyEntries = keyEntries,
        .keyEntryCount = 0,
        .keyEntryCap = keyCap,

        .vals = vals,
        .valCount = 0,
        .valCap = cap,

        .arena = arena
    };
}

typedef struct {
    size_t idx;
    bool found;
} G(SparseArrayMaybeValIdx);

// OPTIMIZE: This does not depend on `T` and so is needlessly monomorphized:
static G(SparseArrayMaybeValIdx) G(sparseArrayValIdx)(G(SparseArray) const* sparr, size_t idx) {
    size_t const keyEntryIdx = idx / SIZE_WIDTH;
    if (keyEntryIdx >= sparr->keyEntryCount) { return (G(SparseArrayMaybeValIdx)){}; }
    G(SparseArrayKeyEntry) const keyEntry = sparr->keyEntries[keyEntryIdx];

    size_t const subIdx = idx % SIZE_WIDTH;
    size_t const presenceMask = (size_t)1 << subIdx;
    size_t const isPresent = keyEntry.presents & presenceMask;
    if (!isPresent) { return (G(SparseArrayMaybeValIdx)){}; }

    size_t const subPreCountMask = presenceMask - (size_t)1;
    size_t const subPreCount = stdc_count_ones(keyEntry.presents & subPreCountMask);
    size_t const valIdx = keyEntry.preCount + subPreCount;

    return (G(SparseArrayMaybeValIdx)){.idx = valIdx, .found = true};
}

static G(SparseArrayGot) G(sparseArrayGet)(G(SparseArray) const* sparr, size_t idx) {
    G(SparseArrayMaybeValIdx) const maybeValIdx = G(sparseArrayValIdx)(sparr, idx);
    return maybeValIdx.found
        ? (G(SparseArrayGot)){.val = sparr->vals + maybeValIdx.idx, .found = true}
        : (G(SparseArrayGot)){};
}

// OPTIMIZE: Most of this does not depend on `T` and so is needlessly monomorphized:
static void G(sparseArraySet)(G(SparseArray)* sparr, size_t idx, T val) {
    size_t const keyEntryIdx = idx / SIZE_WIDTH;

    if (keyEntryIdx >= sparr->keyEntryCap) { // Need to grow `.keyEntries`:
        size_t newKeyCap = sparr->keyEntryCap + sparr->keyEntryCap / 2;
        if (newKeyCap < keyEntryIdx + 1) { newKeyCap = keyEntryIdx + 1; }
        G(SparseArrayKeyEntry)* const newKeyEntries =
            amalloc(sparr->arena, newKeyCap * sizeof *newKeyEntries);

        memcpy(newKeyEntries, sparr->keyEntries, sparr->keyEntryCount * sizeof *newKeyEntries);

        sparr->keyEntries = newKeyEntries;
        sparr->keyEntryCap = newKeyCap;
    }

    if (keyEntryIdx >= sparr->keyEntryCount) { // In unitialized `.keyEntries` => init up to:
        size_t const newKeyCount = keyEntryIdx + 1;
        size_t tailPreCount = 0;
        if (sparr->keyEntryCount > 0) {
            G(SparseArrayKeyEntry) const lastKeyEntry = sparr->keyEntries[sparr->keyEntryCount - 1];
            size_t const lastPreCount = lastKeyEntry.preCount;
            tailPreCount = lastPreCount + stdc_count_ones(lastKeyEntry.presents);
        }

        for (size_t i = sparr->keyEntryCount; i < newKeyCount; ++i) {
            sparr->keyEntries[i] = (G(SparseArrayKeyEntry)){
                .preCount = tailPreCount,
                .presents = 0
            };
        }

        sparr->keyEntryCount = newKeyCount;
    }

    G(SparseArrayKeyEntry)* const keyEntry = &sparr->keyEntries[keyEntryIdx];

    size_t const subIdx = idx % SIZE_WIDTH;
    size_t const presenceMask = (size_t)1 << subIdx;
    size_t const isPresent = keyEntry->presents & presenceMask;

    size_t const subPreCountMask = presenceMask - (size_t)1;
    size_t const subPreCount = stdc_count_ones(keyEntry->presents & subPreCountMask);
    size_t const valIdx = keyEntry->preCount + subPreCount;

    if (isPresent) { // Just replace previous value:
        sparr->vals[valIdx] = val;
        return;
    }

    keyEntry->presents |= presenceMask; // Mark present

    { // Update successor `.preCount`s:
        size_t const keyEntryCount = sparr->keyEntryCount;
        for (size_t i = keyEntryIdx + 1; i < keyEntryCount; ++i) {
            ++sparr->keyEntries[i].preCount;
        }
    }

    if (sparr->valCount == sparr->valCap) { // Need to grow `.vals`:
        size_t const newValCap = sparr->valCap + sparr->valCap / 2;
        sparr->vals = arealloc(sparr->arena, sparr->vals,
                               sparr->valCap * sizeof *sparr->vals,
                               newValCap * sizeof *sparr->vals);
        sparr->valCap = newValCap;
    }

    // Insert `val` into `.vals`:
    T* const begin = sparr->vals + valIdx;
    memmove(begin + 1, begin, sparr->valCount - valIdx);
    sparr->vals[valIdx] = val;
    ++sparr->valCount;
}

// OPTIMIZE: Most of this does not depend on `T` and so is needlessly monomorphized:
static void G(sparseArrayRemove)(G(SparseArray)* sparr, size_t idx) {
    size_t const keyEntryIdx = idx / SIZE_WIDTH;
    if (keyEntryIdx >= sparr->keyEntryCount) { return; } // Was never set to begin with
    G(SparseArrayKeyEntry)* const keyEntry = &sparr->keyEntries[keyEntryIdx];

    size_t const subIdx = idx % SIZE_WIDTH;
    size_t const presenceMask = (size_t)1 << subIdx;
    size_t const isPresent = keyEntry->presents & presenceMask;

    if (!isPresent) { return; } // Was never set to begin with

    size_t const subPreCountMask = presenceMask - (size_t)1;
    size_t const subPreCount = stdc_count_ones(keyEntry->presents & subPreCountMask);
    size_t const valIdx = keyEntry->preCount + subPreCount;

    keyEntry->presents &= ~presenceMask; // Mark not present

    { // Update successor `.preCount`s:
        size_t const keyEntryCount = sparr->keyEntryCount;
        for (size_t i = keyEntryIdx + 1; i < keyEntryCount; ++i) {
            --sparr->keyEntries[i].preCount;
        }
    }

    // Remove from `.vals`:
    T* const begin = sparr->vals + valIdx;
    memmove(begin, begin + 1, sparr->valCount - (valIdx + 1));
    --sparr->valCount;
}

// Iterators
// -------------------------------------------------------------------------------------------------

// OPTIMIZE: This does not depend on `T` and so is needlessly monomorphized:
static void G(sparseArrayIterInc)(G(SparseArrayIter)* it) {
    while (it->keyEntry != it->keyEntriesEnd) {
        if (it->presenceMask < (size_t)1 << (SIZE_WIDTH - 1)) {
            it->presenceMask <<= (size_t)1;
        } else {
            ++it->keyEntry;
            it->presenceMask = 1;
        }

        if (it->keyEntry->presents & it->presenceMask) { break; }
    }
}

// OPTIMIZE: This does not depend on `T` and so is needlessly monomorphized:
static G(SparseArrayIter) G(sparseArrayBegin)(G(SparseArray) const* sparr) {
    G(SparseArrayIter) it = (G(SparseArrayIter)){
        .keyEntry = sparr->keyEntries,
        .keyEntriesEnd = sparr->keyEntries + sparr->keyEntryCount,
        .presenceMask = 1,
        .keyEntries = sparr->keyEntries,
        .vals = sparr->vals
    };

    if (it.keyEntry == it.keyEntriesEnd) { return it; }

    if (!(it.keyEntry->presents & it.presenceMask)) { G(sparseArrayIterInc)(&it); }

    return it;
}

// OPTIMIZE: This does not depend on `T` and so is needlessly monomorphized:
static G(SparseArrayIter) G(sparseArrayEnd)(G(SparseArray) const* sparr) {
    return (G(SparseArrayIter)){
        .keyEntry = sparr->keyEntries + sparr->keyEntryCount,
        .keyEntriesEnd = sparr->keyEntries + sparr->keyEntryCount,
        .presenceMask = 1,
        .keyEntries = sparr->keyEntries,
        .vals = sparr->vals
    };
}

// OPTIMIZE: This does not depend on `T` and so is needlessly monomorphized:
static bool G(sparseArrayIterEq)(G(SparseArrayIter) it1, G(SparseArrayIter) it2) {
    assert(it1.keyEntriesEnd == it2.keyEntriesEnd
           && it1.keyEntries == it2.keyEntries
           && it1.vals == it2.vals);

    return it1.keyEntry == it2.keyEntry
           && it1.presenceMask == it2.presenceMask;
}

// OPTIMIZE: This does not depend on `T` and so is needlessly monomorphized:
static G(SparseArrayEntry) G(sparseArrayIterDeref)(G(SparseArrayIter) it) {
    size_t const subPreCountMask = it.presenceMask - (size_t)1;
    size_t const subPreCount = stdc_count_ones(it.keyEntry->presents & subPreCountMask);
    size_t const valIdx = it.keyEntry->preCount + subPreCount;

    assert(it.keyEntry >= it.keyEntries);
    return (G(SparseArrayEntry)){
        .idx = (size_t)(it.keyEntry - it.keyEntries),
        .val = it.vals + valIdx
    };
}

#endif // SPARSE_ARRAY_IMPLEMENTATION

#undef G
#undef SUFFIX
#undef T
#endif
