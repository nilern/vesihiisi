#pragma once

#include "object.h"

typedef struct Semispace {
    char* free;
    char* limit;
    char* start;
} Semispace;

inline static bool allocatedInSemispace(Semispace const* space, void const* ptr) {
    return space->start <= (char const*)ptr && (char const*)ptr < space->free;
}

static void refurbishSemispace(Semispace* space, Semispace const* other);

typedef struct Heap {
    Semispace tospace;
    Semispace fromspace;
} Heap;

static Heap tryCreateHeap(size_t size);

static bool heapIsValid(Heap const* heap);

static void freeHeap(Heap* heap);

[[nodiscard]]
static void* tryAlloc(Semispace* semispace, Type const* type);

[[nodiscard]]
static void* allocOrDie(Semispace* semispace, Type const* type);

[[nodiscard]]
static void* tryAllocFlex(Semispace* semispace, Type const* type, Fixnum length);

[[nodiscard]]
static void* allocFlexOrDie(Semispace* semispace, Type const* type, Fixnum length);

inline static bool mustCollect(void const* ptr) {
#ifdef GC_ALOT
    return !ptr || true;
#else
    return !ptr;
#endif
}

inline static void flipSemispaces(Heap* heap) {
    Semispace const tmp = heap->tospace;
    heap->tospace = heap->fromspace;
    heap->fromspace = tmp;
}

[[nodiscard]]
static ORef mark(Heap* heap, ORef oref);

static void collectHeap(Heap* heap);
