#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../lib/util.c"
#include "../lib/object.c"
#include "../lib/heap.c"

static void testBootstrap(Heap* heap) {
    Type const* const typeTypePtr = tryCreateTypeType(&heap->tospace);
    assert(typeTypePtr != nullptr);
    ORef const typeType = typeToORef(tagType(typeTypePtr));
    
    assert(eq(typeToORef(typeOf(typeType)), typeType));
    assert(eq(fixnumToORef(typeTypePtr->minSize), fixnumToORef(tagInt(sizeof(Type)))));
    assert(eq(fixnumToORef(typeTypePtr->align), fixnumToORef(tagInt(alignof(Type)))));
    assert(!unwrapBool(typeTypePtr->isBytes));
    assert(!unwrapBool(typeTypePtr->isFlex));
    
    Type const* const stringTypePtr = tryCreateStringType(&heap->tospace, typeTypePtr);
    assert(stringTypePtr != nullptr);
    ORef const stringType = typeToORef(tagType(stringTypePtr));
    
    assert(eq(typeToORef(typeOf(stringType)), typeType));
    assert(eq(fixnumToORef(stringTypePtr->minSize), fixnumToORef(tagInt(0))));
    assert(eq(fixnumToORef(stringTypePtr->align), fixnumToORef(tagInt((intptr_t)objectMinAlign))));
    assert(unwrapBool(stringTypePtr->isBytes));
    assert(unwrapBool(stringTypePtr->isFlex));
}

int main(int /*argc*/, char** /*argv*/) {
    Heap heap = tryCreateHeap(1024);
    assert(heapIsValid(&heap));

    testBootstrap(&heap);

    freeHeap(&heap);
    return EXIT_SUCCESS;
}

