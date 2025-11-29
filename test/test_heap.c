#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../lib/object.c"
#include "../lib/heap.c"

int main(int /*argc*/, char** /*argv*/) {
    Heap heap = tryCreateHeap(1024);
    assert(heapIsValid(&heap));

    Type const* const typeTypePtr = tryCreateTypeType(&heap.tospace);
    assert(typeTypePtr != nullptr);
    ORef const typeType = typeToORef(tagType(typeTypePtr));
    assert(eq(typeToORef(typeOf(typeType)), typeType));
    assert(eq(fixnumToORef(typeTypePtr->minSize), fixnumToORef(tagInt(sizeof(Type)))));
    assert(eq(fixnumToORef(typeTypePtr->align), fixnumToORef(tagInt(alignof(Type)))));
    assert(!unwrapBool(typeTypePtr->isBytes));
    assert(!unwrapBool(typeTypePtr->isFlex));

    freeHeap(&heap);
    return EXIT_SUCCESS;
}

