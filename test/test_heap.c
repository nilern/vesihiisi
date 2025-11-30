#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../lib/util.c"
#include "../lib/object.c"
#include "../lib/heap.c"

static void testBootstrap(void) {
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
    
    Type const* const stringTypePtr = tryCreateStringType(&heap.tospace, typeTypePtr);
    assert(stringTypePtr != nullptr);
    ORef const stringType = typeToORef(tagType(stringTypePtr));
    
    assert(eq(typeToORef(typeOf(stringType)), typeType));
    assert(eq(fixnumToORef(stringTypePtr->minSize), fixnumToORef(tagInt(0))));
    assert(eq(fixnumToORef(stringTypePtr->align), fixnumToORef(tagInt((intptr_t)objectMinAlign))));
    assert(unwrapBool(stringTypePtr->isBytes));
    assert(unwrapBool(stringTypePtr->isFlex));

    freeHeap(&heap);
}

static void testIntern(void) {
    Heap heap = tryCreateHeap(1024);
    assert(heapIsValid(&heap));
    Type const* const typeType = tryCreateTypeType(&heap.tospace);
    assert(typeType != nullptr);
    Type const* symbolType = tryCreateSymbolType(&heap.tospace, typeType);
    assert(symbolType != nullptr);
    Type const* arrayType = tryCreateArrayType(&heap.tospace, typeType);
    assert(arrayType != nullptr);
    SymbolTable symbols = createSymbolTable(&heap, arrayType);

    char const nameChars[] = "foo";
    Str const name = {nameChars, sizeof nameChars - 1};
    SymbolRef const sym = intern(&heap, arrayType, symbolType, &symbols, name);
    Symbol const* symPtr = symbolToPtr(sym);
    
    assert(eq(typeToORef(typeOf(symbolToORef(sym))), typeToORef(tagType(symbolType))));
    assert(eq(fixnumToORef(symPtr->hash), fixnumToORef(tagInt((intptr_t)fnv1aHash(name)))));
    assert(strEq(symbolName(sym), name));
    
    SymbolRef const dupSym = intern(&heap, arrayType, symbolType, &symbols, name);
    
    assert(eq(symbolToORef(dupSym), symbolToORef(sym)));

    freeHeap(&heap);
}

int main(int /*argc*/, char** /*argv*/) {
    testBootstrap();
    testIntern();
    
    return EXIT_SUCCESS;
}

