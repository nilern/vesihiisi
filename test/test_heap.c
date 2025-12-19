#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "../lib/util.c"
#include "../lib/object.c"
#include "../lib/heap.c"
#include "../lib/state.c"
#include "../lib/namespace.c"
#include "../lib/print.c"
#include "../lib/bytecode.c"
#include "../lib/primops.c"
// To avoid linker error for `collectTracingIR`:
#include "../lib/arena.c"
#include "../lib/bitset.c"
#include "../lib/compiler.c"

static void testBootstrap(void) {
    State state;
    assert(tryCreateState(&state, 1024*1024));
    
    TypeRef const typeType = state.typeType;
    Type const* typeTypePtr = typeToPtr(typeType);
    assert(eq(typeToORef(typeOf(&state, typeToORef(typeType))), typeToORef(typeType)));
    assert(eq(fixnumToORef(typeTypePtr->minSize), fixnumToORef(tagInt(sizeof(Type)))));
    assert(eq(fixnumToORef(typeTypePtr->align), fixnumToORef(tagInt(alignof(Type)))));
    assert(!unwrapBool(typeTypePtr->isBytes));
    assert(!unwrapBool(typeTypePtr->isFlex));
    
    TypeRef const stringType = state.stringType;
    Type const* stringTypePtr = typeToPtr(stringType);
    assert(eq(typeToORef(typeOf(&state, typeToORef(stringType))), typeToORef(typeType)));
    assert(eq(fixnumToORef(stringTypePtr->minSize), fixnumToORef(tagInt(0))));
    assert(eq(fixnumToORef(stringTypePtr->align), fixnumToORef(tagInt((intptr_t)objectMinAlign))));
    assert(unwrapBool(stringTypePtr->isBytes));
    assert(unwrapBool(stringTypePtr->isFlex));

    freeState(&state);
}

static void testIntern(void) {
    State state;
    assert(tryCreateState(&state, 1024*1024));

    char const nameChars[] = "foo";
    Str const name = {nameChars, sizeof nameChars - 1};
    SymbolRef const sym = intern(&state, name);
    Symbol const* symPtr = symbolToPtr(sym);
    
    assert(eq(typeToORef(typeOf(&state, symbolToORef(sym))), typeToORef(state.symbolType)));
    assert(eq(fixnumToORef(symPtr->hash), fixnumToORef(tagInt((intptr_t)fnv1aHash(name)))));
    assert(strEq(symbolName(sym), name));
    
    SymbolRef const dupSym = intern(&state, name);
    
    assert(eq(symbolToORef(dupSym), symbolToORef(sym)));

    freeState(&state);
}

int main(int /*argc*/, char** /*argv*/) {
    testBootstrap();
    testIntern();
    
    return EXIT_SUCCESS;
}
