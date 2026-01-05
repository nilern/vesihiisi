#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "../lib/vesihiisi.c"

static void testBootstrap(void) {
    State* const state = tryCreateState(1024*1024);
    assert(state);
    
    TypeRef const typeType = state->typeType;
    Type const* typeTypePtr = typeToPtr(typeType);
    assert(eq(toORef(typeOf(state, toORef(typeType))), toORef(typeType)));
    assert(eq(toORef(typeTypePtr->minSize), toORef(tagInt(sizeof(Type)))));
    assert(eq(toORef(typeTypePtr->align), toORef(tagInt(alignof(Type)))));
    assert(!unwrapBool(typeTypePtr->isBytes));
    assert(!unwrapBool(typeTypePtr->isFlex));
    
    TypeRef const stringType = state->stringType;
    Type const* stringTypePtr = typeToPtr(stringType);
    assert(eq(toORef(typeOf(state, toORef(stringType))), toORef(typeType)));
    assert(eq(toORef(stringTypePtr->minSize), toORef(tagInt(0))));
    assert(eq(toORef(stringTypePtr->align), toORef(tagInt((intptr_t)objectMinAlign))));
    assert(unwrapBool(stringTypePtr->isBytes));
    assert(unwrapBool(stringTypePtr->isFlex));

    freeState(state);
}

static void testIntern(void) {
    State* const state = tryCreateState(1024*1024);
    assert(state);

    char const nameChars[] = "foo";
    Str const name = {nameChars, sizeof nameChars - 1};
    SymbolRef const sym = intern(state, name);
    Symbol const* symPtr = symbolToPtr(sym);
    
    assert(eq(toORef(typeOf(state, toORef(sym))), toORef(state->symbolType)));
    assert(eq(toORef(symPtr->hash), toORef(tagInt((intptr_t)fnv1aHash(name)))));
    assert(strEq(symbolName(sym), name));
    
    SymbolRef const dupSym = intern(state, name);
    
    assert(eq(toORef(dupSym), toORef(sym)));

    freeState(state);
}

int main(int /*argc*/, char** /*argv*/) {
    testBootstrap();
    testIntern();
    
    return EXIT_SUCCESS;
}
