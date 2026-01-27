#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "../lib/vesihiisi.cpp"

static void testBootstrap(void) {
    State* const state = State::tryCreate(1024*1024);
    assert(state);
    
    HRef<Type> const typeType = state->types.type;
    Type const* typeTypePtr = typeType.ptr();
    assert(eq(typeOf(state, typeType), typeType));
    assert(eq(typeTypePtr->minSize, Fixnum((int64_t)sizeof(Type))));
    assert(eq(typeTypePtr->align, Fixnum((int64_t)alignof(Type))));
    assert(!typeTypePtr->isBytes.val());
    assert(!typeTypePtr->isFlex.val());
    
    HRef<Type> const stringType = state->types.string;
    Type const* stringTypePtr = stringType.ptr();
    assert(eq(typeOf(state, stringType), typeType));
    assert(eq(stringTypePtr->minSize, Fixnum(0l)));
    assert(eq(stringTypePtr->align, Fixnum((int64_t)objectMinAlign)));
    assert(stringTypePtr->isBytes.val());
    assert(stringTypePtr->isFlex.val());

    freeState(state);
}

static void testIntern(void) {
    State* const state = State::tryCreate(1024*1024);
    assert(state);

    char const nameChars[] = "foo";
    Str const name = {nameChars, sizeof nameChars - 1};
    HRef<Symbol> const sym = intern(state, name);
    Symbol const* symPtr = sym.ptr();
    
    assert(eq(typeOf(state, sym), state->types.symbol));
    assert(eq(symPtr->hash, Fixnum((intptr_t)fnv1aHash(name))));
    assert(strEq(sym->name(), name));
    
    HRef<Symbol> const dupSym = intern(state, name);
    
    assert(eq(dupSym, sym));

    freeState(state);
}

int main(int /*argc*/, char** /*argv*/) {
    testBootstrap();
    testIntern();
    
    return EXIT_SUCCESS;
}
