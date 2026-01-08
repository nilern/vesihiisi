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
    assert(eq(typeOf(state, typeType.oref()).oref(), typeType.oref()));
    assert(eq(typeTypePtr->minSize.oref(), Fixnum((int64_t)sizeof(Type)).oref()));
    assert(eq(typeTypePtr->align.oref(), Fixnum((int64_t)alignof(Type)).oref()));
    assert(!typeTypePtr->isBytes.val());
    assert(!typeTypePtr->isFlex.val());
    
    HRef<Type> const stringType = state->types.string;
    Type const* stringTypePtr = stringType.ptr();
    assert(eq(typeOf(state, stringType.oref()).oref(), typeType.oref()));
    assert(eq(stringTypePtr->minSize.oref(), Fixnum(0l).oref()));
    assert(eq(stringTypePtr->align.oref(), Fixnum((int64_t)objectMinAlign).oref()));
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
    
    assert(eq(typeOf(state, sym.oref()).oref(), state->types.symbol.oref()));
    assert(eq(symPtr->hash.oref(), Fixnum((intptr_t)fnv1aHash(name)).oref()));
    assert(strEq(sym.ptr()->name(), name));
    
    HRef<Symbol> const dupSym = intern(state, name);
    
    assert(eq(dupSym.oref(), sym.oref()));

    freeState(state);
}

int main(int /*argc*/, char** /*argv*/) {
    testBootstrap();
    testIntern();
    
    return EXIT_SUCCESS;
}
