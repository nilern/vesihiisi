#include "vesihiisi.h"

#include "util/util.cpp"
#include "util/arena.cpp"
#include "util/bitset.cpp"
#include "util/bytefulbitset.cpp"
#include "heap.cpp"
#include "state.cpp"
#include "flyweights.cpp"
#include "read.cpp"
#include "print.cpp"
#include "bytecode.cpp"
#include "namespace.cpp"
#include "dispatch.cpp"
#include "primops.cpp"
#include "vm.cpp"
#include "compiler/compiler.cpp"
#include "compiler/tocps.cpp"
#include "compiler/liveness.cpp"
#include "compiler/pureloads.cpp"
#include "compiler/regalloc.cpp"
#include "compiler/cloverindexing.cpp"
#include "compiler/bytecodegen.cpp"

extern "C" VMRes eval(Vshs_State* extState, ORef expr, bool debug) {
    State* const state = (State*)extState;
    HRef<Method> const method = compile(state, expr, debug);
    HRef<Closure> const closure = allocClosure(state, method, Fixnum(0l));
    return run(state, closure);
}
