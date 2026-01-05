#include "vesihiisi.h"

#include "util/util.c"
#include "util/arena.c"
#include "util/bitset.c"
#include "util/bytefulbitset.c"
#include "object.c"
#include "heap.c"
#include "state.c"
#include "flyweights.c"
#include "read.c"
#include "print.c"
#include "bytecode.c"
#include "namespace.c"
#include "dispatch.c"
#include "primops.c"
#include "vm.c"
#include "compiler/compiler.c"
#include "compiler/tocps.c"
#include "compiler/liveness.c"
#include "compiler/pureloads.c"
#include "compiler/regalloc.c"
#include "compiler/cloverindexing.c"
#include "compiler/bytecodegen.c"

VMRes eval(State* state, ORef expr, bool debug) {
    MethodRef const method = compile(state, expr, debug);
    ClosureRef const closure = allocClosure(state, method, tagInt(0));
    return run(state, closure);
}
