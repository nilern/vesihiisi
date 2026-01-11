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

extern "C" EvalRes eval(Vshs_State* extState, ORef expr, ORef loc, bool debug) {
    State* const state = (State*)extState;

    assert(isa(state, state->types.loc, loc));
    CompilationRes const compilationRes =
        compile(state, expr, HRef<Loc>::fromUnchecked(loc), debug);
    if (!compilationRes.success) {
        return EvalRes{
            {.err = {{.syntaxErrs = compilationRes.err}, SYNTAX_ERROR}},
            false
        };
    }
    auto const method = compilationRes.val;

    HRef<Closure> const closure = allocClosure(state, method, Fixnum(0l));
    VMRes const runRes = run(state, closure);
    return runRes.success
        ? EvalRes{{.val = runRes.val}, true}
        : EvalRes{{.err = {{}, RUNTIME_ERROR}}, false};
}
