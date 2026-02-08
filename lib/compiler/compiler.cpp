#include "compiler.hpp"

#include "ir.hpp"
#include "liveness.hpp"
#include "pureloads.hpp"
#include "regalloc.hpp"
#include "cloverindexing.hpp"
#include "bytecodegen.hpp"
#include "tocps.hpp"
#include "../rt.hpp"
#include "../bytecode.hpp"

namespace {

CompilationRes compile(RT* state, ORef expr, HRef<Loc> loc, bool debug) {
    auto compiler = Compiler{};

    ToIRRes const toIRRes = topLevelExprToIR(state, &compiler, expr, loc);
    if (!toIRRes.success) {
        return CompilationRes{toIRRes.err};
    }
    IRFn irFn = toIRRes.val;
    if (debug) {
        puts(";; # IR:");
        printIRFn(state, stdout, &compiler, printIRName, &irFn);
        puts("\n");
    }

    enlivenFn(compiler, irFn);
    if (debug) {
        puts(";; # Enlivened IR:");
        printIRFn(state, stdout, &compiler, printIRName, &irFn);
        puts("\n");
    }

    fnWithPureLoads(&compiler, &irFn);
    if (debug) {
        puts(";; # Cachy-loading IR:");
        printIRFn(state, stdout, &compiler, printIRName, &irFn);
        puts("\n");
    }

    regAllocFn(&compiler, &irFn);
    if (debug) {
        puts(";; # Registral IR:");
        printIRFn(state, stdout, &compiler, printIRReg, &irFn);
        puts("\n");
    }

    indexToplevelFnClovers(compiler, irFn);
    if (debug) {
        puts(";; # Concrete IR:");
        printIRFn(state, stdout, &compiler, printIRReg, &irFn);
        puts("\n");
    }

    HRef<Method> const method = emitToplevelMethod(state, &compiler, &irFn);
    if (debug) {
        puts(";; # Bytecode:");
        disassemble(state, stdout, method);
        puts("");
    }

    return CompilationRes{method};
}

} // namespace
