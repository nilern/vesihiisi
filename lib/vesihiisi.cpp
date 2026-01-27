#include "vesihiisi.h"

#include "util/util.cpp"
#include "util/arena.cpp"
#include "util/bitset.cpp"
#include "util/bytefulbitset.cpp"
#include "object.cpp"
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

extern "C" Vshs_State* tryCreateState(
    size_t heapSize, char const* vshsHome, int argc, char const* argv[]
    ) {
    return (Vshs_State*)State::tryCreate(heapSize, vshsHome, argc, argv);
}

extern "C" void freeState(Vshs_State* state) { freeState((State*)state); }

extern "C" void pushStackRoot(Vshs_State* state, ORef* stackLoc) {
    pushStackRoot((State*)state, stackLoc);
}

extern "C" void popStackRoots(Vshs_State* state, size_t count) {
    popStackRoots((State*)state, count);
}

extern "C" Parser* createParser(Vshs_State* state, Str src, Str filename) {
    Parser* const parser = (Parser*)malloc(sizeof *parser);
    if (!parser) { return nullptr; }
    return new (parser) Parser{(State*)state, src, filename};
}

extern "C" void freeParser(Parser* parser) {
    parser->~Parser();
    return free(parser);
}

extern "C" void pushFilenameRoot(struct Vshs_State* state, Parser* parser) {
    pushStackRoot((State*)state, &parser->filename);
}

extern "C" ParseRes Vshs_read(struct Vshs_State* state, Parser* parser) {
    return read((State*)state, parser);
}

extern "C" void printParseError(FILE* dest, Str src, ParseError const* err) {
    if (err->type == INVALID_UTF8) {
        fputs("invalid UTF-8", dest);
    } else {
        fputs("unexpected ", dest);
        revealMaybeChar(dest, err->actualMaybeChar);
    }

    fputs(" at ", dest);
    HRef<Loc> const loc = HRef<Loc>::fromUnchecked(err->loc);
    printFilename(dest, loc.ptr()->filename.ptr()->str());
    putc(':', dest);
    byteIdxToCoord(src, (uint64_t)loc.ptr()->byteIdx.val()).print(dest);

    switch (err->type) {
    case EXPECTED_CHAR: fprintf(dest, ", expected '%c'", err->expectedChar); break;
    case EXPECTED_CHAR_CLASS: fprintf(dest, ", expected %s", err->expectedCharClass); break;
    case INVALID_UTF8: break;
    }
}

extern "C" void freeSyntaxErrors(SyntaxErrors* errs) { free(errs->vals); }

extern "C" void printSyntaxError(
    Vshs_State const* extState, FILE* dest, Str src, SyntaxError const* err
    ) {
    auto const state = (State*)extState;

    switch (err->type) {
    case INVALID_DEFINIEND: fputs("Invalid definiend", dest); break;
    case INVALID_PARAM: fputs("Invalid parameter", dest); break;
    case INVALID_BINDING: fputs("Invalid binding (not a two-element list)", dest); break;
    case INVALID_BINDER: fputs("Invalid binder (not a symbol)", dest); break;
    case OVERLONG_BINDING: fputs("Invalid binding (too many expressions)", dest); break;
    }

    fputs(" at ", dest);
    ORef const maybeLoc = err->maybeLoc;
    if (isa(state, state->types.loc, maybeLoc)) {
        auto const loc = HRef<Loc>::fromUnchecked(maybeLoc);
        printFilename(dest, loc.ptr()->filename.ptr()->str());
        putc(':', dest);
        byteIdxToCoord(src, (uint64_t)loc.ptr()->byteIdx.val()).print(dest);
    } else {
        fputs("unknown location (from macro?)", dest);
    }
}

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

extern "C" void print(Vshs_State const* state, FILE* dest, ORef v) {
    print((State const*)state, dest, v);
}
