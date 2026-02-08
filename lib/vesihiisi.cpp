#include "vesihiisi.h"

#include "util/util.cpp"
#include "util/arena.cpp"
#include "util/bitset.cpp"
#include "util/bytefulbitset.cpp"
#include "value.cpp"
#include "heap.cpp"
#include "rt.cpp"
#include "flyweights.cpp"
#include "read.cpp"
#include "write.cpp"
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

extern "C" Vshs_RT* tryCreateRT(
    size_t heapSize, char const* vshsHome, int argc, char const* argv[]
    ) {
    return (Vshs_RT*)RT::tryCreate(heapSize, vshsHome, argc, argv);
}

extern "C" void freeRT(Vshs_RT* state) { freeRT((RT*)state); }

namespace {
typedef struct Vshs_RootGuard {
    struct Vshs_RT* state;
} Vshs_RootGuard;

Vshs_RootGuard* pushRoot(Vshs_RT* state, ORef* stackLoc) {
    auto const guard = new RootGuard{}; // So that we do not move-assign into uninitialized
    *guard = ((RT*)state)->pushRoot(stackLoc);
    return (Vshs_RootGuard*)guard;
}

void popRoot(Vshs_RootGuard* guard) { delete (RootGuard*)guard; }

Parser* createParser(Vshs_RT* state, Str src, Str filename) {
    Parser* const parser = (Parser*)malloc(sizeof *parser);
    if (!parser) { return nullptr; }
    return new (parser) Parser{(RT*)state, src, filename};
}

void freeParser(Parser* parser) {
    parser->~Parser();
    return free(parser);
}

Vshs_RootGuard* pushFilenameRoot(struct Vshs_RT* state, Parser* parser) {
    return pushRoot(state, &parser->filename);
}

ParseRes Vshs_read(struct Vshs_RT* state, Parser* parser) {
    return read((RT*)state, parser);
}
} // namespace

extern "C" void printParseError(FILE* dest, Str src, ParseError const* err) {
    if (err->type == INVALID_UTF8) {
        fputs("invalid UTF-8", dest);
    } else {
        fputs("unexpected ", dest);
        revealMaybeChar(dest, err->actualMaybeChar);
    }

    fputs(" at ", dest);
    HRef<Loc> const loc = HRef<Loc>::fromUnchecked(err->loc);
    printFilename(dest, loc->filename->str());
    putc(':', dest);
    byteIdxToCoord(src, (uint64_t)loc->byteIdx.val()).print(dest);

    switch (err->type) {
    case EXPECTED_CHAR: fprintf(dest, ", expected '%c'", err->expectedChar); break;
    case EXPECTED_CHAR_CLASS: fprintf(dest, ", expected %s", err->expectedCharClass); break;
    case INVALID_UTF8: break;
    }
}

extern "C" void freeSyntaxErrors(SyntaxErrors* errs) { free(errs->vals); }

extern "C" void printSyntaxError(
    Vshs_RT const* extRT, FILE* dest, Str src, SyntaxError const* err
) {
    auto const state = (RT*)extRT;

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
        printFilename(dest, loc->filename->str());
        putc(':', dest);
        byteIdxToCoord(src, (uint64_t)loc->byteIdx.val()).print(dest);
    } else {
        fputs("unknown location (from macro?)", dest);
    }
}

namespace {
typedef enum EvalErrorType {
    SYNTAX_ERROR,
    RUNTIME_ERROR
} EvalErrorType;

typedef struct EvalError {
    union {
        SyntaxErrors syntaxErrs;
        // Runtime errors are handled by `RT::errorHandler`, we just need to know it failed
    };
    EvalErrorType type;
} EvalError;

typedef struct EvalRes {
    union {
        ORef val;
        EvalError err;
    };
    bool success;
} EvalRes;

EvalRes eval(Vshs_RT* extRT, ORef expr, ORef loc, bool debug) {
    RT* const state = (RT*)extRT;

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
} // namespace

extern "C" void Vshs_write(Vshs_RT const* state, FILE* dest, ORef v) {
    write((RT const*)state, dest, v);
}

extern "C" void Vshs_freeError(Vshs_Err* err) {
    switch (err->type) {
    case Vshs_Err::VSHS_PARSE_ERR: break;

    case Vshs_Err::VSHS_SYNTAX_ERRS: {
        freeSyntaxErrors(&err->syntaxErrs);
    }; break;

    case Vshs_Err::VSHS_RUNTIME_ERR: break;
    }
}

static Vshs_MaybeRes readEval(struct Vshs_RT* state, Parser* parser) {
    bool const debug = !eq(reinterpret_cast<RT const*>(state)->debug->val().get(), False);

    ParseRes const readRes = Vshs_read(state, parser);
    if (!readRes.success) {
        return (Vshs_MaybeRes){
            {{.err = {{.parseErr = readRes.err}, Vshs_Err::VSHS_PARSE_ERR}}, RES_ERR},
            true
        };
    }
    Vshs_MaybeLocatedORef const maybeExpr = readRes.val;
    if (!maybeExpr.hasVal) { return (Vshs_MaybeRes){}; }
    ORef const expr = maybeExpr.val.val;
    ORef const loc = maybeExpr.val.loc;

    if (debug) {
        puts(";; # S-Expression:");
        Vshs_write(state, stdout, expr);
        puts("\n");
    }

    EvalRes const res = eval(state, expr, loc, debug);
    if (res.success) {
        return (Vshs_MaybeRes){{{.val = res.val}, RES_OK}, true};
    } else {
        switch (res.err.type) {
        case SYNTAX_ERROR:
            return (Vshs_MaybeRes){
                {
                    {.err = {{.syntaxErrs = res.err.syntaxErrs}, Vshs_Err::VSHS_SYNTAX_ERRS}},
                    RES_ERR
                },
                true
            };

        case RUNTIME_ERROR:
            return (Vshs_MaybeRes){
                {{.err = {{}, Vshs_Err::VSHS_RUNTIME_ERR}}, RES_ERR},
                true
            };

        default: exit(EXIT_FAILURE); // Unreachable
        }
    }
}

extern "C" bool bootstrap(struct Vshs_RT* state, char const* bootstrapFilename) {
    char* fchars = nullptr;
    size_t fsize = 0;
    // OPTIMIZE: Use `mmap`:
    FILE* const file = fopen(bootstrapFilename, "rb");
    if (!file) {
        fprintf(stderr, "Can't open %s: %s\n", bootstrapFilename, strerror(errno));
        return false;
    }

    fseek(file, 0, SEEK_END);
    fsize = (size_t)ftell(file);
    fseek(file, 0, SEEK_SET);

    fchars = static_cast<char*>(malloc(fsize + 1));
    size_t nread /*HACK:*/ [[maybe_unused]] = fread(fchars, 1, fsize, file);
    assert(nread == fsize);
    fchars[fsize] = 0;
    fclose(file);

    Str filenameStr = (Str){(uint8_t*)bootstrapFilename, strlen(bootstrapFilename) - 1};

    Str const src = {(uint8_t*)fchars, fsize};
    Parser* const parser = createParser(state, src, filenameStr);
    Vshs_RootGuard* filenameG = pushFilenameRoot(state, parser);

    bool loadFailed = false;
    while (!loadFailed) {
        Vshs_MaybeRes const maybeRes = readEval(state, parser);
        if (!maybeRes.hasVal) { break; }
        Vshs_Res res = maybeRes.val;

        switch (res.tag) {
        case RES_OK: break;

        case RES_ERR: {
            switch (res.err.type) {
            case Vshs_Err::VSHS_PARSE_ERR: {
                ParseError const* const err = &res.err.parseErr;

                fputs("ParseError: ", stderr);
                printParseError(stderr, src, err);
                putc('\n', stderr);
            }; break;

            case Vshs_Err::VSHS_SYNTAX_ERRS: {
                SyntaxErrors const* const errs = &res.err.syntaxErrs;

                size_t const errorCount = errs->count;
                for (size_t i = 0; i < errorCount; ++i) {
                    fputs("SyntaxError: ", stderr);
                    printSyntaxError(state, stderr, src, &errs->vals[i]);
                    putc('\n', stderr);
                }
            }; break;

            case Vshs_Err::VSHS_RUNTIME_ERR: break; // FIXME?
            }

            Vshs_freeError(&res.err);
            loadFailed = true;
        }; break;
        }
    }

    popRoot(filenameG);
    freeParser(parser);
    free(fchars);
    return !loadFailed;
}

extern "C" Vshs_MaybeRes Vshs_evalString(struct Vshs_RT* state, Str src, Str filename) {
    Parser* parser = createParser(state, src, filename);
    Vshs_RootGuard* filenameG = pushFilenameRoot(state, parser);

    Vshs_MaybeRes const maybeRes = readEval(state, parser);

    popRoot(filenameG);
    freeParser(parser);
    return maybeRes;
}
