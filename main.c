#include "lib/vesihiisi.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

typedef enum ResTag {RES_ERR, RES_OK} ResTag;

typedef struct Vshs_Err {
    union {
        ParseError parseErr;
        SyntaxErrors syntaxErrs;
    };
    enum {
        VSHS_PARSE_ERR,
        VSHS_SYNTAX_ERRS,
        VSHS_RUNTIME_ERR
    } type;
} Vshs_Err;

typedef struct Vshs_Res {
    union {
        Vshs_Err err;
        ORef val;
    };
    ResTag tag;
} Vshs_Res;

typedef struct Vshs_MaybeRes {
    Vshs_Res val;
    bool hasVal;
} Vshs_MaybeRes;

static Vshs_MaybeRes readEval(struct Vshs_State* state, Parser* parser) {
    bool const debug = Vshs_debug(state);

    ParseRes const readRes = Vshs_read(state, parser);
    if (!readRes.success) {
        return (Vshs_MaybeRes){{.err = {.parseErr = readRes.err, VSHS_PARSE_ERR}}, RES_OK};
    }
    Vshs_MaybeLocatedORef const maybeExpr = readRes.val;
    if (!maybeExpr.hasVal) { return (Vshs_MaybeRes){}; }
    ORef const expr = maybeExpr.val.val;
    ORef const loc = maybeExpr.val.loc;

    if (debug) {
        puts(";; # S-Expression:");
        print(state, stdout, expr);
        puts("\n");
    }

    EvalRes const res = eval(state, expr, loc, debug);
    if (res.success) {
        return (Vshs_MaybeRes){.val = {.val = res.val, RES_OK}, true};
    } else {
        switch (res.err.type) {
        case SYNTAX_ERROR:
            return (Vshs_MaybeRes){
                {
                    .err = {.syntaxErrs = res.err.syntaxErrs, VSHS_SYNTAX_ERRS},
                    RES_ERR
                },
                true
            };

        case RUNTIME_ERROR:
            return (Vshs_MaybeRes){.val = {.err = {.type = VSHS_RUNTIME_ERR}, RES_ERR}, true};

        default: exit(EXIT_FAILURE); // Unreachable
        }
    }
}

static const char bootstrapFilename[] = "base/bootstrap.lisp";
static const char homeEnvVarName[] = "VSHS_HOME";

static const char replFilename[] = "REPL";
static Str const replFilenameStr = {
    (uint8_t const*)replFilename,
    sizeof replFilename / sizeof *replFilename
};

int main(int argc, char const* argv[static argc]) {
    char const* const vshsHome = getenv(homeEnvVarName);
    if (!vshsHome) {
        fprintf(stderr, "Error: %s not set.\n", homeEnvVarName);
        exit(EXIT_FAILURE);
    }

    size_t const vshsHomeCount = strlen(vshsHome);
    size_t const bootstrapFilenameCount = sizeof bootstrapFilename / sizeof *bootstrapFilename;
    size_t const fullbootstrapFilenameCount = vshsHomeCount + 1 + bootstrapFilenameCount;
    char* const fullBootstrapFilename = malloc(fullbootstrapFilenameCount);
    strcpy(fullBootstrapFilename, vshsHome);
    fullBootstrapFilename[vshsHomeCount] = '/'; // TODO: Support non-POSIX filename separator
    strcpy(fullBootstrapFilename + vshsHomeCount + 1, bootstrapFilename);
    fullBootstrapFilename[fullbootstrapFilenameCount - 1] = '\0';

    struct Vshs_State* state = tryCreateState(10*1024*1024, vshsHome, argc, argv);
    if (!state) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }

    bool loadFailed = false;

    char* fchars = nullptr;
    size_t fsize = 0;
    // OPTIMIZE: Use `mmap`:
    FILE* const file = fopen(fullBootstrapFilename, "rb");
    if (!file) {
        fprintf(stderr, "Can't open %s: %s\n", fullBootstrapFilename, strerror(errno));
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    fsize = (size_t)ftell(file);
    fseek(file, 0, SEEK_SET);

    fchars = malloc(fsize + 1);
    size_t nread /*HACK:*/ [[maybe_unused]] = fread(fchars, 1, fsize, file);
    assert(nread == fsize);
    fchars[fsize] = 0;
    fclose(file);

    Str filenameStr = (Str){(uint8_t*)fullBootstrapFilename, strlen(fullBootstrapFilename)};

    Str const src = {(uint8_t*)fchars, fsize};
    Parser* const parser = createParser(state, src, filenameStr);
    Vshs_RootGuard* filenameG = pushFilenameRoot(state, parser);

    while (!loadFailed) {
        Vshs_MaybeRes const maybeRes = readEval(state, parser);
        if (!maybeRes.hasVal) { break; }
        Vshs_Res const res = maybeRes.val;

        switch (res.tag) {
        case RES_OK: break;

        case RES_ERR: {
            switch (res.err.type) {
            case VSHS_PARSE_ERR: { // TODO: DRY wrt. parse error in REPL
                fputs("ParseError: ", stderr);
                printParseError(stderr, src, &res.err.parseErr);
                putc('\n', stderr);
            }; break;

            case VSHS_SYNTAX_ERRS: {
                SyntaxErrors errs = res.err.syntaxErrs;

                size_t const errorCount = errs.count;
                for (size_t i = 0; i < errorCount; ++i) {
                    fputs("SyntaxError: ", stderr);
                    printSyntaxError(state, stderr, src, &errs.vals[i]);
                    putc('\n', stderr);
                }

                freeSyntaxErrors(&errs);
            }; break;

            case VSHS_RUNTIME_ERR: break; // FIXME?
            }

            loadFailed = true;
        }; break;
        }
    }

    popRoot(filenameG);
    freeParser(parser);
    free(fchars);

    {
        uint8_t rawSrc[] = "(load \"base/interpreter.lisp\" *vm-debug*)";
        Str src = {rawSrc, (sizeof rawSrc / sizeof *rawSrc) - 1};
        Parser* parser = createParser(state, src, replFilenameStr);
        pushFilenameRoot(state, parser);

        Vshs_MaybeRes const maybeRes = readEval(state, parser);
        assert(maybeRes.hasVal);
        Vshs_Res const res = maybeRes.val;
        if (res.tag != RES_OK) {
            fputs("Bad interpreter file\n", stderr);
            exit(EXIT_FAILURE);
        }

        popRoot(filenameG);
        freeParser(parser);
    }

    freeState(state);
    return EXIT_SUCCESS;
}

