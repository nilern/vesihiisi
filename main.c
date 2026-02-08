#include "lib/vesihiisi.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#define countof(v) sizeof(v) / sizeof *(v)

static const char bootstrapFilename[] = "base/bootstrap.lisp";
static const char homeEnvVarName[] = "VSHS_HOME";

static const uint8_t replFilename[] = "REPL";
static Str const replFilenameStr = {replFilename, countof(replFilename) - 1};

int main(int argc, char const* argv[static argc]) {
    char const* const vshsHome = getenv(homeEnvVarName);
    if (!vshsHome) {
        fprintf(stderr, "Error: %s not set.\n", homeEnvVarName);
        return EXIT_FAILURE;
    }

    size_t const vshsHomeCount = strlen(vshsHome);
    size_t const bootstrapFilenameCount = countof(bootstrapFilename);
    size_t const fullbootstrapFilenameCount = vshsHomeCount + 1 + bootstrapFilenameCount;
    char* const fullBootstrapFilename = malloc(fullbootstrapFilenameCount);
    strcpy(fullBootstrapFilename, vshsHome);
    fullBootstrapFilename[vshsHomeCount] = '/'; // TODO: Support non-POSIX filename separator
    strcpy(fullBootstrapFilename + vshsHomeCount + 1, bootstrapFilename);
    fullBootstrapFilename[fullbootstrapFilenameCount - 1] = '\0';

    struct Vshs_State* state = tryCreateState(10*1024*1024, vshsHome, argc, argv);
    if (!state) {
        puts("Insufficient memory");
        free(fullBootstrapFilename);
        return EXIT_FAILURE;
    }

    bool loadFailed = false;

    char* fchars = nullptr;
    size_t fsize = 0;
    // OPTIMIZE: Use `mmap`:
    FILE* const file = fopen(fullBootstrapFilename, "rb");
    if (!file) {
        fprintf(stderr, "Can't open %s: %s\n", fullBootstrapFilename, strerror(errno));
        freeState(state);
        free(fullBootstrapFilename);
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

    Str filenameStr = (Str){(uint8_t*)fullBootstrapFilename, strlen(fullBootstrapFilename) - 1};

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
        uint8_t const rawSrc[] = "(load \"base/interpreter.lisp\" *vm-debug*)";
        Str const src = {rawSrc, countof(rawSrc) - 1};
        Vshs_MaybeRes const maybeRes = Vshs_evalString(state, src, replFilenameStr);
        assert(maybeRes.hasVal);
        Vshs_Res const res = maybeRes.val;
        if (res.tag != RES_OK) {
            fputs("Bad interpreter file\n", stderr);
            freeState(state);
            free(fullBootstrapFilename);
            return EXIT_FAILURE;
        }
    }

    freeState(state);
    free(fullBootstrapFilename);
    return EXIT_SUCCESS;
}

