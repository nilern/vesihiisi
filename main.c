#include "lib/vesihiisi.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define countof(v) (sizeof (v) / sizeof *(v))

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

    struct Vshs_RT* state = Vshs_tryCreateRT(10*1024*1024, vshsHome, argc, argv);
    if (!state) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }

    {
        size_t const vshsHomeCount = strlen(vshsHome);
        size_t const bootstrapFilenameCount = countof(bootstrapFilename);
        size_t const fullbootstrapFilenameCount = vshsHomeCount + 1 + bootstrapFilenameCount;
        char* const fullBootstrapFilename = malloc(fullbootstrapFilenameCount);
        strcpy(fullBootstrapFilename, vshsHome);
        fullBootstrapFilename[vshsHomeCount] = '/'; // TODO: Support non-POSIX filename separator
        strcpy(fullBootstrapFilename + vshsHomeCount + 1, bootstrapFilename);
        fullBootstrapFilename[fullbootstrapFilenameCount - 1] = '\0';

        bool const bootstrapped = Vshs_bootstrap(state, fullBootstrapFilename);

        free(fullBootstrapFilename);
        if (!bootstrapped) { goto error; }
    }

    {
        uint8_t const rawSrc[] = "(load \"base/interpreter.lisp\" *vm-debug*)";
        Str const src = {rawSrc, countof(rawSrc) - 1};
        Vshs_MaybeRes const maybeRes = Vshs_evalString(state, src, replFilenameStr);
        assert(maybeRes.hasVal);
        Vshs_Res res = maybeRes.val;
        if (res.tag != RES_OK) {
            fputs("Bad interpreter file\n", stderr);
            Vshs_freeError(&res.err);
            goto error;
        }
    }

    Vshs_freeRT(state);
    return EXIT_SUCCESS;

error:
    Vshs_freeRT(state);
    return EXIT_FAILURE;
}

