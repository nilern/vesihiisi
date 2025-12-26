#define _POSIX_C_SOURCE 200809L // For `getline`

#include "lib/util.c"
#include "lib/arena.c"
#include "lib/bitset.c"
#include "lib/bytefulbitset.c"
#include "lib/object.c"
#include "lib/heap.c"
#include "lib/state.c"
#include "lib/flyweights.c"
#include "lib/read.c"
#include "lib/print.c"
#include "lib/bytecode.c"
#include "lib/namespace.c"
#include "lib/primops.c"
#include "lib/vm.c"
#include "lib/compiler.c"
#include "lib/tocps.c"
#include "lib/liveness.c"
#include "lib/pureloads.c"
#include "lib/regalloc.c"
#include "lib/cloverindexing.c"
#include "lib/bytecodegen.c"

#include <errno.h>

typedef enum ResTag {RES_ERR, RES_OK} ResTag;

typedef struct Vshs_Err {
    enum {
        VSHS_PARSE_ERR,
        VSHS_VM_ERR
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

static MethodRef compile(State* state, ORef expr, bool debug) {
    Compiler compiler = createCompiler();

    IRFn irFn = topLevelExprToIR(state, &compiler, expr);
    if (debug) {
        puts(";; # IR:");
        printIRFn(state, stdout, &compiler, printIRName, &irFn);
        puts("\n");
    }

    enlivenFn(&compiler, &irFn);
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

    indexToplevelFnClovers(&compiler, &irFn);
    if (debug) {
        puts(";; # Concrete IR:");
        printIRFn(state, stdout, &compiler, printIRReg, &irFn);
        puts("\n");
    }

    MethodRef const method = emitToplevelMethod(state, &irFn);
    if (debug) {
        puts(";; # Bytecode:");
        disassemble(state, stdout, method);
        puts("");
    }

    freeCompiler(&compiler);
    return method;
}

static VMRes eval(State* state, ORef expr, bool debug) {
    MethodRef const method = compile(state, expr, debug);
    ClosureRef const closure = allocClosure(state, method, Zero);
    return run(state, closure);
}

static Vshs_MaybeRes readEval(State* state, Parser* parser, bool debug) {
    MaybeORef maybeExpr;
    if (!read(state, &maybeExpr, parser)) {
        return (Vshs_MaybeRes){.val = {.err = {.type = VSHS_PARSE_ERR}, RES_ERR}, true};
    }
    if (!maybeExpr.hasVal) {
        return (Vshs_MaybeRes){};
    }
    ORef const expr = maybeExpr.val;

    if (debug) {
        puts(";; # S-Expression:");
        print(state, stdout, expr);
        puts("\n");
    }

    VMRes const res = eval(state, expr, debug);
    if (res.success) {
        return (Vshs_MaybeRes){.val = {.val = res.val, RES_OK}, true};
    } else {
        return (Vshs_MaybeRes){.val = {.err = {.type = VSHS_VM_ERR}, RES_ERR}, true};
    }
}

#define countof(v) (sizeof(v) / sizeof(*(v)))

typedef struct CLIArgs {
    char const* name;
    char const* filename;
    bool debug;
    bool help;
    bool forceInteractive;
} CLIArgs;

typedef struct CLIErr {
    int idx;
    enum {CLI_ERR_NONFLAG, CLI_ERR_EXTRA_ARGS} type;
} CLIErr;

typedef struct ParseArgvRes {
    union {
        CLIErr err;
        CLIArgs val;
    };
    ResTag tag;
} ParseArgvRes;

void printCLIErr(FILE* dest, char const* argv[], CLIErr const* err) {
    switch (err->type) {
    case CLI_ERR_NONFLAG: {
        fprintf(dest, "%s: unrecognized option '%s'", argv[0], argv[err->idx]);
    }; break;

    case CLI_ERR_EXTRA_ARGS: {
        fprintf(dest, "%s: extra arguments (starting with '%s')", argv[0], argv[err->idx]);
    }; break;
    }
}

static void initDebug(CLIArgs* args) { args->debug = true; }

static void initHelp(CLIArgs* args) { args->help = true; }

static void initForceInteractive(CLIArgs* args) { args->forceInteractive = true; }

static char const* const longFlagNames[] = {"debug", "help", "interactive"};

static char const shortFlagNames[] = {'d', 'h', 'i'};
static_assert(countof(shortFlagNames) == countof(longFlagNames));

static void (*flagInits[])(CLIArgs*) = {
    initDebug, initHelp, initForceInteractive
};
static_assert(countof(flagInits) == countof(longFlagNames));

static char const* const flagDescriptions[] = {
    "turn on debug output (for developing the VM)",
    "display this help and exit",
    "force interactive i.e. if FILE is given launch repl after loading it"
};
static_assert(countof(flagDescriptions) == countof(longFlagNames));

static ParseArgvRes parseArgv(int argc, char const* argv[static argc]) {
    CLIArgs config = {
        .name = argv[0],
        .filename = nullptr,
        .debug = false,
        .help = false,
        .forceInteractive = false
    };

    int i = 1;

    for (char const* arg; i < argc && *(arg = argv[i]) == '-'; ++i) {
        ++arg;

        if (*arg == '-') {
            ++arg;

            for (size_t j = 0; j < countof(longFlagNames); ++j) {
                if (strcmp(arg, longFlagNames[j]) == 0) {
                    flagInits[j](&config);
                    goto nextArg;
                }
            }

            return (ParseArgvRes){.err = {.idx = i, CLI_ERR_NONFLAG}, RES_ERR};
        }

        for (size_t j = 0; j < countof(shortFlagNames); ++j) {
            if (*arg == shortFlagNames[j]) {
                flagInits[j](&config);
                goto nextArg;
            }
        }

        return (ParseArgvRes){.err = {.idx = i, CLI_ERR_NONFLAG}, RES_ERR};

    nextArg:
    }

    if (i < argc) {
        config.filename = argv[i];
        ++i;
    }

    if (i < argc) {
        return (ParseArgvRes){.err = {.idx = i, CLI_ERR_EXTRA_ARGS}, RES_ERR};
    }

    return (ParseArgvRes){.val = config, RES_OK};
}

static const char prompt[] = "vesihiisi> ";

int main(int argc, char const* argv[static argc]) {
    ParseArgvRes const argvRes = parseArgv(argc, argv);
    if (argvRes.tag == RES_ERR) {
        printCLIErr(stderr, argv, &argvRes.err);
        putc('\n', stderr);
        fprintf(stderr, "Try '%s --help' for more information.\n", argv[0]);
        return EXIT_FAILURE;
    }
    CLIArgs const args = argvRes.val;

    if (args.help) {
        printf("Usage: %s [OPTION]... [FILE]\n", argv[0]);
        puts("An uncommon Lisp. Runs FILE if given, else launches a REPL.\n");

        for (size_t i = 0; i < countof(longFlagNames); ++i) {
            printf("  -%c, --%s\t%s\n", shortFlagNames[i], longFlagNames[i], flagDescriptions[i]);
        }

        return EXIT_SUCCESS;
    }

    State state;
    if (!tryCreateState(&state, 1024*1024)) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }

    if (args.filename) { // OPTIMIZE: Use `mmap`:
        FILE* const file = fopen(args.filename, "rb");
        if (!file) {
            fprintf(stderr, "Can't open %s: %s\n", args.filename, strerror(errno));
            return EXIT_FAILURE;
        }

        fseek(file, 0, SEEK_END);
        size_t const fsize = (size_t)ftell(file);
        fseek(file, 0, SEEK_SET);

        char* const fchars = malloc(fsize + 1);
        fread(fchars, fsize, 1, file);
        fchars[fsize] = 0;
        fclose(file);

        Parser parser = createParser((Str){fchars, fsize});

        for (bool validExprs = true; validExprs;) {
            Vshs_MaybeRes const maybeRes = readEval(&state, &parser, args.debug);
            if (!maybeRes.hasVal) { break; }
            Vshs_Res const res = maybeRes.val;

            switch (res.tag) {
            case RES_OK: break;

            case RES_ERR: {
                switch (res.err.type) {
                case VSHS_PARSE_ERR: {
                    fputs("ParseError", stderr);
                    putc('\n', stderr);
                }; break;

                case VSHS_VM_ERR: break; // FIXME?
                }

                validExprs = false;
            }; break;
            }
        }

        free(fchars);
    }

    if (!args.filename || args.forceInteractive) {
        for (;/*ever*/;) {
            printf("%s", prompt);

            char* line = NULL;
            size_t maxLen = 0;
            ssize_t const len = getline(&line, &maxLen, stdin);
            if (len != -1) {
                Parser parser = createParser((Str){line, (size_t)len});
                Vshs_MaybeRes const maybeRes = readEval(&state, &parser, args.debug);
                if (maybeRes.hasVal) {
                    Vshs_Res const res = maybeRes.val;

                    switch (res.tag) {
                    case RES_OK: {
                        print(&state, stdout, res.val);
                        putc('\n', stdout);
                    }; break;

                    case RES_ERR: {
                        switch (res.err.type) {
                        case VSHS_PARSE_ERR: {
                            fputs("ParseError", stderr);
                            putc('\n', stderr);
                        }; break;

                        case VSHS_VM_ERR: break; // FIXME?
                        }
                    }; break;
                    }
                }
            } else {
                puts("Error reading input");

                free(line);
                freeState(&state);
                return EXIT_FAILURE;
            }
            free(line);
        }
    }

    freeState(&state);
    return EXIT_SUCCESS;
}

