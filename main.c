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

typedef enum ResTag {RES_ERR, RES_OK} ResTag;

typedef struct CLIArgs {
    char const* name;
    bool help;
    bool debug;
} CLIArgs;

typedef struct CLIErr {
    size_t idx;
    enum {
        CLI_ERR_NONFLAG
    } type;
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
        fprintf(dest, "%s: unrecognized option '%s'\n", argv[0], argv[err->idx]);
    }; break;
    }
}

static ParseArgvRes parseArgv(int argc, char const* argv[static argc]) {
    CLIArgs config = {
        .name = argv[0],
        .help = false,
        .debug = false
    };

    char const* arg;
    for (size_t i = 1; i < (size_t)argc && *(arg = argv[i]) == '-'; ++i) {
        ++arg;

        switch (*arg) {
        case '-': {// Long flag
            ++arg;

            if (strcmp(arg, "debug") == 0) {
                config.debug = true;
            } else if (strcmp(arg, "help") == 0) {
                config.help = true;
            } else {
                return (ParseArgvRes){.err = {.idx = i, CLI_ERR_NONFLAG}, RES_ERR};
            }
        }; break;

        case 'd': {
            config.debug = true;
        }; break;

        case 'h': {
            config.help = true;
        }; break;

        default: return (ParseArgvRes){.err = {.idx = i, CLI_ERR_NONFLAG}, RES_ERR};
        }
    }

    return (ParseArgvRes){.val = config, RES_OK};
}

static const char prompt[] = "vesihiisi> ";

int main(int argc, char const* argv[static argc]) {
    ParseArgvRes const argvRes = parseArgv(argc, argv);
    if (argvRes.tag == RES_ERR) {
        printCLIErr(stderr, argv, &argvRes.err);
        printf("Try '%s --help' for more information.\n", argv[0]);
        return EXIT_FAILURE;
    }
    CLIArgs const args = argvRes.val;

    if (args.help) {
        printf("Usage: %s [OPTION]...\n", argv[0]);
        puts("An uncommon Lisp.\n");

        // TODO: Make data-driven for consistency with `parseArgv`:
        printf("  -d, --debug\t turn on debug output (for developing the VM)\n");
        printf("  -h, --help\t display this help and exit\n");

        return EXIT_SUCCESS;
    }

    State state;
    if (!tryCreateState(&state, 1024*1024)) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }

    for (;/*ever*/;) {
        printf("%s", prompt);
        
        char* line = NULL;
        size_t maxLen = 0;
        ssize_t const len = getline(&line, &maxLen, stdin);
        if (len != -1) {
            Parser parser = createParser((Str){line, (size_t)len});
            ORef expr;
            if (!read(&state, &expr, &parser)) {
                puts("ParseError");
                
                free(line);
                continue;
            }

            if (args.debug) {
                puts(";; # S-Expression:");
                print(&state, stdout, expr);
                puts("\n");
            }

            MethodRef method;
            {
                Compiler compiler = createCompiler();

                IRFn irFn = topLevelExprToIR(&state, &compiler, expr);
                if (args.debug) {
                    puts(";; # IR:");
                    printIRFn(&state, stdout, &compiler, printIRName, &irFn);
                    puts("\n");
                }

                enlivenFn(&compiler, &irFn);
                if (args.debug) {
                    puts(";; # Enlivened IR:");
                    printIRFn(&state, stdout, &compiler, printIRName, &irFn);
                    puts("\n");
                }

                fnWithPureLoads(&compiler, &irFn);
                if (args.debug) {
                    puts(";; # Cachy-loading IR:");
                    printIRFn(&state, stdout, &compiler, printIRName, &irFn);
                    puts("\n");
                }

                regAllocFn(&compiler, &irFn);
                if (args.debug) {
                    puts(";; # Registral IR:");
                    printIRFn(&state, stdout, &compiler, printIRReg, &irFn);
                    puts("\n");
                }

                indexToplevelFnClovers(&compiler, &irFn);
                if (args.debug) {
                    puts(";; # Concrete IR:");
                    printIRFn(&state, stdout, &compiler, printIRReg, &irFn);
                    puts("\n");
                }

                method = emitToplevelMethod(&state, &irFn);
                if (args.debug) {
                    puts(";; # Bytecode:");
                    disassemble(&state, stdout, method);
                    puts("");
                }

                freeCompiler(&compiler);
            }

            ClosureRef const closure = allocClosure(&state, method, Zero);
            VMRes const res = run(&state, closure);
            if (res.success) {
                print(&state, stdout, res.val);
                putc('\n', stdout);
            }

            free(line);
        } else {
            puts("Error reading input");
            
            free(line);
            freeState(&state);
            return EXIT_FAILURE;
        }
    }

    freeState(&state);
    return EXIT_SUCCESS;
}

