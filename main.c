#define _XOPEN_SOURCE 700 // For `realpath` & `getline`

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

static Vshs_MaybeRes readEval(struct Vshs_State* state, Parser* parser, bool debug) {
    ParseRes const readRes = read(state, parser);
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

#define countof(v) (sizeof(v) / sizeof(*(v)))

typedef struct CLIArgs {
    char const* name;
    char const* filename;
    bool fromStdin;
    bool debug;
    bool help;
    bool forceInteractive;
} CLIArgs;

static void freeCLIArgs(CLIArgs* args) { free((void*)args->filename); }

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

static void printCLIErr(FILE* dest, char const* argv[], CLIErr const* err) {
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
        .fromStdin = false,
        .debug = false,
        .help = false,
        .forceInteractive = false
    };

    int i = 1;

    for (char const* arg; i < argc && *(arg = argv[i]) == '-'; ++i) {
        ++arg;

        bool validFlag = false;

        if (*arg == '-') { // Long flag:
            ++arg;

            for (size_t j = 0; j < countof(longFlagNames); ++j) {
                if (strcmp(arg, longFlagNames[j]) == 0) {
                    flagInits[j](&config);
                    validFlag = true;
                    break;
                }
            }

            if (!validFlag) {
                return (ParseArgvRes){.err = {.idx = i, CLI_ERR_NONFLAG}, RES_ERR};
            }
        } else if (*arg != '\0') { // Short flag(s):
            for (; *arg != '\0'; ++arg) {
                for (size_t j = 0; j < countof(shortFlagNames); ++j) {
                    if (*arg == shortFlagNames[j]) {
                        flagInits[j](&config);
                        validFlag = true;
                        break;
                    }
                }

                if (!validFlag) {
                    return (ParseArgvRes){.err = {.idx = i, CLI_ERR_NONFLAG}, RES_ERR};
                }
            }
        } else { // Empty short flag '-':
            config.fromStdin = true;
        }
    }

    if (i < argc) {
        config.filename = realpath(argv[i], nullptr);
        ++i;
    }

    if (i < argc) {
        return (ParseArgvRes){.err = {.idx = i, CLI_ERR_EXTRA_ARGS}, RES_ERR};
    }

    return (ParseArgvRes){.val = config, RES_OK};
}

static const char stdinName[] = "STDIN";

static const char prompt[] = "vesihiisi> ";

static const char replFilename[] = "REPL";
static Str const replFilenameStr = {replFilename, sizeof replFilename / sizeof *replFilename};

int main(int argc, char const* argv[static argc]) {
    ParseArgvRes const argvRes = parseArgv(argc, argv);
    if (argvRes.tag == RES_ERR) {
        printCLIErr(stderr, argv, &argvRes.err);
        putc('\n', stderr);
        fprintf(stderr, "Try '%s --help' for more information.\n", argv[0]);
        return EXIT_FAILURE;
    }
    CLIArgs args = argvRes.val;

    if (args.help) {
        printf("Usage: %s [OPTION]... [FILE]\n", argv[0]);
        puts("An uncommon Lisp. Runs FILE if given, else launches a REPL.\n");

        for (size_t i = 0; i < countof(longFlagNames); ++i) {
            printf("  -%c, --%s\t%s\n", shortFlagNames[i], longFlagNames[i], flagDescriptions[i]);
        }

        puts("  -\tread program from stdin (as if from FILE)");

        return EXIT_SUCCESS;
    }

    struct Vshs_State* state = tryCreateState(1024*1024);
    if (!state) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }

    bool loadFailed = false;
    if (args.filename || args.fromStdin) {
        if (args.filename && args.fromStdin) {
            puts("Inconsistent CLI args: both '-' and filename given.");
            exit(EXIT_FAILURE);
        }

        char* fchars = nullptr;
        size_t fsize = 0;
        Str filenameStr;
        if (args.filename) {
            // OPTIMIZE: Use `mmap`:
            FILE* const file = fopen(args.filename, "rb");
            if (!file) {
                fprintf(stderr, "Can't open %s: %s\n", args.filename, strerror(errno));
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

            filenameStr = (Str){args.filename, strlen(args.filename)};
        } else {
            assert(args.fromStdin);

            size_t cap = 4096; // A familiar size from paging. Also probably about 100 LOC.
            fchars = malloc(cap);

            for (size_t readCount;
                 (readCount = fread(fchars + fsize, 1, cap - fsize, stdin)) > 0;
            ) {
                fsize += readCount;

                if (fsize == cap) {
                    cap = cap + cap / 2;
                    fchars = realloc(fchars, cap);
                }
            }

            filenameStr = (Str){stdinName, sizeof stdinName / sizeof *stdinName};
        }

        Str const src = {fchars, fsize};
        Parser* const parser = createParser(state, src, filenameStr);
        pushFilenameRoot(state, parser);

        while (!loadFailed) {
            Vshs_MaybeRes const maybeRes = readEval(state, parser, args.debug);
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

        popStackRoots(state, 1);
        freeParser(parser);
        free(fchars);
    }

    if (!(args.filename || args.fromStdin) || (args.forceInteractive && !loadFailed)) {
        for (;/*ever*/;) {
            printf("%s", prompt);

            char* line = NULL;
            size_t maxLen = 0;
            ssize_t const len = getline(&line, &maxLen, stdin);
            if (len != -1) {
                Str const src = {line, (size_t)len};
                Parser* parser = createParser(state, src, replFilenameStr);
                pushFilenameRoot(state, parser);
                Vshs_MaybeRes const maybeRes = readEval(state, parser, args.debug);
                if (maybeRes.hasVal) {
                    Vshs_Res const res = maybeRes.val;

                    switch (res.tag) {
                    case RES_OK: {
                        print(state, stdout, res.val);
                        putc('\n', stdout);
                    }; break;

                    case RES_ERR: {
                        switch (res.err.type) {
                        case VSHS_PARSE_ERR: { // TODO: DRY wrt. parse error on file load
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
                    }; break;
                    }
                }

                popStackRoots(state, 1);
                freeParser(parser);
            } else {
                puts("Error reading input");

                free(line);
                freeCLIArgs(&args);
                freeState(state);
                return EXIT_FAILURE;
            }
            free(line);
        }
    }

    freeState(state);
    freeCLIArgs(&args);
    return EXIT_SUCCESS;
}

