#define _POSIX_C_SOURCE 200809L // For `getline`

#include <stddef.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdbit.h>
#include <stdarg.h>

#include "lib/util.c"
#include "lib/arena.c"
#include "lib/bitset.c"
#include "lib/bytefulbitset.c"
#include "lib/object.c"
#include "lib/heap.c"
#include "lib/state.c"
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

static const char prompt[] = "vesihiisi> ";

int main(int /*argc*/, char** /*argv*/) {
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

            puts(";; # S-Expression:");
            print(&state, stdout, expr);
            puts("\n");

            MethodRef method;
            {
                Compiler compiler = createCompiler();

                IRFn irFn = topLevelExprToIR(&state, &compiler, expr);
                puts(";; # IR:");
                printIRFn(&state, stdout, &compiler, printIRName, &irFn);

                puts("\n");

                enlivenFn(&compiler, &irFn);
                puts(";; # Enlivened IR:");
                printIRFn(&state, stdout, &compiler, printIRName, &irFn);

                puts("\n");

                fnWithPureLoads(&compiler, &irFn);
                puts(";; # Cachy-loading IR:");
                printIRFn(&state, stdout, &compiler, printIRName, &irFn);

                puts("\n");

                regAllocFn(&compiler, &irFn);
                puts(";; # Registral IR:");
                printIRFn(&state, stdout, &compiler, printIRReg, &irFn);

                puts("\n");

                indexToplevelFnClovers(&compiler, &irFn);
                puts(";; # Concrete IR:");
                printIRFn(&state, stdout, &compiler, printIRReg, &irFn);

                puts("\n");

                method = emitToplevelMethod(&state, &irFn);
                puts(";; # Bytecode:");
                disassemble(&state, stdout, method);
                puts("");

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

