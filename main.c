#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "lib/util.c"
#include "lib/bitset.c"
#include "lib/object.c"
#include "lib/heap.c"
#include "lib/state.c"
#include "lib/read.c"
#include "lib/print.c"
#include "lib/compiler.c"
#include "lib/tocps.c"
#include "lib/bytecode.c"
#include "lib/bytecodegen.c"
#include "lib/namespace.c"
#include "lib/vm.c"

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
            
            Compiler compiler = createCompiler();
            IRFn irFn = topLevelExprToIR(&state, &compiler, expr);
            puts(";; # IR:");
            printIRFn(&state, stdout, &compiler, &irFn);
            puts("\n");
            MethodRef const method = emitMethod(&state, &irFn);
            puts(";; # Bytecode:");
            disassemble(&state, stdout, method);
            puts("");
            freeIRFn(&irFn);
            freeCompiler(&compiler);

            ClosureRef const closure = allocClosure(&state, method, Zero);
            ORef const res = run(&state, closure);
            print(&state, stdout, res);
            puts("");

            free(line);
        } else {
            puts("Error reading input");
            
            free(line);
            return EXIT_FAILURE;
        }
    }

    freeState(&state);
    return EXIT_SUCCESS;
}

