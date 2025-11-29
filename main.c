#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "lib/object.c"
#include "lib/heap.c"
#include "lib/read.c"
#include "lib/print.c"

static const char prompt[] = "vesihiisi> ";

int main(int /*argc*/, char** /*argv*/) {
    Heap heap = tryCreateHeap(1024*1024);
    if (!heapIsValid(&heap)) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }

    for (;/*ever*/;) {
        printf("%s", prompt);
        
        char* line = NULL;
        size_t len = 0;
        if (getline(&line, &len, stdin) != -1) {
            ORef expr;
            if (!read(&expr, line)) {
                puts("ParseError");
                
                free(line);
                continue;
            }
            
            print(stdout, expr);
            puts("");

            free(line);
        } else {
            puts("Error reading input");
            
            free(line);
            return EXIT_FAILURE;
        }
    }

    freeHeap(&heap);
    return EXIT_SUCCESS;
}

