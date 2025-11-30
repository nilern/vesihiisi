#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "lib/util.c"
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
    Type const* typeTypePtr = tryCreateTypeType(&heap.tospace);
    if (!typeTypePtr) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }
    Type const* stringTypePtr = tryCreateStringType(&heap.tospace, typeTypePtr);
    if (!stringTypePtr) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }
    Type const* arrayType = tryCreateArrayType(&heap.tospace, typeTypePtr);
    if (!arrayType) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }
    Type const* symbolType = tryCreateSymbolType(&heap.tospace, typeTypePtr);
    if (!symbolType) {
        puts("Insufficient memory");
        return EXIT_FAILURE;
    }
    SymbolTable symbols = createSymbolTable(&heap, arrayType);

    for (;/*ever*/;) {
        printf("%s", prompt);
        
        char* line = NULL;
        size_t len = 0;
        if (getline(&line, &len, stdin) != -1) {
            ORef expr;
            if (!read(&heap, stringTypePtr, arrayType, symbolType, &symbols, &expr, line)) {
                puts("ParseError");
                
                free(line);
                continue;
            }
            
            print(stdout, stringTypePtr, symbolType, expr);
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

