#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "lib/object.c"
#include "lib/read.c"
#include "lib/print.c"

static const char prompt[] = "vesihiisi> ";

int main(int /*argc*/, char** /*argv*/) {
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

    return EXIT_SUCCESS;
}

