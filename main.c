#define _POSIX_C_SOURCE 200809L

#include "lib/object.c"

#include <stdlib.h>
#include <stdio.h>

static const char prompt[] = "vesihiisi> ";

int main(int /*argc*/, char** /*argv*/) {
    for (;/*ever*/;) {
        printf("%s", prompt);
        
        char* line = NULL;
        size_t len = 0;
        if (getline(&line, &len, stdin) != -1) {
            printf("%s", line);
        } else {
            puts("Error reading input");
            
            return EXIT_FAILURE;
        }

        free(line);
    }

    return EXIT_SUCCESS;
}

