typedef struct Str {
    char* data;
    size_t len;
} Str;

typedef struct StringBuilder {
    char* data;
    size_t len;
    size_t cap;
} StringBuilder;

static StringBuilder createStringBuilder(void) {
    size_t const cap = 2;
    
    char* data = malloc(cap);
    if (!data) { exit(EXIT_FAILURE); }
    
    return (StringBuilder){
        .data = data,
        .len = 0,
        .cap = cap
    };
}

inline static Str stringBuilderStr(StringBuilder const* s) { return (Str){s->data, s->len}; }

static void stringBuilderPush(StringBuilder* s, char c) {
    if (s->len == s->cap) {
        size_t const newCap = s->cap + (s->cap >> 1); // cap * 1.5
        
        char* const data = realloc(s->data, newCap);
        if (!data) { exit(EXIT_FAILURE); }
        
        s->data = data;
        s->cap = newCap;
    }
    
    s->data[s->len++] = c;
}

inline static void freeStringBuilder(StringBuilder* s) { free(s->data); }

