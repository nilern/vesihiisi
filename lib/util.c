typedef struct MaybeSize {
    size_t val;
    bool hasVal;
} MaybeSize;

typedef void (*SwapFn)(void* restrict x, void* restrict y);

static void reverse(void* arr, size_t count, size_t size, SwapFn swap) {
    if (count < 2) { return; }

    for (char *begin = arr, *end = begin + (count - 1) * size;
         begin < end;
         begin += size, end -= size
    ) {
        swap(begin, end);
    }
}

typedef struct BucketIdx {
    size_t idx;
    bool occupied;
} BucketIdx;

typedef struct Str {
    char const* data;
    size_t len;
} Str;

static bool strEq(Str s1, Str s2) {
    return s1.len == s2.len
        && strncmp(s1.data, s2.data, s1.len) == 0;
}

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

// FIXME: Replace with SipHash to prevent DoS attacks:
static uint64_t fnv1aHash(Str s) {
    uint64_t hash = 14695981039346656037u;
    
    for (size_t i = 0; i < s.len; ++i) {
        hash ^= (uint8_t)s.data[i];
        hash *= 1099511628211 ;
    }
    
    return hash;
}

