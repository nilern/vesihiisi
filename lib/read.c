typedef struct Parser {
    char const* curr;
    char const* end;
} Parser;

inline static Parser createParser(Str src) {
    return (Parser){.curr = src.data, .end = src.data + src.len};
}

static bool read(
    Heap* heap,
    Type const* stringTypePtr, Type const* arrayType, Type const* symbolType,
    SymbolTable* symbols,
    ORef* dest, Parser* parser
) {
    while (isspace(*parser->curr)) { ++parser->curr; }
    
    char c = *parser->curr;
    if (*parser->curr == '\0') { return false; }
    
    if (isalpha(c)) {
        StringBuilder builder = createStringBuilder();
         do {
            stringBuilderPush(&builder, c);
            c = *++parser->curr;
         } while (isalpha(c));
        
        *dest =
            symbolToORef(intern(heap, arrayType, symbolType, symbols, stringBuilderStr(&builder)));
        freeStringBuilder(&builder); // OPTIMIZE: Reuse same builder
        return true;
    } else if (c == '#') {
        ++parser->curr;
        
        switch (*parser->curr) {
        case '"':
            ++parser->curr;
            c = *parser->curr;
            if (c == '"') { return false; }
            ++parser->curr;
            if (*parser->curr != '"') { return false; }
            
            *dest = charToORef(tagChar(c));
            return true;
        
        case 't':
            ++parser->curr;
            *dest = boolToORef(tagBool(true));
            return true;
        
        case 'f':
            ++parser->curr;
            *dest = boolToORef(tagBool(false));
            return true;
        
        default: return false;
        }
    } else if (isdigit(c)) {
        intptr_t n = 0;
        const intptr_t radix = 10;
        
        do {
            n = n * radix + (c - '0');
            c = *++parser->curr;
        } while (isdigit(c));
        
        *dest = fixnumToORef(tagInt(n));
        return true;
    } else if (c == '"') {
        ++parser->curr;
        
        StringBuilder builder = createStringBuilder();
        
        for (;(c = *parser->curr) != '"'; ++parser->curr) {
            stringBuilderPush(&builder, c);
        }
        
        *dest = stringToORef(createString(heap, stringTypePtr, stringBuilderStr(&builder)));
        freeStringBuilder(&builder); // OPTIMIZE: Reuse same builder
        return true;
    }
    
    return false;
}

