static bool read(
    Heap* heap,
    Type const* stringTypePtr, Type const* arrayType, Type const* symbolType,
    SymbolTable* symbols,
    ORef* dest, char const* src
) {
    while (isspace(*src)) { ++src; }
    
    char c = *src;
    if (*src == '\0') { return false; }
    
    if (isalpha(c)) {
        StringBuilder builder = createStringBuilder();
         do {
            stringBuilderPush(&builder, c);
            c = *++src;
         } while (isalpha(c));
        
        *dest =
            symbolToORef(intern(heap, arrayType, symbolType, symbols, stringBuilderStr(&builder)));
        freeStringBuilder(&builder); // OPTIMIZE: Reuse same builder
        return true;
    } else if (c == '#') {
        ++src;
        
        switch (*src) {
        case '"':
            ++src;
            c = *src;
            if (c == '"') { return false; }
            ++src;
            if (*src != '"') { return false; }
            
            *dest = charToORef(tagChar(c));
            return true;
        
        case 't':
            ++src;
            *dest = boolToORef(tagBool(true));
            return true;
        
        case 'f':
            ++src;
            *dest = boolToORef(tagBool(false));
            return true;
        
        default: return false;
        }
    } else if (isdigit(c)) {
        intptr_t n = 0;
        const intptr_t radix = 10;
        
        do {
            n = n * radix + (c - '0');
            c = *++src;
        } while (isdigit(c));
        
        *dest = fixnumToORef(tagInt(n));
        return true;
    } else if (c == '"') {
        ++src;
        
        StringBuilder builder = createStringBuilder();
        
        for (;(c = *src) != '"'; ++src) {
            stringBuilderPush(&builder, c);
        }
        
        *dest = stringToORef(createString(heap, stringTypePtr, stringBuilderStr(&builder)));
        freeStringBuilder(&builder); // OPTIMIZE: Reuse same builder
        return true;
    }
    
    return false;
}

