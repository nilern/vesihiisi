static bool read(ORef* dest, char const* src) {
    char c = *src;
    if (c == '\0') { return false; }
    
    if (c == '#') {
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
            ++src;
            c = *src;
        } while (isdigit(c));
        
        *dest = fixnumToORef(tagInt(n));
        return true;
    }
    
    return false;
}

