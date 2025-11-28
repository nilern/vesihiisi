static bool read(ORef* dest, char const* src) {
    char c = *src;
    if (c == '\0') { return false; }
    
    if (c == '#') {
        ++src;
        
        if (*src != '"') { return false; }
        ++src;
        c = *src;
        if (c == '"') { return false; }
        ++src;
        if (*src != '"') { return false; }
        
        *dest = charToORef(c);
        return true;
    } else if (isdigit(c)) {
        int32_t n = 0;
        const int32_t radix = 10;
        
        do {
            n = n * radix + (c - '0');
            ++src;
            c = *src;
        } while (isdigit(c));
        
        *dest = int32ToFixnum(n); // FIXME: Overflow (fixnum is only 30 bits)
        return true;
    }
    
    return false;
}

