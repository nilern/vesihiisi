typedef struct Parser {
    char const* curr;
    char const* end;
} Parser;

inline static Parser createParser(Str src) {
    return (Parser){.curr = src.data, .end = src.data + src.len};
}

inline static bool isSymbolChar(char c) { return isalpha(c) || c == '!' || c == '?'; }

static bool read(State* state, ORef* dest, Parser* parser) {
    while (isspace(*parser->curr)) { ++parser->curr; }
    
    char c = *parser->curr;
    if (*parser->curr == '\0') { return false; }
    
    if (c == '(') {
        ++parser->curr; // Discard '('
        
        while (isspace(*parser->curr)) { ++parser->curr; } // Skip whitespace
        
        if (*parser->curr == ')') { // Empty list
            ++parser->curr; // Discard ')'
            *dest = emptyListToORef(state->emptyList);
            return true;
        }
        
        PairRef const firstPair = allocPair(state);
        PairRef* const pairHandle = (PairRef*)pushTmp(state, pairToORef(firstPair));
        pushTmp(state, pairToORef(firstPair));
        
        // <expr>
        ORef car;
        if (!read(state, &car, parser)) { return false;}
        pairToPtr(*pairHandle)->car = car;
        
        while (isspace(*parser->curr)) { ++parser->curr; } // Skip whitespace
        
        while (*parser->curr != ')') {
            if (*parser->curr != '.') {
                PairRef const newPair = allocPair(state);
                pairToPtr(*pairHandle)->cdr = pairToORef(newPair);
                *pairHandle = newPair;
                
                // <expr>
                ORef car;
                if (!read(state, &car, parser)) { return false;}
                pairToPtr(*pairHandle)->car = car;
            } else { // '.' <expr>
                ++parser->curr; // Discard '.'
                
                ORef cdr;
                if (!read(state, &cdr, parser)) { return false;}
                pairToPtr(*pairHandle)->cdr = cdr;
        
                while (isspace(*parser->curr)) { ++parser->curr; } // Skip whitespace
                
                if (*parser->curr != ')') { return false; }
                ++parser->curr; // Discard ')'
                *dest = popTmp(state);
                return true;
            }
            
            while (isspace(*parser->curr)) { ++parser->curr; } // Skip whitespace
        }
        
        ++parser->curr; // Discard ')'
        
        pairToPtr(*pairHandle)->cdr = emptyListToORef(state->emptyList);
        *dest = popTmp(state);
        popTmp(state);
        return true;
    } else if (isSymbolChar(c)) {
        StringBuilder builder = createStringBuilder();
         do {
            stringBuilderPush(&builder, c);
            c = *++parser->curr;
         } while (isSymbolChar(c));
        
        *dest = symbolToORef(intern(state, stringBuilderStr(&builder)));
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
        
        *dest = stringToORef(createString(state, stringBuilderStr(&builder)));
        freeStringBuilder(&builder); // OPTIMIZE: Reuse same builder
        return true;
    }
    
    return false;
}

