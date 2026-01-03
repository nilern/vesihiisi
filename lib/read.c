#include <ctype.h>

#include "util/util.h"
#include "vesihiisi.h"
#include "state.h"

Parser createParser(Str src) {
    return (Parser){.curr = src.data, .end = src.data + src.len};
}

/// (\s+|;[^\n]*\n)*
static void skipWhitespace(Parser* parser) {
    while (isspace(*parser->curr) || *parser->curr == ';') {
        while (isspace(*parser->curr)) { ++parser->curr; }

        if (*parser->curr == ';') {
            ++parser->curr; // Discard ';'

            while (*parser->curr) {
                ++parser->curr;

                if (*parser->curr == '\n') { break; }
            }
        }
    }
}

inline static bool isInitial(char c) {
    return isalpha(c) || c == ':' || c == '!' || c == '?'
        || c == '+' || c == '-' || c == '*' || c == '/'
        || c == '=' || c == '<' || c == '>';
}

inline static bool isSubsequent(char c) { return isInitial(c) || isdigit(c); }

static bool readExpr(State* state, ORef* dest, Parser* parser) {
    skipWhitespace(parser);
    
    char c = *parser->curr;
    if (*parser->curr == '\0') { return false; }
    
    if (c == '(') {
        ++parser->curr; // Discard '('

        skipWhitespace(parser);
        
        if (*parser->curr == ')') { // Empty list
            ++parser->curr; // Discard ')'
            *dest = emptyListToORef(state->emptyList);
            return true;
        }
        
        PairRef firstPair = allocPair(state);
        pushStackRoot(state, (ORef*)&firstPair);
        PairRef pair = firstPair;
        pushStackRoot(state, (ORef*)&pair);
        
        // <expr>
        ORef car;
        if (!readExpr(state, &car, parser)) { return false;}
        pairToPtr(pair)->car = car;
        
        skipWhitespace(parser);
        
        while (*parser->curr != ')') {
            if (*parser->curr != '.') {
                PairRef const newPair = allocPair(state);
                pairToPtr(pair)->cdr = pairToORef(newPair);
                pair = newPair;
                
                // <expr>
                ORef car;
                if (!readExpr(state, &car, parser)) {
                    popStackRoots(state, 2);
                    return false;
                }
                pairToPtr(pair)->car = car;
            } else { // '.' <expr>
                ++parser->curr; // Discard '.'
                
                ORef cdr;
                if (!readExpr(state, &cdr, parser)) {
                    popStackRoots(state, 2);
                    return false;
                }
                pairToPtr(pair)->cdr = cdr;
        
                skipWhitespace(parser);
                
                if (*parser->curr != ')') {
                    popStackRoots(state, 2);
                    return false;
                }
                ++parser->curr; // Discard ')'
                *dest = pairToORef(firstPair);
                popStackRoots(state, 2);
                return true;
            }
            
            skipWhitespace(parser);
        }
        
        ++parser->curr; // Discard ')'
        
        pairToPtr(pair)->cdr = emptyListToORef(state->emptyList);
        *dest = pairToORef(firstPair);
        popStackRoots(state, 2);
        return true;
    } else if (isInitial(c)) {
        StringBuilder builder = createStringBuilder();
         do {
            stringBuilderPush(&builder, c);
            c = *++parser->curr;
         } while (isSubsequent(c));
        
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
        
        for (; (c = *parser->curr) != '"'; ++parser->curr) {
            if (c == '\\') {
                ++parser->curr; // Discard '\\'

                switch (*parser->curr) {
                case 'a': c = '\a'; break;
                case 'b': c = '\b'; break;
                case 't': c = '\t'; break;
                case 'n': c = '\n'; break;
                case 'r': c = '\r'; break;
                case '\\': c = '\\'; break;
                default: return false; // TODO: Proper invalid escape parse error
                }
            }

            stringBuilderPush(&builder, c);
        }

        ++parser->curr; // Discard '"'
        
        *dest = stringToORef(createString(state, stringBuilderStr(&builder)));
        freeStringBuilder(&builder); // OPTIMIZE: Reuse same builder
        return true;
    }
    
    return false;
}

bool read(State* state, MaybeORef* dest, Parser* parser) {
    skipWhitespace(parser);

    if (*parser->curr == '\0') {
        *dest = (MaybeORef){};
        return true;
    }

    ORef oref;
    if (readExpr(state, &oref, parser)) {
        *dest = (MaybeORef){.val = oref, true};
        return true;
    } else {
        return false;
    }
}
