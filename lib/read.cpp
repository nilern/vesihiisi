#include <ctype.h>

#include "util/util.hpp"
#include "state.hpp"

extern "C" Parser createParser(Str src) {
    return Parser{.curr = src.data, .end = src.data + src.len};
}

namespace {

/// (\s+|;[^\n]*\n)*
void skipWhitespace(Parser* parser) {
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

inline bool isInitial(char c) {
    return isalpha(c) || c == ':' || c == '!' || c == '?'
        || c == '+' || c == '-' || c == '*' || c == '/'
        || c == '=' || c == '<' || c == '>';
}

inline bool isSubsequent(char c) { return isInitial(c) || isdigit(c); }

bool readExpr(State* state, ORef* dest, Parser* parser) {
    skipWhitespace(parser);
    
    char c = *parser->curr;
    if (*parser->curr == '\0') { return false; }
    
    if (c == '(') {
        ++parser->curr; // Discard '('

        skipWhitespace(parser);
        
        if (*parser->curr == ')') { // Empty list
            ++parser->curr; // Discard ')'
            *dest = state->singletons.emptyList.oref();
            return true;
        }
        
        HRef<Pair> firstPair = allocPair(state);
        pushStackRoot(state, (ORef*)&firstPair);
        HRef<Pair> pair = firstPair;
        pushStackRoot(state, (ORef*)&pair);
        
        // <expr>
        ORef car;
        if (!readExpr(state, &car, parser)) { return false;}
        pair.ptr()->car = car;
        
        skipWhitespace(parser);
        
        while (*parser->curr != ')') {
            if (*parser->curr != '.') {
                HRef<Pair> const newPair = allocPair(state);
                pair.ptr()->cdr = newPair.oref();
                pair = newPair;
                
                // <expr>
                ORef car;
                if (!readExpr(state, &car, parser)) {
                    popStackRoots(state, 2);
                    return false;
                }
                pair.ptr()->car = car;
            } else { // '.' <expr>
                ++parser->curr; // Discard '.'
                
                ORef cdr;
                if (!readExpr(state, &cdr, parser)) {
                    popStackRoots(state, 2);
                    return false;
                }
                pair.ptr()->cdr = cdr;
        
                skipWhitespace(parser);
                
                if (*parser->curr != ')') {
                    popStackRoots(state, 2);
                    return false;
                }
                ++parser->curr; // Discard ')'
                *dest = firstPair.oref();
                popStackRoots(state, 2);
                return true;
            }
            
            skipWhitespace(parser);
        }
        
        ++parser->curr; // Discard ')'
        
        pair.ptr()->cdr = state->singletons.emptyList.oref();
        *dest = firstPair.oref();
        popStackRoots(state, 2);
        return true;
    } else if (isInitial(c)) {
        StringBuilder builder = createStringBuilder();
         do {
            stringBuilderPush(&builder, c);
            c = *++parser->curr;
         } while (isSubsequent(c));

        *dest = intern(state, stringBuilderStr(&builder)).oref();
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
            
            *dest = Char{c}.oref();
            return true;
        
        case 't':
            ++parser->curr;
            *dest = Bool{true}.oref();
            return true;
        
        case 'f':
            ++parser->curr;
            *dest = Bool{false}.oref();
            return true;

        case 'x': {
            ++parser->curr;

            int const radix = 16;
            char const* const start = parser->curr;

            // \d
            if (!isxdigit(*parser->curr)) {
                return false; // TODO: Proper zero digits error
            }
            ++parser->curr;

            // \d*
            while (isxdigit(*parser->curr)) { ++parser->curr; }

            *dest = Fixnum{(int64_t)strtoll(start, nullptr, radix)}.oref();
            return true;
        }
        
        default: return false;
        }
    } else if (isdigit(c)) {
        char const* const start = parser->curr;

        // \d+
        do {
            ++parser->curr;
        } while (isdigit(*parser->curr));

        if (*parser->curr != '.') { // Fixnum
            *dest = Fixnum{(int64_t)atoll(start)}.oref();
            return true;
        } else { // Flonum
            ++parser->curr; // Skip '.'

            // \d*
            while (isdigit(*parser->curr)) { ++parser->curr; }

            *dest = Flonum{atof(start)}.oref();
            return true;
        }
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
        
        *dest = createString(state, stringBuilderStr(&builder)).oref();
        freeStringBuilder(&builder); // OPTIMIZE: Reuse same builder
        return true;
    }
    
    return false;
}

bool read(State* state, MaybeORef* dest, Parser* parser) {
    skipWhitespace(parser);

    if (*parser->curr == '\0') {
        *dest = MaybeORef{};
        return true;
    }

    ORef oref;
    if (readExpr(state, &oref, parser)) {
        *dest = MaybeORef{.val = oref, .hasVal = true};
        return true;
    } else {
        return false;
    }
}

} // namespace

extern "C" bool read(Vshs_State* state, MaybeORef* dest, Parser* parser) {
    return read((State*)state, dest, parser);
}
