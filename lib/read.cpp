#include <ctype.h>
#include <new> // For placement new

#include "util/util.hpp"
#include "state.hpp"

namespace {
void revealMaybeChar(FILE* dest, int mc) {
    switch (mc) {
    case '\a': fputs("'\\a'", dest); break;
    case '\b': fputs("'\\b'", dest); break;
    case '\f': fputs("'\\f'", dest); break;
    case '\n': fputs("'\\n'", dest); break;
    case '\r': fputs("'\\r'", dest); break;
    case '\t': fputs("'\\t'", dest); break;
    case '\v': fputs("'\\v'", dest); break;

    case EOF: fputs("EOF", dest); break;

    default: fprintf(dest, "'%c'", (char)mc); break;
    }
}
} // namespace

extern "C" void printParseError(FILE* dest, Str src, ParseError const* err) {
    fputs("unexpected ", dest);
    revealMaybeChar(dest, err->actualMaybeChar);

    fputs(" at ", dest);
    HRef<Loc> const loc = HRef<Loc>::fromUnchecked(err->loc);
    printFilename(dest, loc.ptr()->filename.ptr()->str());
    putc(':', dest);
    byteIdxToCoord(src, (uint64_t)loc.ptr()->byteIdx.val()).print(dest);

    fputs(", expected ", dest);
    switch (err->type) {
    case EXPECTED_CHAR: fprintf(dest, "'%c'", err->expectedChar); break;
    case EXPECTED_CHAR_CLASS: fputs(err->expectedCharClass, dest); break;
    }
}

using ReadExprRes = Res<ParseError, Vshs_LocatedORef>;

using MaybeCharPred = bool (*)(int mc);

// TODO: Avoid creating public symbols in libvesihiisi(-dev).a:
struct Parser {
private:
    char const* start;
    char const* const end;
    size_t byteIdx;

public:
    ORef filename; // FIXME: Actually `HRef<String>` but that would use anonymous namespace

    explicit Parser(State* state, Str str, Str t_filename) :
        start{str.data},
        end{str.data + str.len},
        byteIdx{0},
        filename{createString(state, t_filename)}
    {}

    char const* curr() const { return start + byteIdx; }

    size_t currIdx() const { return byteIdx; }

    [[nodiscard]]
    int peek() const { return curr() < end ? *curr() : EOF; }

    void skipUnchecked() {
        assert(curr() < end);
        ++byteIdx;
    }

    [[nodiscard]]
    int pop() {
        int const res = peek();
        if (res == EOF) { return res; }
        skipUnchecked();
        return *(curr() - 1);
    }

    bool match(MaybeCharPred acceptable) {
        int const res = peek();
        if (res == EOF) { return false; }

        if (acceptable(res)) {
            skipUnchecked();
            return true;
        }

        return false;
    }

    bool match(char c) {
        int const res = peek();
        if (res == EOF) { return false; }

        if ((char)res == c) {
            skipUnchecked();
            return true;
        }

        return false;
    }

    ParseError error(State* state, char c) {
        HRef<Loc> const loc =
            createLoc(state, HRef<String>::fromUnchecked(filename), Fixnum{(int64_t)byteIdx});
        return ParseError{loc, peek(), {.expectedChar = c}, EXPECTED_CHAR};
    }

    ParseError error(State* state, char const* charClass) {
        HRef<Loc> const loc =
            createLoc(state, HRef<String>::fromUnchecked(filename), Fixnum{(int64_t)byteIdx});
        return ParseError{loc, peek(), {.expectedCharClass = charClass}, EXPECTED_CHAR_CLASS};
    }
};

extern "C" Parser* createParser(Vshs_State* state, Str src, Str filename) {
    Parser* const parser = (Parser*)malloc(sizeof *parser);
    if (!parser) { return nullptr; }
    return new (parser) Parser{(State*)state, src, filename};
}

extern "C" void freeParser(Parser* parser) {
    parser->~Parser();
    return free(parser);
}

extern "C" void pushFilenameRoot(struct Vshs_State* state, Parser* parser) {
    pushStackRoot((State*)state, &parser->filename);
}

namespace {

bool cannotBe(int /*mc*/) { exit(EXIT_FAILURE); }

bool isSpace(int mc) { return (bool)isspace(mc); }

bool isDecimalDigit(int mc) { return (bool)isdigit(mc); }

bool isHexDigit(int mc) { return (bool)isxdigit(mc); }

bool online(int mc) { return !(mc == '\n' || mc == EOF); }

inline bool isInitial(int mc) {
    return isalpha(mc) || mc == ':' || mc == '!' || mc == '?'
           || mc == '+' || mc == '-' || mc == '*' || mc == '/'
           || mc == '=' || mc == '<' || mc == '>';
}

inline bool isSubsequent(int mc) { return isInitial(mc) || isdigit(mc); }

// HastuHasturHastur there is probably some fancy pretty way to initialize things like this. In C
// we could [10] = "[0-9]" etc.
char const* const radixClasses[] = {
    nullptr, nullptr, nullptr, nullptr, nullptr,
    nullptr, nullptr, nullptr, nullptr, nullptr,
    "[0-9]",
    nullptr, nullptr, nullptr, nullptr, nullptr,
    "[0-9a-f]"
};
static_assert(sizeof radixClasses / sizeof *radixClasses == 17);

MaybeCharPred const isDigit[] = {
    cannotBe, cannotBe, cannotBe, cannotBe, cannotBe,
    cannotBe, cannotBe, cannotBe, cannotBe, cannotBe,
    isDecimalDigit,
    cannotBe, cannotBe, cannotBe, cannotBe, cannotBe,
    isHexDigit
};
static_assert(sizeof isDigit / sizeof *isDigit == 17);

/// (\s+|;[^\n]*(\n|$))*
void skipWhitespace(Parser* parser) {
    for (; /*ever*/;) {
        int const c = parser->peek();
        if (isSpace(c)) {
            parser->skipUnchecked(); // \s

            while (parser->match(isSpace)) {} // \s*
        } else if (c == ';') {
            parser->skipUnchecked(); // ';'

            while (parser->match(online)) {} // [^\n]*
            // '\n' | $
            if (parser->peek() != EOF) {
                parser->match('\n');
            }
        } else {
            break;
        }
    }
}

using ReadExprTailRes = Res<ParseError, ORef>;

ReadExprRes readExpr(State* state, Parser* parser);

// <ws> (')' | <expr> <ws> (<expr> <ws>)* (')' | '.' <expr> <ws> ')')
ReadExprTailRes readListTail(State* state, Parser* parser) {
    skipWhitespace(parser); // <ws>

    if (parser->peek() == ')') { // Empty list
        parser->skipUnchecked(); // ')'
        return ReadExprTailRes{{.val = state->singletons.emptyList}, true};
    }

    HRef<Pair> firstPair = allocPair(state);
    pushStackRoot(state, (ORef*)&firstPair);
    HRef<Pair> pair = firstPair;
    pushStackRoot(state, (ORef*)&pair);

    // <expr>
    ReadExprRes const carRes = readExpr(state, parser);
    if (!carRes.success) {
        popStackRoots(state, 2);
        return ReadExprTailRes{{.err = carRes.err}, false};
    }
    {
        Vshs_LocatedORef const locVal = carRes.val;
        Pair* const pairPtr = pair.ptr();
        pairPtr->car = locVal.val;
        pairPtr->maybeLoc = locVal.loc;
    }

    skipWhitespace(parser); // <ws>

    // (<expr> <ws>)* ; FOLLOW = {')', '.'}
    for (int c; !((c = parser->peek()) == ')' || c == '.');) {
        HRef<Pair> const newPair = allocPair(state);
        pair.ptr()->cdr = newPair;
        pair = newPair;

        // <expr>
        ReadExprRes const carRes = readExpr(state, parser);
        if (!carRes.success) {
            popStackRoots(state, 2);
            return ReadExprTailRes{{.err = carRes.err}, false};
        }
        {
            Vshs_LocatedORef const locVal = carRes.val;
            Pair* const pairPtr = pair.ptr();
            pairPtr->car = locVal.val;
            pairPtr->maybeLoc = locVal.loc;
        }

        skipWhitespace(parser); // <ws>
    }

    switch (parser->peek()) {
    case ')': {
        parser->skipUnchecked(); // ')'

        pair.ptr()->cdr = state->singletons.emptyList;
    }; break;

    case '.': {
        parser->skipUnchecked(); // '.'

        // <expr>
        ReadExprRes const improperRes = readExpr(state, parser);
        if (!improperRes.success) {
            popStackRoots(state, 2);
            return ReadExprTailRes{{.err = carRes.err}, false};
        }
        pair.ptr()->cdr = improperRes.val.val;

        skipWhitespace(parser); // <ws>

        if (!parser->match(')')) {
            popStackRoots(state, 2);
            return ReadExprTailRes{{.err = parser->error(state, ')')}, false};
        }
    }; break;

    default: {
        popStackRoots(state, 2);
        return ReadExprTailRes{{.err = parser->error(state, "')' or '.'")}, false};
    }; break;
    }

    popStackRoots(state, 2);
    return ReadExprTailRes{{.val = firstPair}, true};
}

// <digit radix>+ ('.' <digit radix>*)?
ReadExprTailRes readNumber(State* state, Parser* parser, int radix) {
    char const* const start = parser->curr();

    // <digit radix>
    if (!parser->match(isDigit[radix])) {
        return ReadExprTailRes{{.err = parser->error(state, radixClasses[radix])}, false};
    }

    // <digit radix>*
    while (parser->match(isDigit[radix])) {}

    // ('.' <digit radix>*)?
    if (parser->peek() != '.') { // Fixnum
        return ReadExprTailRes{{.val= Fixnum{(int64_t)atoll(start)}}, true};
    } else { // Flonum
        parser->skipUnchecked(); // '.'

        // <digit radix>*
        while (parser->match(isDigit[radix])) {}

        return ReadExprTailRes{{.val= Flonum{atof(start)}}, true};
    }
}

// <initial> <subsequent>*
ReadExprTailRes readSymbolTail(State* state, Parser* parser, char const* start) {
    assert(start == parser->curr() - 1 && isInitial(*start)); // <initial>
    // <subsequent>*
    while (parser->match(isSubsequent)) {}

    Str const name{start, (size_t)(parser->curr() - start)};
    return ReadExprTailRes{{.val = intern(state, name)}, true};
}

using EscapeCharRes = Res<char const*, char>;

EscapeCharRes escapeChar(int mc) {
    char c;

    switch (mc) {
    case 'a': c = '\a'; break;
    case 'b': c = '\b'; break;
    case 't': c = '\t'; break;
    case 'n': c = '\n'; break;
    case 'r': c = '\r'; break;
    case '\\': c = '\\'; break;
    default: return EscapeCharRes{{.err = "char escape [abtnr\\]"}, false};
    }

    return EscapeCharRes{{.val = c}, true};
}

// [^"]* '"'
ReadExprTailRes readStringTail(State* state, Parser* parser) {
    StringBuilder builder = createStringBuilder(); // OPTIMIZE: Reusable one in `Parser`

    // [^"]*
    for (int c; (c = parser->peek()) != '"';) {
        if (c == EOF) {
            freeStringBuilder(&builder);
            return ReadExprTailRes{{.err = parser->error(state, '"')}, false};
        }

        if (c == '\\') { // Char escape
            parser->skipUnchecked(); // '\\'

            EscapeCharRes const escRes = escapeChar(parser->peek());
            if (!escRes.success) {
                freeStringBuilder(&builder);
                return ReadExprTailRes{{.err = parser->error(state, escRes.err)}, false};
            }
            parser->skipUnchecked(); // Escapee
            c = escRes.val;
        }
        parser->skipUnchecked(); // c

        stringBuilderPush(&builder, (char)c);
    }

    // '"'
    if (!parser->match('"')) {
        freeStringBuilder(&builder);
        return ReadExprTailRes{{.err = parser->error(state, '"')}, false};
    }

    freeStringBuilder(&builder);
    return ReadExprTailRes{{.val = createString(state, stringBuilderStr(&builder))}, true};
}

// 't' | 'f' | '"' [^"] '"' | 'x' <number 16>
ReadExprTailRes readAltTail(State* state, Parser* parser) {
    switch (parser->peek()) {
    case 't': {
        parser->skipUnchecked(); // 't'

        return ReadExprTailRes{{.val = True}, true};
    }; break;

    case 'f': {
        parser->skipUnchecked(); // 'f'

        return ReadExprTailRes{{.val = False}, true};
    }; break;

    case '"': {
        parser->skipUnchecked(); // '"'

        int const mc = parser->peek();
        if (mc == '"' || mc == EOF) {
            return ReadExprTailRes{{.err = parser->error(state, "a character following #\"")}, false};
        }
        auto c = static_cast<char>(mc);

        if (c == '\\') {
            parser->skipUnchecked(); // '\\'

            EscapeCharRes const escRes = escapeChar(parser->peek());
            if (!escRes.success) {
                return ReadExprTailRes{{.err = parser->error(state, escRes.err)}, false};
            }
            parser->skipUnchecked(); // Escapee
            c = escRes.val;
        }

        return ReadExprTailRes{{.val = Char(c)}, true};
    }; break;


    case 'x': {
        parser->skipUnchecked(); // 'x'

        return readNumber(state, parser, 16);
    }; break;

    default: return ReadExprTailRes{{.err = parser->error(state, "[tf\"x] following '#'")}, false};
    }
}

// <ws> (<list> | <alt> | <string> | <symbol> | <number>)
ReadExprRes readExpr(State* state, Parser* parser) {
    skipWhitespace(parser); // <ws>

    size_t const byteIdx = parser->currIdx();

    int const c = parser->peek();
    switch (c) {
    case '(': {
        parser->skipUnchecked(); // '('

        ReadExprTailRes tailRes = readListTail(state, parser);
        if (!tailRes.success) { return ReadExprRes{{.err = tailRes.err}, false}; }

        pushStackRoot(state, &tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        popStackRoots(state, 1);
        return ReadExprRes{{.val = {tailRes.val, loc}}, true};
    }; break;

    case '#': {
        parser->skipUnchecked(); // '#'


        ReadExprTailRes const tailRes = readAltTail(state, parser);
        if (!tailRes.success) { return ReadExprRes{{.err = tailRes.err}, false}; }

        // TODO: If `readAltTail` starts returning non-scalars, need to save `&tailRes.val`.
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        return ReadExprRes{{.val = {tailRes.val, loc}}, true};
    }; break;

    case '"': {
        parser->skipUnchecked(); // '"'

        ReadExprTailRes tailRes = readStringTail(state, parser);
        if (!tailRes.success) { return ReadExprRes{{.err = tailRes.err}, false}; }

        pushStackRoot(state, &tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        popStackRoots(state, 1);
        return ReadExprRes{{.val = {tailRes.val, loc}}, true};
    }; break;
    }

    if (isInitial(c)) {
        char const* const start = parser->curr();
        parser->skipUnchecked(); // `c`

        ReadExprTailRes tailRes =  readSymbolTail(state, parser, start);
        if (!tailRes.success) { return ReadExprRes{{.err = tailRes.err}, false}; }

        pushStackRoot(state, &tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        popStackRoots(state, 1);
        return ReadExprRes{{.val = {tailRes.val, loc}}, true};
    } else if (isDigit[10](c)) {
        ReadExprTailRes const tailRes = readNumber(state, parser, 10); // OPTIMIZE: Rechecks `c`
        if (!tailRes.success) { return ReadExprRes{{.err = tailRes.err}, false}; }

        // TODO: If `readNumber` starts returning non-scalars, need to save `&tailRes.val`.
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        return ReadExprRes{{.val = {tailRes.val, loc}}, true};
    } else {
        return ReadExprRes{{.err = parser->error(state, "S-expression")}, false};
    }
}

// <ws> (<expr> | $)
ParseRes read(State* state, Parser* parser) {
    skipWhitespace(parser); // <ws>

    if (parser->peek() == EOF) { return ParseRes{{.val = {}}, true}; }

    ReadExprRes const exprRes = readExpr(state, parser);
    if (!exprRes.success) { return ParseRes{{.err = exprRes.err}, false}; }
    return ParseRes{{.val = Vshs_MaybeLocatedORef{{exprRes.val.val, exprRes.val.loc}, true}}, true};
}

} // namespace

extern "C" ParseRes read(struct Vshs_State* state, Parser* parser) {
    return read((State*)state, parser);
}
