#include <ctype.h>
#include <new> // For placement new

#include "util/util.hpp"
#include "state.hpp"

namespace {
void revealMaybeChar(FILE* dest, int mc) {
    switch (mc) {
    case '\a': fputs("\\a", dest); break;
    case '\b': fputs("\\b", dest); break;
    case '\f': fputs("\\f", dest); break;
    case '\n': fputs("\\n", dest); break;
    case '\r': fputs("\\r", dest); break;
    case '\t': fputs("\\t", dest); break;
    case '\v': fputs("\\v", dest); break;

    case EOF: fputs("EOF", dest); break;

    default: putc(mc, dest); break;
    }
}

struct Pos {
    size_t line;
    size_t col;

    Pos() : line{1}, col{1} {}

    void advance(char c) {
        if (c != '\n') {
            ++col;
        } else {
            ++line;
            col = 1;
        }
    }

    void print(FILE* dest) { fprintf(dest, "%lu:%lu", line, col); }
};

// A bit slow but will save lots of debug info space. And fixing the error that the position is
// calculated for will be far slower still. While we do not have e.g. .fasl files `src` should be
// available (if the user lost it they have bigger problems than missing line and column
// numbers...).
Pos byteIdxToPos(Str src, size_t byteIdx) {
    Pos pos{};

    size_t const limit = byteIdx < src.len ? byteIdx : src.len; // Basically use `src.len` for EOF
    for (size_t i = 0; i < limit; ++i) {
        pos.advance(src.data[i]);
    }

    return pos;
}

} // namespace

extern "C" void printParseError(FILE* dest, Str src, ParseError const* err) {
    fputs("unexpected ", dest);
    revealMaybeChar(dest, err->actualMaybeChar);

    // TODO: (Canonical) filename:
    fputs(" at ", dest);
    byteIdxToPos(src, err->byteIdx).print(dest);

    fputs(", expected ", dest);
    switch (err->type) {
    case EXPECTED_CHAR: fprintf(dest, "'%c'", err->expectedChar); break;
    case EXPECTED_CHAR_CLASS: fputs(err->expectedCharClass, dest); break;
    }
}

using ReadExprRes = Res<ParseError, ORef>;

using MaybeCharPred = bool (*)(int mc);

// TODO: Avoid creating public symbols in libvesihiisi(-dev).a:
struct Parser {
private:
    char const* start;
    char const* const end;
    size_t byteIdx;

public:
    explicit Parser(Str str) : start{str.data}, end{str.data + str.len}, byteIdx{0} {}

    char const* curr() const { return start + byteIdx; }

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

    ReadExprRes error(char c) {
        ParseError err{byteIdx, peek(), {.expectedChar = c}, EXPECTED_CHAR};
        return ReadExprRes{{.err = err}, false};
    }

    ReadExprRes error(char const* charClass) {
        ParseError err{byteIdx, peek(), {.expectedCharClass = charClass}, EXPECTED_CHAR_CLASS};
        return ReadExprRes{{.err = err}, false};
    }
};

extern "C" Parser* createParser(Str src) {
    Parser* const parser = (Parser*)malloc(sizeof *parser);
    if (!parser) { return nullptr; }
    return new (parser) Parser{src};
}

extern "C" void freeParser(Parser* parser) {
    parser->~Parser();
    return free(parser);
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

ReadExprRes readExpr(State* state, Parser* parser);

// <ws> (')' | <expr> <ws> (<expr> <ws>)* (')' | '.' <expr> <ws> ')')
ReadExprRes readListTail(State* state, Parser* parser) {
    skipWhitespace(parser); // <ws>

    if (parser->peek() == ')') { // Empty list
        parser->skipUnchecked(); // ')'
        return ReadExprRes{{.val = state->singletons.emptyList.oref()}, true};
    }

    HRef<Pair> firstPair = allocPair(state);
    pushStackRoot(state, (ORef*)&firstPair);
    HRef<Pair> pair = firstPair;
    pushStackRoot(state, (ORef*)&pair);

    // <expr>
    ReadExprRes const carRes = readExpr(state, parser);
    if (!carRes.success) {
        popStackRoots(state, 2);
        return carRes;
    }
    pair.ptr()->car = carRes.val;

    skipWhitespace(parser); // <ws>

    // (<expr> <ws>)* ; FOLLOW = {')', '.'}
    for (int c; !((c = parser->peek()) == ')' || c == '.');) {
        HRef<Pair> const newPair = allocPair(state);
        pair.ptr()->cdr = newPair.oref();
        pair = newPair;

        // <expr>
        ReadExprRes const carRes = readExpr(state, parser);
        if (!carRes.success) {
            popStackRoots(state, 2);
            return carRes;
        }
        pair.ptr()->car = carRes.val;

        skipWhitespace(parser); // <ws>
    }

    switch (parser->peek()) {
    case ')': {
        parser->skipUnchecked(); // ')'

        pair.ptr()->cdr = state->singletons.emptyList.oref();
    }; break;

    case '.': {
        parser->skipUnchecked(); // '.'

        // <expr>
        ReadExprRes const improperRes = readExpr(state, parser);
        if (!improperRes.success) {
            popStackRoots(state, 2);
            return improperRes;
        }
        pair.ptr()->cdr = improperRes.val;

        skipWhitespace(parser); // <ws>

        if (!parser->match(')')) {
            popStackRoots(state, 2);
            return parser->error(')');
        }
    }; break;

    default: {
        popStackRoots(state, 2);
        return parser->error("')' or '.'");
    }; break;
    }

    popStackRoots(state, 2);
    return ReadExprRes{{.val = firstPair.oref()}, true};
}

// <digit radix>+ ('.' <digit radix>*)?
ReadExprRes readNumber(Parser* parser, int radix) {
    char const* const start = parser->curr();

    // <digit radix>
    if (!parser->match(isDigit[radix])) {
        return parser->error(radixClasses[radix]);
    }

    // <digit radix>*
    while (parser->match(isDigit[radix])) {}

    // ('.' <digit radix>*)?
    if (parser->peek() != '.') { // Fixnum
        return ReadExprRes{{.val= Fixnum{(int64_t)atoll(start)}.oref()}, true};
    } else { // Flonum
        parser->skipUnchecked(); // '.'

        // <digit radix>*
        while (parser->match(isDigit[radix])) {}

        return ReadExprRes{{.val= Flonum{atof(start)}.oref()}, true};
    }
}

// <initial> <subsequent>*
ReadExprRes readSymbolTail(State* state, Parser* parser, char const* start) {
    assert(start == parser->curr() - 1 && isInitial(*start)); // <initial>
    // <subsequent>*
    while (parser->match(isSubsequent)) {}

    Str const name{start, (size_t)(parser->curr() - start)};
    return ReadExprRes{{.val = intern(state, name).oref()}, true};
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
ReadExprRes readStringTail(State* state, Parser* parser) {
    StringBuilder builder = createStringBuilder();

    // [^"]*
    for (int c; (c = parser->peek()) != '"';) {
        if (c == EOF) { return parser->error('"'); }

        if (c == '\\') { // Char escape
            parser->skipUnchecked(); // '\\'

            EscapeCharRes const escRes = escapeChar(parser->peek());
            if (!escRes.success) { return parser->error(escRes.err); }
            parser->skipUnchecked(); // Escapee
            c = escRes.val;
        }
        parser->skipUnchecked(); // c

        stringBuilderPush(&builder, (char)c);
    }

    // '"'
    if (!parser->match('"')) { return parser->error('"'); }

    return ReadExprRes{{.val = createString(state, stringBuilderStr(&builder)).oref()}, true};
}

// 't' | 'f' | '"' [^"] '"' | 'x' <number 16>
ReadExprRes readAltTail(Parser* parser) {
    switch (parser->peek()) {
    case 't': {
        parser->skipUnchecked(); // 't'

        return ReadExprRes{{.val = True.oref()}, true};
    }; break;

    case 'f': {
        parser->skipUnchecked(); // 'f'

        return ReadExprRes{{.val = False.oref()}, true};
    }; break;

    case '"': {
        parser->skipUnchecked(); // '"'

        int const mc = parser->peek();
        if (mc == '"' || mc == EOF) { return parser->error("a character following #\""); }
        auto c = static_cast<char>(mc);

        if (c == '\\') {
            parser->skipUnchecked(); // '\\'

            EscapeCharRes const escRes = escapeChar(parser->peek());
            if (!escRes.success) { return parser->error(escRes.err); }
            parser->skipUnchecked(); // Escapee
            c = escRes.val;
        }

        return ReadExprRes{{.val = Char(c).oref()}, true};
    }; break;


    case 'x': {
        parser->skipUnchecked(); // 'x'

        return readNumber(parser, 16);
    }; break;

    default: return parser->error("[tf\"x] following '#'");
    }
}

// <ws> (<list> | <alt> | <string> | <symbol> | <number>)
ReadExprRes readExpr(State* state, Parser* parser) {
    skipWhitespace(parser); // <ws>

    int const c = parser->peek();
    switch (c) {
    case '(': {
        parser->skipUnchecked(); // '('
        return readListTail(state, parser);
    }; break;

    case '#': {
        parser->skipUnchecked(); // '#'
        return readAltTail(parser);
    }; break;

    case '"': {
        parser->skipUnchecked(); // '"'
        return readStringTail(state, parser);
    }; break;
    }

    if (isInitial(c)) {
        char const* const start = parser->curr();
        parser->skipUnchecked(); // `c`
        return readSymbolTail(state, parser, start);
    } else if (isDigit[10](c)) {
        return readNumber(parser, 10); // OPTIMIZE: Rechecks `c`
    } else {
        return parser->error("S-expression");
    }
}

// <ws> (<expr> | $)
ParseRes read(State* state, Parser* parser) {
    skipWhitespace(parser); // <ws>

    if (parser->peek() == EOF) { return ParseRes{{.val = MaybeORef{}}, true}; }

    ReadExprRes const exprRes = readExpr(state, parser);
    if (!exprRes.success) { return ParseRes{{.err = exprRes.err}, false}; }
    return ParseRes{{.val = MaybeORef{exprRes.val, true}}, true};
}

} // namespace

extern "C" ParseRes read(struct Vshs_State* state, Parser* parser) {
    return read((State*)state, parser);
}
