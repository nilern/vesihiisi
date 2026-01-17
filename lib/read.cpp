#include <ctype.h>
#include <new> // For placement new

#include "../deps/utf8proc/utf8proc.h"

#include "util/util.hpp"
#include "state.hpp"

namespace {
void revealMaybeChar(FILE* dest, int32_t mc) {
    switch (mc) {
    case '\a': fputs("'\\a'", dest); break;
    case '\b': fputs("'\\b'", dest); break;
    case '\f': fputs("'\\f'", dest); break;
    case '\n': fputs("'\\n'", dest); break;
    case '\r': fputs("'\\r'", dest); break;
    case '\t': fputs("'\\t'", dest); break;
    case '\v': fputs("'\\v'", dest); break;

    case EOF: fputs("EOF", dest); break;

    default: {
        uint8_t buf[4];
        ssize_t const width = utf8proc_encode_char(mc, buf);
        // TODO: Avoid POSIX format specifier extension:
        fprintf(dest, "#\"%.*s\"", (int)width, buf);
    }; break;
    }
}
} // namespace

extern "C" void printParseError(FILE* dest, Str src, ParseError const* err) {
    if (err->type == INVALID_UTF8) {
        fputs("invalid UTF-8", dest);
    } else {
        fputs("unexpected ", dest);
        revealMaybeChar(dest, err->actualMaybeChar);
    }

    fputs(" at ", dest);
    HRef<Loc> const loc = HRef<Loc>::fromUnchecked(err->loc);
    printFilename(dest, loc.ptr()->filename.ptr()->str());
    putc(':', dest);
    byteIdxToCoord(src, (uint64_t)loc.ptr()->byteIdx.val()).print(dest);

    switch (err->type) {
    case EXPECTED_CHAR: fprintf(dest, ", expected '%c'", err->expectedChar); break;
    case EXPECTED_CHAR_CLASS: fprintf(dest, ", expected %s", err->expectedCharClass); break;
    case INVALID_UTF8: break;
    }
}

using ReadExprRes = Res<ParseError, Vshs_LocatedORef>;

using MaybeCharPred = bool (*)(int mc);

// TODO: Avoid creating public symbols in libvesihiisi(-dev).a:
struct Parser { // More a class but was declared as a struct (for C) in vesihiisi.h
private:
    uint8_t const* start;
    uint8_t const* const end;
    size_t byteIdx;
    Vshs_State* state;

    HRef<Loc> currLoc() const {
        return createLoc((State*)state, HRef<String>::fromUnchecked(filename),
                         Fixnum{(int64_t)byteIdx});
    }

public:
    ORef filename; // FIXME: Actually `HRef<String>` but that would use anonymous namespace

    using PeekRes = Res<ParseError, int32_t>;
    using MatchRes = Res<ParseError, bool>;

    explicit Parser(State* t_state, Str str, Str t_filename) :
        start{str.data}, // HACK
        end{str.data + str.len}, // HACK
        byteIdx{0},
        state{(Vshs_State*)t_state},
        filename{createString(t_state, t_filename)}
    {}

    [[nodiscard]]
    uint8_t const* curr() const { return start + byteIdx; }

    [[nodiscard]]
    size_t currIdx() const { return byteIdx; }

    [[nodiscard]]
    PeekRes peek() const {
        uint8_t const* const data = curr();
        if (data >= end) { return PeekRes{int32_t(EOF)}; }

        ssize_t const count = end - data;
        int32_t maybeCp;
        ssize_t const maybeCpSize = utf8proc_iterate(data, count, &maybeCp);
        if (maybeCpSize < 0) { return PeekRes{utf8Error()}; }

        return PeekRes{maybeCp};
    }

    void skipUnchecked(size_t cpWidth) {
        assert(curr() + cpWidth <= end);
        byteIdx += cpWidth;
    }

    [[nodiscard]]
    MatchRes match(MaybeCharPred acceptable) {
        int32_t const maybeCp = TRY(MatchRes, peek());
        if (maybeCp == EOF) { return MatchRes{false}; }

        if (acceptable(maybeCp)) {
            skipUnchecked(size_t(utf8EncodedWidth(maybeCp)));
            return MatchRes{true};
        }

        return MatchRes{false};
    }

    [[nodiscard]]
    MatchRes match(uint32_t c) {
        int32_t const maybeCp = TRY(MatchRes, peek());
        if (maybeCp == EOF) { return MatchRes{false}; }

        if (uint32_t(maybeCp) == c) {
            skipUnchecked(size_t(utf8EncodedWidth(maybeCp)));
            return MatchRes{true};
        }

        return MatchRes{false};
    }

    ParseError error(char c) const {
        auto const loc = currLoc();
        // If this were to fail we should have bailed out with `utf8Error` already:
        assert(peek().success);
        int32_t const actual = peek().val;
        return ParseError{loc, actual, {.expectedChar = c}, EXPECTED_CHAR};
    }

    ParseError error(char const* charClass) const {
        auto const loc = currLoc();
        // If this were to fail we should have bailed out with `utf8Error` already:
        assert(peek().success);
        int32_t const actual = peek().val;
        return ParseError{loc, actual, {.expectedCharClass = charClass}, EXPECTED_CHAR_CLASS};
    }

    ParseError utf8Error() const {
        auto const loc = currLoc();
        return ParseError{loc, UTF8PROC_ERROR_INVALIDUTF8, {}, INVALID_UTF8};
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
    return isalpha(mc) || mc == '_' || mc == ':' || mc == '!' || mc == '?'
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

/// The `uintptr_t` carries no information but we can't use `void` or an empty struct...
using SkipWhitespaceRes = Res<ParseError, uintptr_t>;

/// (\s+|;[^\n]*(\n|$))*
[[nodiscard]]
SkipWhitespaceRes skipWhitespace(Parser* parser) {
    for (; /*ever*/;) {
        int32_t const maybeCp = TRY(SkipWhitespaceRes, parser->peek());
        if (isSpace(maybeCp)) {
            parser->skipUnchecked(size_t(utf8EncodedWidth(maybeCp))); // \s

            while (TRY(SkipWhitespaceRes, parser->match(isSpace))) {} // \s*
        } else if (maybeCp == ';') {
            parser->skipUnchecked(1); // ';'

            while (TRY(SkipWhitespaceRes, parser->match(online))) {} // [^\n]*
            // '\n' | $
            if (TRY(SkipWhitespaceRes, parser->peek()) != EOF) {
                TRY(SkipWhitespaceRes, parser->match('\n'));
            }
        } else {
            break;
        }
    }

    return SkipWhitespaceRes{0};
}

using ReadExprTailRes = Res<ParseError, ORef>;

ReadExprRes readExpr(State* state, Parser* parser);

// <ws> (')' | <expr> <ws> (<expr> <ws>)* (')' | '.' <expr> <ws> ')')
ReadExprTailRes readListTail(State* state, Parser* parser) {
    TRY(ReadExprTailRes, skipWhitespace(parser)); // <ws>

    if (TRY(ReadExprTailRes, parser->peek()) == ')') { // Empty list
        parser->skipUnchecked(1); // ')'
        return ReadExprTailRes{state->singletons.emptyList};
    }

    HRef<Pair> firstPair = allocPair(state);
    pushStackRoot(state, (ORef*)&firstPair);
    HRef<Pair> pair = firstPair;
    pushStackRoot(state, (ORef*)&pair);

    // <expr>
    ReadExprRes const carRes = readExpr(state, parser);
    if (!carRes.success) {
        popStackRoots(state, 2);
        return ReadExprTailRes{carRes.err};
    }
    {
        Vshs_LocatedORef const locVal = carRes.val;
        Pair* const pairPtr = pair.ptr();
        pairPtr->car = locVal.val;
        pairPtr->maybeLoc = locVal.loc;
    }

    TRY(ReadExprTailRes, skipWhitespace(parser)); // <ws>

    // (<expr> <ws>)* ; FOLLOW = {')', '.'}
    for (int c; !((c = TRY(ReadExprTailRes, parser->peek())) == ')' || c == '.');) {
        HRef<Pair> const newPair = allocPair(state);
        pair.ptr()->cdr = newPair;
        pair = newPair;

        // <expr>
        ReadExprRes const carRes = readExpr(state, parser);
        if (!carRes.success) {
            popStackRoots(state, 2);
            return ReadExprTailRes{carRes.err};
        }
        {
            Vshs_LocatedORef const locVal = carRes.val;
            Pair* const pairPtr = pair.ptr();
            pairPtr->car = locVal.val;
            pairPtr->maybeLoc = locVal.loc;
        }

        TRY(ReadExprTailRes, skipWhitespace(parser)); // <ws>
    }

    switch (TRY(ReadExprTailRes, parser->peek())) {
    case ')': {
        parser->skipUnchecked(1); // ')'

        pair.ptr()->cdr = state->singletons.emptyList;
    }; break;

    case '.': {
        parser->skipUnchecked(1); // '.'

        // <expr>
        ReadExprRes const improperRes = readExpr(state, parser);
        if (!improperRes.success) {
            popStackRoots(state, 2);
            return ReadExprTailRes{carRes.err};
        }
        pair.ptr()->cdr = improperRes.val.val;

        TRY(ReadExprTailRes, skipWhitespace(parser)); // <ws>

        if (!TRY(ReadExprTailRes, parser->match(')'))) {
            popStackRoots(state, 2);
            return ReadExprTailRes{parser->error(')')};
        }
    }; break;

    default: {
        popStackRoots(state, 2);
        return ReadExprTailRes{parser->error("')' or '.'")};
    }; break;
    }

    popStackRoots(state, 2);
    return ReadExprTailRes{firstPair};
}

// <digit radix>+ ('.' <digit radix>*)?
ReadExprTailRes readNumber(Parser* parser, int radix) {
    uint8_t const* const start = parser->curr();

    // <digit radix>
    if (!TRY(ReadExprTailRes, parser->match(isDigit[radix]))) {
        return ReadExprTailRes{parser->error(radixClasses[radix])};
    }

    // <digit radix>*
    while (TRY(ReadExprTailRes, parser->match(isDigit[radix]))) {}

    // ('.' <digit radix>*)?
    if (TRY(ReadExprTailRes, parser->peek()) != '.') { // Fixnum
        return ReadExprTailRes{
            Fixnum{(int64_t)atoll(reinterpret_cast<char const*>(start))} // HACK
        };
    } else { // Flonum
        parser->skipUnchecked(1); // '.'

        // <digit radix>*
        while (TRY(ReadExprTailRes, parser->match(isDigit[radix]))) {}

        return ReadExprTailRes{Flonum{atof(reinterpret_cast<char const*>(start))}}; // HACK
    }
}

// <initial> <subsequent>*
ReadExprTailRes readSymbolTail(State* state, Parser* parser, uint8_t const* start) {
    assert(start == parser->curr() - 1 && isInitial(*start)); // <initial>
    // <subsequent>*
    while (TRY(ReadExprTailRes, parser->match(isSubsequent))) {}

    Str const name{start, (size_t)(parser->curr() - start)};
    return ReadExprTailRes{intern(state, name)};
}

using EscapeCharRes = Res<char const*, char>;

EscapeCharRes escapeChar(int mc) {
    char c;

    switch (mc) {
    case '"': c = '"'; break;
    case 'a': c = '\a'; break;
    case 'b': c = '\b'; break;
    case 't': c = '\t'; break;
    case 'n': c = '\n'; break;
    case 'r': c = '\r'; break;
    case '\\': c = '\\'; break;
    default: return EscapeCharRes{"char escape [\"abtnr\\]"};
    }

    return EscapeCharRes{c};
}

// [^"]* '"'
ReadExprTailRes readStringTail(State* state, Parser* parser) {
    StringBuilder builder = createStringBuilder(); // OPTIMIZE: Reusable one in `Parser`

    // [^"]*
    for (int mc; (mc = TRY(ReadExprTailRes, parser->peek())) != '"';) {
        if (mc == EOF) {
            freeStringBuilder(&builder);
            return ReadExprTailRes{parser->error('"')};
        }
        parser->skipUnchecked(size_t(utf8EncodedWidth(mc))); // `c`

        if (mc == '\\') { // Char escape
            EscapeCharRes const escRes = escapeChar(TRY(ReadExprTailRes, parser->peek()));
            if (!escRes.success) {
                freeStringBuilder(&builder);
                return ReadExprTailRes{parser->error(escRes.err)};
            }
            mc = escRes.val;
            parser->skipUnchecked(size_t(utf8EncodedWidth(mc))); // Escapee
        }

        // OPTIMIZE: encode directly into string builder:
        uint8_t buf[4];
        ssize_t const width = utf8proc_encode_char(mc, buf);
        for (ssize_t i = 0; i < width; ++i) {
            stringBuilderPush(&builder, buf[i]);
        }
    }

    // '"'
    if (!TRY(ReadExprTailRes, parser->match('"'))) {
        freeStringBuilder(&builder);
        return ReadExprTailRes{parser->error('"')};
    }

    HRef<String> const val = createString(state, stringBuilderStr(&builder));
    freeStringBuilder(&builder);
    return ReadExprTailRes{val};
}

// 't' | 'f' | '"' [^"] '"' | 'x' <number 16>
ReadExprTailRes readAltTail(Parser* parser) {
    switch (TRY(ReadExprTailRes, parser->peek())) {
    case 't': {
        parser->skipUnchecked(1); // 't'

        return ReadExprTailRes{True};
    }; break;

    case 'f': {
        parser->skipUnchecked(1); // 'f'

        return ReadExprTailRes{False};
    }; break;

    case '"': {
        parser->skipUnchecked(1); // '"'

        int32_t mc = TRY(ReadExprTailRes, parser->peek());
        if (mc == '"' || mc == EOF) {
            return ReadExprTailRes{parser->error("a character following #\"")};
        }
        parser->skipUnchecked(size_t(utf8EncodedWidth(mc))); // `mc`

        if (mc == '\\') {
            EscapeCharRes const escRes = escapeChar(TRY(ReadExprTailRes, parser->peek()));
            if (!escRes.success) {
                return ReadExprTailRes{parser->error(escRes.err)};
            }
            mc = escRes.val;
            parser->skipUnchecked(size_t(utf8EncodedWidth(mc))); // Escapee
        }

        // '"'
        if (!TRY(ReadExprTailRes, parser->match('"'))) {
            return ReadExprTailRes{parser->error('"')};
        }

        return ReadExprTailRes{Char(uint32_t(mc))};
    }; break;


    case 'x': {
        parser->skipUnchecked(1); // 'x'

        return readNumber(parser, 16);
    }; break;

    default: return ReadExprTailRes{parser->error("[tf\"x] following '#'")};
    }
}

// <ws> (<list> | <alt> | <string> | <symbol> | <number>)
ReadExprRes readExpr(State* state, Parser* parser) {
    TRY(ReadExprRes, skipWhitespace(parser)); // <ws>

    size_t const byteIdx = parser->currIdx();

    int32_t const c = TRY(ReadExprRes, parser->peek());
    switch (c) {
    case '(': {
        parser->skipUnchecked(1); // '('

        ReadExprTailRes tailRes = readListTail(state, parser);
        if (!tailRes.success) { return ReadExprRes{tailRes.err}; }

        pushStackRoot(state, &tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        popStackRoots(state, 1);
        return ReadExprRes{{tailRes.val, loc}};
    }; break;

    case '#': {
        parser->skipUnchecked(1); // '#'


        ReadExprTailRes const tailRes = readAltTail(parser);
        if (!tailRes.success) { return ReadExprRes{tailRes.err}; }

        // TODO: If `readAltTail` starts returning non-scalars, need to save `&tailRes.val`.
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        return ReadExprRes{{tailRes.val, loc}};
    }; break;

    case '"': {
        parser->skipUnchecked(1); // '"'

        ReadExprTailRes tailRes = readStringTail(state, parser);
        if (!tailRes.success) { return ReadExprRes{tailRes.err}; }

        pushStackRoot(state, &tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        popStackRoots(state, 1);
        return ReadExprRes{{tailRes.val, loc}};
    }; break;

    case '\'': {
        parser->skipUnchecked(1); // '"'

        HRef<Loc> loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                  Fixnum{(int64_t)byteIdx});
        pushStackRoot(state, &loc);

        auto const quotee = TRY(ReadExprRes, readExpr(state, parser));
        auto quotation = createPair(state, quotee.val, state->singletons.emptyList, quotee.loc);
        quotation = createPair(state, state->singletons.quote, quotation, loc);

        popStackRoots(state, 1);
        return ReadExprRes{{quotation, loc}};
    }; break;
    }

    if (isInitial(c)) {
        uint8_t const* const start = parser->curr();
        parser->skipUnchecked(size_t(utf8EncodedWidth(c))); // `c`

        ReadExprTailRes tailRes =  readSymbolTail(state, parser, start);
        if (!tailRes.success) { return ReadExprRes{tailRes.err}; }

        pushStackRoot(state, &tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        popStackRoots(state, 1);
        return ReadExprRes{{tailRes.val, loc}};
    } else if (isDigit[10](c)) {
        ReadExprTailRes const tailRes = readNumber(parser, 10); // OPTIMIZE: Rechecks `c`
        if (!tailRes.success) { return ReadExprRes{tailRes.err}; }

        // TODO: If `readNumber` starts returning non-scalars, need to save `&tailRes.val`.
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        return ReadExprRes{{tailRes.val, loc}};
    } else {
        return ReadExprRes{parser->error("S-expression")};
    }
}

// <ws> (<expr> | $)
ParseRes read(State* state, Parser* parser) {
    auto const wsRes = skipWhitespace(parser); // <ws>
    if (!wsRes.success) { return ParseRes{{.err = wsRes.err}, false}; }

    auto const peekRes = parser->peek();
    if (!peekRes.success) { return ParseRes{{.err = peekRes.err}, false}; }
    if (peekRes.val == EOF) { return ParseRes{{.val = {}}, true}; }

    ReadExprRes const exprRes = readExpr(state, parser);
    if (!exprRes.success) { return ParseRes{{.err = exprRes.err}, false}; }
    return ParseRes{{.val = Vshs_MaybeLocatedORef{{exprRes.val.val, exprRes.val.loc}, true}}, true};
}

} // namespace

extern "C" ParseRes read(struct Vshs_State* state, Parser* parser) {
    return read((State*)state, parser);
}
