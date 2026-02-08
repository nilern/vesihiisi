#include "read.hpp"

#include <ctype.h>

namespace {

bool cannotBe(int /*mc*/) { PANIC("Unreachable code reached"); }

bool isSpace(int mc) { return (bool)isspace(mc); }

bool isDecimalDigit(int mc) { return (bool)isdigit(mc); }

bool isHexDigit(int mc) { return (bool)isxdigit(mc); }

bool online(int mc) { return !(mc == '\n' || mc == EOF); }

inline bool isInitial(int mc) {
    return isalpha(mc) || mc == '_' || mc == ':' || mc == '!' || mc == '?'
           || mc == '+' || mc == '-' || mc == '*' || mc == '/'
           || mc == '=' || mc == '<' || mc == '>'
           || mc == '&' || mc == '|';
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
using SkipWhitespaceRes = Res<Vshs_ParseError, uintptr_t>;

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

using ReadExprTailRes = Res<Vshs_ParseError, ORef>;

ReadExprRes readExpr(RT* state, Parser* parser);

// <ws> (')' | <expr> <ws> (<expr> <ws>)* (')' | '.' <expr> <ws> ')')
ReadExprTailRes readListTail(RT* state, Parser* parser) {
    TRY(ReadExprTailRes, skipWhitespace(parser)); // <ws>

    if (TRY(ReadExprTailRes, parser->peek()) == ')') { // Empty list
        parser->skipUnchecked(1); // ')'
        return ReadExprTailRes{state->singletons.emptyList};
    }

    HRef<Pair> firstPair = allocPair(state);
    auto const firstPairG = state->pushRoot(&firstPair);
    HRef<Pair> pair = firstPair;
    auto const pairG = state->pushRoot(&pair);

    // <expr>
    ReadExprRes const carRes = readExpr(state, parser);
    if (!carRes.success) { return ReadExprTailRes{carRes.err}; }
    {
        Vshs_LocatedORef locVal = carRes.val;
        auto const locG = state->pushRoot(&locVal.loc);
        pair->car().set(*state, locVal.val);
        pair->maybeLoc().set(*state, locVal.loc);
    }

    TRY(ReadExprTailRes, skipWhitespace(parser)); // <ws>

    // (<expr> <ws>)* ; FOLLOW = {')', '.'}
    for (int c; !((c = TRY(ReadExprTailRes, parser->peek())) == ')' || c == '.');) {
        HRef<Pair> newPair = allocPair(state);
        auto const newPairG = state->pushRoot(&newPair);
        pair->cdr().set(*state, newPair);
        pair = newPair;

        // <expr>
        ReadExprRes const carRes = readExpr(state, parser);
        if (!carRes.success) { return ReadExprTailRes{carRes.err}; }
        {
            Vshs_LocatedORef locVal = carRes.val;
            auto const locG = state->pushRoot(&locVal.loc);
            pair->car().set(*state, locVal.val);
            pair->maybeLoc().set(*state, locVal.loc);
        }

        TRY(ReadExprTailRes, skipWhitespace(parser)); // <ws>
    }

    switch (TRY(ReadExprTailRes, parser->peek())) {
    case ')': {
        parser->skipUnchecked(1); // ')'

        pair->cdr().set(*state, state->singletons.emptyList);
    }; break;

    case '.': {
        parser->skipUnchecked(1); // '.'

        // <expr>
        ReadExprRes const improperRes = readExpr(state, parser);
        if (!improperRes.success) { return ReadExprTailRes{carRes.err}; }
        pair->cdr().set(*state, improperRes.val.val);

        TRY(ReadExprTailRes, skipWhitespace(parser)); // <ws>

        if (!TRY(ReadExprTailRes, parser->match(')'))) {
            return ReadExprTailRes{parser->error(')')};
        }
    }; break;

    default: return ReadExprTailRes{parser->error("')' or '.'")};
    }

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
ReadExprTailRes readSymbolTail(RT* state, Parser* parser, uint8_t const* start) {
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
ReadExprTailRes readStringTail(RT* state, Parser* parser) {
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
ReadExprRes readExpr(RT* state, Parser* parser) {
    TRY(ReadExprRes, skipWhitespace(parser)); // <ws>

    size_t const byteIdx = parser->currIdx();

    int32_t const c = TRY(ReadExprRes, parser->peek());
    switch (c) {
    case '(': {
        parser->skipUnchecked(1); // '('

        ReadExprTailRes tailRes = readListTail(state, parser);
        if (!tailRes.success) { return ReadExprRes{tailRes.err}; }

        auto const tailResValG = state->pushRoot(&tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
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

        auto const tailResValG = state->pushRoot(&tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
        return ReadExprRes{{tailRes.val, loc}};
    }; break;

    case '\'': {
        parser->skipUnchecked(1); // '"'

        HRef<Loc> loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                  Fixnum{(int64_t)byteIdx});
        auto const locG = state->pushRoot(&loc);

        auto const quotee = TRY(ReadExprRes, readExpr(state, parser));
        auto quotation = createPair(state, quotee.val, state->singletons.emptyList, quotee.loc);
        quotation = createPair(state, state->singletons.quote, quotation, loc);

        return ReadExprRes{{quotation, loc}};
    }; break;
    }

    if (isInitial(c)) {
        uint8_t const* const start = parser->curr();
        parser->skipUnchecked(size_t(utf8EncodedWidth(c))); // `c`

        ReadExprTailRes tailRes =  readSymbolTail(state, parser, start);
        if (!tailRes.success) { return ReadExprRes{tailRes.err}; }

        auto const tailResValG = state->pushRoot(&tailRes.val);
        HRef<Loc> const loc = createLoc(state, HRef<String>::fromUnchecked(parser->filename),
                                        Fixnum{(int64_t)byteIdx});
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
Vshs_ParseRes read(RT* state, Parser* parser) {
    auto const wsRes = skipWhitespace(parser); // <ws>
    if (!wsRes.success) { return Vshs_ParseRes{{.err = wsRes.err}, false}; }

    auto const peekRes = parser->peek();
    if (!peekRes.success) { return Vshs_ParseRes{{.err = peekRes.err}, false}; }
    if (peekRes.val == EOF) { return Vshs_ParseRes{{.val = {}}, true}; }

    ReadExprRes const exprRes = readExpr(state, parser);
    if (!exprRes.success) { return Vshs_ParseRes{{.err = exprRes.err}, false}; }
    return Vshs_ParseRes{{.val = Vshs_MaybeLocatedORef{{exprRes.val.val, exprRes.val.loc}, true}}, true};
}

} // namespace
