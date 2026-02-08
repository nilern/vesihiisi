#pragma once

#include "../deps/utf8proc/utf8proc.h"

#include "rt.hpp"

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

using ReadExprRes = Res<Vshs_ParseError, Vshs_LocatedORef>;

using MaybeCharPred = bool (*)(int mc);

// TODO: Avoid creating public symbols in libvesihiisi(-dev).a:
struct Parser { // More a class but was declared as a struct (for C) in vesihiisi.h
private:
    uint8_t const* start;
    uint8_t const* const end;
    size_t byteIdx;
    Vshs_RT* state;

    HRef<Loc> currLoc() const {
        return createLoc((RT*)state, HRef<String>::fromUnchecked(filename),
                         Fixnum{(int64_t)byteIdx});
    }

public:
    ORef filename; // FIXME: Actually `HRef<String>` but that would use anonymous namespace

    using PeekRes = Res<Vshs_ParseError, int32_t>;
    using MatchRes = Res<Vshs_ParseError, bool>;

    explicit Parser(RT* t_state, Str str, Str t_filename) :
        start{str.data}, // HACK
        end{str.data + str.len}, // HACK
        byteIdx{0},
        state{(Vshs_RT*)t_state},
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

    Vshs_ParseError error(char c) const {
        auto const loc = currLoc();
        // If this were to fail we should have bailed out with `utf8Error` already:
        assert(peek().success);
        int32_t const actual = peek().val;
        return Vshs_ParseError{loc, actual, {.expectedChar = c}, EXPECTED_CHAR};
    }

    Vshs_ParseError error(char const* charClass) const {
        auto const loc = currLoc();
        // If this were to fail we should have bailed out with `utf8Error` already:
        assert(peek().success);
        int32_t const actual = peek().val;
        return Vshs_ParseError{loc, actual, {.expectedCharClass = charClass}, EXPECTED_CHAR_CLASS};
    }

    Vshs_ParseError utf8Error() const {
        auto const loc = currLoc();
        return Vshs_ParseError{loc, UTF8PROC_ERROR_INVALIDUTF8, {}, INVALID_UTF8};
    }
};

namespace {

Vshs_ParseRes read(RT* state, Parser* parser);

} // namespace
