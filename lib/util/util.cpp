#include "util.hpp"

#include <stdlib.h>
#include <string.h>
#include <filesystem> // OPTIMIZE: Avoid this, requires linking to a bunch of `std::` stuff.

#include "../../deps/utf8proc/utf8proc.h"

namespace {

void reverse(void* arr, size_t count, size_t size, SwapFn swap) {
    if (count < 2) { return; }

    for (char *begin = (char*)arr, *end = begin + (count - 1) * size;
         begin < end;
         begin += size, end -= size
    ) {
        swap(begin, end);
    }
}

bool strEq(Str s1, Str s2) {
    return s1.len == s2.len
        // HACK:
        && strncmp(reinterpret_cast<char const*>(s1.data), reinterpret_cast<char const*>(s2.data),
                   s1.len)
           == 0;
}

StringBuilder createStringBuilder(void) {
    size_t const cap = 2;
    
    auto data = (uint8_t*)malloc(cap);
    if (!data) { exit(EXIT_FAILURE); }
    
    return StringBuilder{
        .data = data,
        .len = 0,
        .cap = cap
    };
}

void stringBuilderPush(StringBuilder* s, uint8_t c) {
    if (s->len == s->cap) {
        size_t const newCap = s->cap + (s->cap >> 1); // cap * 1.5
        
        auto const data = (uint8_t*)realloc(s->data, newCap);
        if (!data) { exit(EXIT_FAILURE); }
        
        s->data = data;
        s->cap = newCap;
    }
    
    s->data[s->len++] = c;
}

uint64_t fnv1aHash_n(uint8_t const* ptr, size_t count) {
    uint64_t hash = 14695981039346656037u;

    for (size_t i = 0; i < count; ++i) {
        hash ^= ptr[i];
        hash *= 1099511628211 ;
    }

    return hash;
}

// FIXME: Replace with SipHash to prevent DoS attacks:
uint64_t fnv1aHash(Str s) { return fnv1aHash_n(s.data, s.len); }

// OPTIMIZE: More sophisticated combination algorithm:
uint64_t hashCombine(uint64_t h1, uint64_t h2) { return 3 * h1 + h2; }

int utf8EncodedWidth(int32_t codepoint) {
    // OPTIMIZE: So many branches:
    return codepoint < 0x80 ? 1
        : codepoint < 0x800 ? 2
        : codepoint < 0x10000 ? 3
        : 4;
}

UTF8InputFile::UTF8InputFile(Str filename) {
    // OPTIMIZE: Do not require copy (ultimately requires storing null terminator in Vesihiisi
    // `String`s):
    char* const cfilename = static_cast<char*>(malloc(filename.len + 1));
    memcpy(cfilename, filename.data, filename.len);
    cfilename[filename.len] = '\0';
    cfile = fopen(cfilename, "rb");
    free(cfilename);
}

size_t UTF8InputFile::skipInvalid() {
    assert(bufCount > 0);
    assert(({int32_t sink; utf8proc_iterate(buf, ssize_t(bufCount), &sink) < 0;}));

    size_t skipCount = 1; // There must be at least one invalid byte

    // Skip any following continuation bytes:
    for (; skipCount < bufCount && isUTF8Cont(buf[skipCount]); ++skipCount) {}

    bufCount -= skipCount;
    memmove(buf, buf + skipCount, bufCount);

    return skipCount;
}

int32_t UTF8InputFile::peec() {
    if (!isValid()) { return EOF; }

    // OPTIMIZE: Store decoded `int32_t` for this instead:
    int32_t maybeCp;
    ssize_t const cpWidth = utf8proc_iterate(buf, ssize_t(bufCount), &maybeCp);
    if (cpWidth > 0) { return maybeCp; } // `buf` already holds a valid codepoint

    for (;/*ever*/;) {
        // Read 1-4 bytes and try `utf8proc_iterate` for each intermediate length:
        // OPTIMIZE: Actually the first byte in UTF-8 gives the length
        while (bufCount < bufCap) {
            int maybeByte = std::getc(cfile);
            if (maybeByte < 0) { break; }
            buf[bufCount++] = uint8_t(maybeByte);

            int32_t maybeCp;
            ssize_t const cpWidth = utf8proc_iterate(buf, ssize_t(bufCount), &maybeCp);
            if (cpWidth > 0) { return maybeCp; }
        }

        if (bufCount == 0) { return EOF; }

        skipInvalid(); // If we got here `buf` has an invalid prefix, so remove that
    }
}

int32_t UTF8InputFile::getc() {
    int32_t const res = peec();
    bufCount = 0;
    return res;
}

void printFilename(FILE* dest, Str filename) {
    auto const chars = reinterpret_cast<char const*>(filename.data);
    std::filesystem::path const path{chars, chars + filename.len};
    std::error_code pathErr;
    auto const relative = std::filesystem::relative(path, pathErr);
    if (!pathErr && !relative.empty()) {
        fprintf(dest, "%s", relative.c_str());
    } else { // Default to printing the absolute path:
        // TODO: Avoid using POSIX `printf` extension:
        fprintf(dest, "%.*s", (int)filename.len, chars);
    }
}

Coord byteIdxToCoord(Str src, size_t byteIdx) {
    Coord pos{};

    size_t const limit = byteIdx < src.len ? byteIdx : src.len; // Basically use `src.len` for EOF
    for (auto prefix = Str{src.data, limit}; prefix.len > 0;) {
        int32_t codepoint;
        auto maybeCodepointSize = utf8proc_iterate(prefix.data, ssize_t(prefix.len), &codepoint);

        if (maybeCodepointSize >= 0) {
            size_t const codepointSize = size_t(maybeCodepointSize);

            pos.advance(codepoint);

            prefix.data += codepointSize;
            prefix.len -= codepointSize;
        } else { // Best effort recovery:
            // Skip over invalid bytes:
            for (; prefix.len > 0; ++prefix.data, --prefix.len) {
                int32_t codePointSink;
                auto maybeCodepointSize =
                    utf8proc_iterate(prefix.data, ssize_t(prefix.len), &codePointSink);
                if (maybeCodepointSize >= 0) { break; }
            }

            pos.advance(' '); // Count invalid bytes as one codepoint
        }
    }

    return pos;
}

Maybe<Coord> fileByteIdxToCoord(Str filename, size_t byteIdx) {
    Coord pos{};

    UTF8InputFile file;
    if (!UTF8InputFile::open(file, filename)) { return Maybe<Coord>{}; }
    size_t i = 0;
    for (/*size_t i = 0*/; i < byteIdx;) {
        int32_t const maybeCp = file.getc();
        if (maybeCp == EOF) {
            // Surely if we got here the error is at the end of the file:
            return Maybe{pos};
        }

        if (maybeCp > EOF) {
            pos.advance(maybeCp);

            i += size_t(utf8EncodedWidth(maybeCp));
        } else { // Best effort recovery:
            size_t const skipCount = file.skipInvalid();
            pos.advance(' '); // Count invalid bytes as one codepoint

            i += skipCount;
        }
    }

    return Maybe{pos};
}

} // namespace
