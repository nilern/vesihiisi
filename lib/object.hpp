#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <bit>

#include "util/util.hpp"
#include "vesihiisi.h"

namespace {

static_assert(sizeof(void*) == sizeof(uint64_t)); // Only 64-bit supported (for now)
// Could support 32-bit now that we NaN-tag. If we get native threads the non-atomicity of 64-bit
// loads and stores would complicate synchronization primitives on 32-bit though.

inline bool eq(ORef x, ORef y) { return x.bits == y.bits; }

enum class TaggedType : uint64_t {
    FLONUM = 0,
    FIXNUM = 1,
    CHAR = 2,
    BOOL = 3,
    HEAPED = 4
};

constexpr uint64_t payloadWidth = 48;
constexpr uint64_t payloadMask = ((uint64_t)1 << payloadWidth) - 1; // `payloadWidth` ones

constexpr uint64_t tagMask = (uint64_t)0x7fff << payloadWidth;

constexpr uint64_t nonFlonumTag = (uint64_t)0x7ffc << payloadWidth;

// OPTIMIZE: Utilize unused bits
constexpr uint64_t fixnumTag = nonFlonumTag | ((uint64_t)TaggedType::FIXNUM << payloadWidth);
constexpr uint64_t charTag = nonFlonumTag | ((uint64_t)TaggedType::CHAR << payloadWidth);
constexpr uint64_t boolTag = nonFlonumTag | ((uint64_t)TaggedType::BOOL << payloadWidth);
// By using 0b00 for pointers we avoid any conflict with actual NaN:s as we do not want null
// pointers anyway:
constexpr uint64_t heapedTag = nonFlonumTag | ((uint64_t)0b00 << payloadWidth);

struct Scalar : public ORef {
    constexpr explicit Scalar(uint64_t t_bits) : ORef{t_bits} {}

    // TODO: Remove now that `Scalar : public ORef`:
    ORef oref() const { return ORef{bits}; }
};
static_assert(sizeof(Scalar) == sizeof(ORef));

struct Fixnum : public Scalar {
    static bool contains(ORef v) { return (v.bits & tagMask) == fixnumTag; }

    static constexpr int64_t max = ((int64_t)1 << (payloadWidth - 1)) - 1;
    static constexpr int64_t min = -(Fixnum::max + 1);

    // FIXME: Handle overflow (fixnum is only 48 bits):
    constexpr explicit Fixnum(int64_t n) : Fixnum{fixnumTag | ((uint64_t)n & payloadMask)} {}

    static Fixnum fromUnchecked(ORef v) { return Fixnum{v.bits}; }

    int64_t val() const {
        uint64_t const nonPayloadWidth = UINT64_WIDTH - payloadWidth;
        return (int64_t)(bits << nonPayloadWidth) >> nonPayloadWidth; // Sign extension
    }

private:
    constexpr explicit Fixnum(uint64_t t_bits) : Scalar{t_bits} {}
};

struct Flonum : public Scalar {
    static bool contains(ORef v) {
        return v.bits == nonFlonumTag // Actual NaN OPTIMIZE: Is this necessary?
               || (v.bits & nonFlonumTag) != nonFlonumTag; // OPTIMIZE: Do not short-circuit?
    }

    constexpr explicit Flonum(double n) : Flonum{std::bit_cast<uint64_t>(n)} {}

    static Flonum fromUnchecked(ORef v) { return Flonum{v.bits}; }

    double val() const { return std::bit_cast<double>(bits); }

private:
    constexpr explicit Flonum(uint64_t t_bits) : Scalar{t_bits} {}
};

struct Char : public Scalar {
    static bool contains(ORef v) { return (v.bits & tagMask) == charTag; }

    constexpr explicit Char(uint32_t c) : Char{charTag | uint64_t(c)} {}

    static Char fromUnchecked(ORef v) { return Char{v.bits}; }

    uint32_t val() const { return uint32_t(bits & payloadMask); }

private:
    constexpr explicit Char(uint64_t t_bits) : Scalar{t_bits} {}
};

struct Bool : public Scalar {
    static bool contains(ORef v) { return (v.bits & tagMask) == boolTag; }

    constexpr explicit Bool(bool b) : Bool{boolTag | (uint64_t)b} {}

    static Bool fromUnchecked(ORef v) { return Bool{v.bits}; }

    bool val() const { return (bool)(bits & payloadMask); }

private:
    constexpr explicit Bool(uint64_t t_bits) : Scalar{t_bits} {}
};

inline bool isHeaped(ORef v) {
    return v.bits != nonFlonumTag // Not a NaN OPTIMIZE: Is this necessary?
           && (v.bits & tagMask) == heapedTag; // OPTIMIZE: Do not short-circuit?
}

inline TaggedType getTag(ORef v) {
    if (isHeaped(v)) { return TaggedType::HEAPED; }
    if (Flonum::contains(v)) { return TaggedType::FLONUM; }
    return (TaggedType)((v.bits >> payloadWidth) & 0b11);
}

constexpr ORef Default{0}; // 0.0

constexpr Bool True{true};
constexpr Bool False{false};

// Just needs to be distinguishable from both `Header` and flex count fixnum. Using 0.0 has the
// added advantage of requiring no initialization on allocation (as heap is already zeroed):
constexpr ORef AlignmentHole{0};

constexpr ORef Tombstone{boolTag | (uint64_t)false};

inline int64_t uncheckedFixnumToInt(ORef v) { return Fixnum::fromUnchecked(v).val(); }

inline uint32_t uncheckedORefToChar(ORef v) { return Char::fromUnchecked(v).val(); }

struct Object {
    struct Header const* header() const;
    struct Header* header();

    Object* tryForwarded() const;

    void forwardTo(Object* copy);
};

/// Reference to `Object` of type `T`
template<typename T>
struct HRef : public ORef {
    constexpr explicit HRef(T* ptr) : HRef{heapedTag | (uint64_t)ptr} {}

    static HRef<T> fromUnchecked(ORef v) { return std::bit_cast<HRef<T>>(v); }

    T* operator->() const { return std::bit_cast<T*>(bits & payloadMask); }

    T& operator*() const { return *std::bit_cast<T*>(bits & payloadMask); }

private:
    constexpr explicit HRef(uint64_t t_bits) : ORef{t_bits} {}
};

inline ORef tagHeaped(Object* ptr) { return ORef{heapedTag | (uint64_t)ptr}; }

inline Object* uncheckedORefToPtr(ORef v) { return (Object*)(v.bits & payloadMask); }

inline Object* tryORefToPtr(ORef oref) {
    return isHeaped(oref) ? uncheckedORefToPtr(oref) : nullptr;
}

template<typename CRTPSub>
struct BootstrapObject : public Object {
    static bool contains(struct State const& state, ORef v);
};

/// GC-heap object without a flex field
template<typename CRTPSub>
struct FixedObject : public BootstrapObject<CRTPSub> {};

/// GC-heap object with a flex field
template<typename CRTPSub, typename I>
struct AnyIndexedObject : public BootstrapObject<CRTPSub> {
    using Item = I;

    struct FlexHeader const* flexHeader() const;

    Fixnum flexCount() const;

    // TODO: Align result `.data` if we go beyond 'either all slots or all bytes':
    Slice<Item const> flexItems() const {
        return Slice{
            static_cast<CRTPSub const*>(this)->flexData(),
            static_cast<size_t>(flexCount().val())
        };
    }
};

template<typename CRTPSub, typename Item>
struct AnyIndexedMutObject : public AnyIndexedObject<CRTPSub, Item> {
    // TODO: Align result `.data` if we go beyond 'either all slots or all bytes':
    Slice<Item> flexItemsMut() {
        return Slice{
            static_cast<CRTPSub*>(this)->flexDataMut(),
            static_cast<size_t>(this->flexCount().val())
        };
    }
};

/// GC-heap object with only a flex field. Since structs cannot have zero size, this is needed to
/// get the correct `.flexData()` (as `FlexObject::flexData` would add the size of this type, which
/// is 1 != 0).
template<typename CRTPSub, typename Item>
struct IndexedObject : public AnyIndexedObject<CRTPSub, Item> {
    Item const* flexData() const { return std::bit_cast<Item const*>(this); }
};

template<typename CRTPSub, typename Item>
struct IndexedMutObject : public AnyIndexedMutObject<CRTPSub, Item> {
    Item const* flexData() const { return std::bit_cast<Item const*>(this); }

    Item* flexDataMut() { return std::bit_cast<Item*>(this); }
};

/// GC-heap object with both fixed fields and flex field
template<typename CRTPSub, typename Item>
struct FlexObject : public AnyIndexedObject<CRTPSub, Item> {
    Item const* flexData() const {
        return std::bit_cast<Item const*>(std::bit_cast<char const*>(this) + sizeof(CRTPSub));
    }
};

template<typename CRTPSub, typename Item>
struct FlexMutObject : public AnyIndexedMutObject<CRTPSub, Item> {
    Item const* flexData() const {
        return std::bit_cast<Item const*>(std::bit_cast<char const*>(this) + sizeof(CRTPSub));
    }

    Item* flexDataMut() const {
        return std::bit_cast<Item*>(std::bit_cast<char*>(this) + sizeof(CRTPSub));
    }
};

struct Type : public FixedObject<Type> {
    Fixnum minSize;
    Fixnum align;
    Bool isBytes;
    Bool hasCodePtr;
    Bool isFlex;
    Fixnum hash;
    HRef<struct Symbol> name;

    static HRef<Type> reify(struct State const& state);
};

struct Header {
    explicit Header(Type const* type) : Header{std::bit_cast<uint64_t>(type)} {}

    Type* typePtr() const {
        assert(!isRelocation());
        return std::bit_cast<Type*>(bits);
    }

    HRef<Type> type() const { return HRef{typePtr()}; }

    static Header relocation(Object* obj) { return Header{markBit | std::bit_cast<uint64_t>(obj)}; }

    Object* tryForwarded() const {
        return isRelocation() ? std::bit_cast<Object*>(bits & payloadMask) : nullptr;
    }

private:
    explicit Header(uint64_t t_bits) : bits{t_bits} {}

    static constexpr uint64_t markBit = 0b01ull << payloadWidth;

    bool isRelocation() const { return static_cast<bool>(bits & markBit); }

    uint64_t bits;
};

constexpr size_t objectMinAlign = alignof(Header);

Header const* Object::header() const { return std::bit_cast<Header const*>(this) - 1; }
Header* Object::header() { return std::bit_cast<Header*>(this) - 1; }

Object* Object::tryForwarded() const { return header()->tryForwarded(); }

void Object::forwardTo(Object* copy) { *header() = Header::relocation(copy); }

template<typename CRTPSub>
bool BootstrapObject<CRTPSub>::contains(struct State const& state, ORef v) {
    return isHeaped(v)
        && eq(HRef<Object>::fromUnchecked(v)->header()->type(), CRTPSub::reify(state));
}

struct FlexHeader {
    FlexHeader(Fixnum t_count, Type const* t_type) : count{t_count}, base{t_type} {}

    Fixnum count;
    Header base; // Cannot inherit from this since we want `count` to come first.
};

FlexHeader const* uncheckedFlexHeader(ORef v) {
    return std::bit_cast<FlexHeader const*>(uncheckedORefToPtr(v)) - 1;
}

// TODO: Align result if we go beyond 'either all slots or all bytes':
void const* uncheckedUntypedFlexPtr(ORef v) {
    Object const* const obj = &*HRef<Object>::fromUnchecked(v);
    size_t const minSize = (uint64_t)obj->header()->typePtr()->minSize.val();
    return static_cast<void const*>(std::bit_cast<char const*>(obj) + minSize);
}

// TODO: Align result if we go beyond 'either all slots or all bytes':
void* uncheckedUntypedFlexPtrMut(ORef v) {
    Object* const obj = &*HRef<Object>::fromUnchecked(v);
    size_t const minSize = (uint64_t)obj->header()->typePtr()->minSize.val();
    return static_cast<void*>(std::bit_cast<char*>(obj) + minSize);
}

template<typename CRTPSub, typename Item>
FlexHeader const* AnyIndexedObject<CRTPSub, Item>::flexHeader() const {
    return std::bit_cast<FlexHeader const*>(this) - 1;
}

template<typename CRTPSub, typename Item>
Fixnum AnyIndexedObject<CRTPSub, Item>::flexCount() const { return flexHeader()->count; }

struct String : public IndexedObject<String, uint8_t> {
    static HRef<Type> reify(struct State const& state);

    Str str() const { return Str{flexData(), static_cast<size_t>(flexCount().val())}; }
};

struct StringIterator : public FixedObject<StringIterator> {
    ORef string;
    ORef byteIdx;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct Symbol : public FlexObject<Symbol, uint8_t> {
    Fixnum hash;

    static HRef<Type> reify(struct State const& state);

    Str name() const { return Str{flexData(), static_cast<size_t>(flexCount().val())}; }
};

// TODO: `template<typename T> struct Array<T> :`?
struct Array : public IndexedObject<Array, ORef> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    Slice<ORef const> items() const { return flexItems(); }
};

// TODO: `template<typename T> struct Array<T> :`?
struct ArrayMut : public IndexedMutObject<ArrayMut, ORef> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    Slice<ORef const> items() const { return flexItems(); }
    Slice<ORef> itemsMut() { return flexItemsMut(); }
};

struct ByteArray : public IndexedObject<ByteArray, uint8_t> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    Slice<uint8_t const> items() const { return flexItems(); }
};

struct ByteArrayMut : public IndexedMutObject<ByteArrayMut, uint8_t> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    Slice<uint8_t const> items() const { return flexItems(); }
    Slice<uint8_t> itemsMut() { return flexItemsMut(); }
};

struct Loc : public FixedObject<Loc> {
    HRef<String> filename;
    Fixnum byteIdx;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct Pair : public FixedObject<Pair> {
    ORef car;
    ORef cdr;
    ORef maybeLoc;

    static HRef<Type> reify(struct State const& state);
};

/// FIXME: Should have zero size but a byte is forced upon us :(
struct EmptyList : public FixedObject<EmptyList> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

enum class PrimopRes : uintptr_t {
    CONTINUE,
    TAILCALL,
    TAILAPPLY,
    ABORT
};

using MethodCode = PrimopRes (*)(struct State*);

struct Method : public FlexMutObject<Method, ORef> {
    MethodCode nativeCode;
    ORef code;
    ORef consts;
    Bool hasVarArg;
    Fixnum hash;
    ORef maybeName;
    ORef maybeFilenames;
    ORef maybeSrcByteIdxs;

    static HRef<Type> reify(struct State const& state);

    Slice<ORef const> domain() const { return flexItems(); }
    Slice<ORef> domain() { return flexItemsMut(); }
};

struct Closure : public FlexObject<Closure, ORef> {
    ORef method;

    static HRef<Type> reify(struct State const& state);

    Slice<ORef const> clovers() const { return flexItems(); }
};

struct Multimethod : public FixedObject<Multimethod> {
    HRef<Array> methods;
    ORef maybeName;

    static HRef<Type> reify(struct State const& state);
};

struct Continuation : FlexObject<Continuation, ORef> {
    ORef method;
    Fixnum pc;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    Slice<ORef const> saves() const { return flexItems(); }
};

/// FIXME: Should have zero size but a byte is forced upon us :(
struct Unbound : public FixedObject<Unbound> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct Var : public FixedObject<Var> {
    ORef val;
    ORef macroCategory;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct Knot : public FixedObject<Knot> {
    ORef val;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct Namespace : public FixedObject<Namespace> {
    HRef<ArrayMut> keys;
    HRef<ArrayMut> vals;
    Fixnum count;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

/// FIXME: Should have zero size but a byte is forced upon us :(
struct End : public FixedObject<End> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct InputFile : public FixedObject<InputFile> {
    UTF8InputFile file;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    InputFile(UTF8InputFile&& t_file) : file{std::move(t_file)} {}

    static bool open(State* state, HRef<InputFile>& res, HRef<String> filename);
};

// TODO: Eliminate all the other error types:
struct FatalError : public FlexObject<FatalError, ORef> {
    HRef<Symbol> name;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    Slice<ORef const> irritants() const { return flexItems(); }
};

struct UnboundError : public FixedObject<UnboundError> {
    HRef<Symbol> name;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct TypeError : public FixedObject<TypeError> {
    HRef<Type> type;
    ORef val;

    static HRef<Type> reify(struct State const& state);
};

struct ArityError : public FixedObject<ArityError> {
    HRef<Closure> callee;
    Fixnum callArgc;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct InapplicableError : public FixedObject<InapplicableError> {
    HRef<Multimethod> callee;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

} // namespace
