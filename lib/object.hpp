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

struct Scalar {
    constexpr explicit Scalar(uint64_t t_bits) : bits{t_bits} {}

    ORef oref() const { return ORef{bits}; }

protected:
    uint64_t bits;
};
static_assert(sizeof(Scalar) == sizeof(ORef));

struct Fixnum : public Scalar {
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
    constexpr explicit Flonum(double n) : Flonum{std::bit_cast<uint64_t>(n)} {}

    static Flonum fromUnchecked(ORef v) { return Flonum{v.bits}; }

    double val() const { return std::bit_cast<double>(bits); }

private:
    constexpr explicit Flonum(uint64_t t_bits) : Scalar{t_bits} {}
};

struct Char : public Scalar {
    constexpr explicit Char(char c) : Char{charTag | (uint64_t)c} {}

    static Char fromUnchecked(ORef v) { return Char{v.bits}; }

    char val() const { return (char)(bits & payloadMask); }

private:
    constexpr explicit Char(uint64_t t_bits) : Scalar{t_bits} {}
};

struct Bool : public Scalar {
    constexpr explicit Bool(bool b) : Bool{boolTag | (uint64_t)b} {}

    static Bool fromUnchecked(ORef v) { return Bool{v.bits}; }

    bool val() const { return (bool)(bits & payloadMask); }

private:
    constexpr explicit Bool(uint64_t t_bits) : Scalar{t_bits} {}
};

inline bool isFixnum(ORef v) { return (v.bits & tagMask) == fixnumTag; }

inline bool isFlonum(ORef v) {
    return v.bits == nonFlonumTag // Actual NaN OPTIMIZE: Is this necessary?
           || (v.bits & nonFlonumTag) != nonFlonumTag; // OPTIMIZE: Do not short-circuit?
}

inline bool isChar(ORef v) { return (v.bits & tagMask) == charTag; }

inline bool isBool(ORef v) { return (v.bits & tagMask) == boolTag; }

inline bool isHeaped(ORef v) {
    return v.bits != nonFlonumTag // Not a NaN OPTIMIZE: Is this necessary?
           && (v.bits & tagMask) == heapedTag; // OPTIMIZE: Do not short-circuit?
}

inline TaggedType getTag(ORef v) {
    if (isHeaped(v)) { return TaggedType::HEAPED; }
    if (isFlonum(v)) { return TaggedType::FLONUM; }
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

inline char uncheckedORefToChar(ORef v) { return Char::fromUnchecked(v).val(); }

struct Object {
    struct Header const* header() const;
    struct Header* header();

    Object* tryForwarded() const;

    void forwardTo(Object* copy);
};

/// GC-heap object without a flex field
struct FixedObject : public Object {};

/// GC-heap object with a flex field
template<typename CRTPSub, typename Item>
struct AnyIndexedObject : public Object {
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

/// Reference to `Object` of type `T`
template<typename T>
struct HRef {
    constexpr explicit HRef(T* ptr) : HRef{heapedTag | (uint64_t)ptr} {}

    ORef oref() const { return ORef{bits}; }

    static HRef<T> fromUnchecked(ORef v) { return HRef{v.bits}; }

    T* ptr() const { return std::bit_cast<T*>(bits & payloadMask); }

private:
    constexpr explicit HRef(uint64_t t_bits) : bits{t_bits} {}

    uint64_t bits;
};

inline ORef tagHeaped(Object* ptr) { return ORef{heapedTag | (uint64_t)ptr}; }

inline Object* uncheckedORefToPtr(ORef v) { return (Object*)(v.bits & payloadMask); }

inline Object* tryORefToPtr(ORef oref) {
    return isHeaped(oref) ? uncheckedORefToPtr(oref) : nullptr;
}

struct Type {
    Fixnum minSize;
    Fixnum align;
    Bool isBytes;
    Bool hasCodePtr;
    Bool isFlex;
    Fixnum hash;
    HRef<struct Symbol> name;
};

struct Header {
    explicit Header(Type const* type) : Header{std::bit_cast<uint64_t>(type)} {}

    // Cannot work on broken hearts (as they have relocation pointer instead):
    HRef<Type> type() const {
        return HRef{std::bit_cast<Type*>(heapedTag | (bits & payloadMask))};
    }

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
    Object const* const obj = HRef<Object>::fromUnchecked(v).ptr();
    size_t const minSize = (uint64_t)obj->header()->type().ptr()->minSize.val();
    return static_cast<void const*>(std::bit_cast<char const*>(obj) + minSize);
}

// TODO: Align result if we go beyond 'either all slots or all bytes':
void* uncheckedUntypedFlexPtrMut(ORef v) {
    Object* const obj = HRef<Object>::fromUnchecked(v).ptr();
    size_t const minSize = (uint64_t)obj->header()->type().ptr()->minSize.val();
    return static_cast<void*>(std::bit_cast<char*>(obj) + minSize);
}

template<typename CRTPSub, typename Item>
FlexHeader const* AnyIndexedObject<CRTPSub, Item>::flexHeader() const {
    return std::bit_cast<FlexHeader const*>(this) - 1;
}

template<typename CRTPSub, typename Item>
Fixnum AnyIndexedObject<CRTPSub, Item>::flexCount() const { return flexHeader()->count; }

struct String : IndexedObject<String, char> {
    Str str() const { return Str{flexData(), static_cast<size_t>(flexCount().val())}; }
};

struct Symbol : public FlexObject<Symbol, char> {
    Fixnum hash;

    Str name() const { return Str{flexData(), static_cast<size_t>(flexCount().val())}; }
};

// TODO: `template<typename T> struct Array<T> :`?
struct Array : public IndexedObject<Array, ORef> {
    Slice<ORef const> items() const { return flexItems(); }
};

// TODO: `template<typename T> struct Array<T> :`?
struct ArrayMut : public IndexedMutObject<ArrayMut, ORef> {
    Slice<ORef const> items() const { return flexItems(); }
    Slice<ORef> itemsMut() { return flexItemsMut(); }
};

struct ByteArray : public IndexedObject<ByteArray, uint8_t> {
    Slice<uint8_t const> items() const { return flexItems(); }
};

struct Pair : public FixedObject {
    ORef car;
    ORef cdr;
};

/// FIXME: Should have zero size but a byte is forced upon us :(
struct EmptyList : public FixedObject {};

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

    Slice<ORef const> domain() const { return flexItems(); }
    Slice<ORef> domain() { return flexItemsMut(); }
};

struct Closure : public FlexObject<Closure, ORef> {
    ORef method;

    Slice<ORef const> clovers() const { return flexItems(); }
};

struct Multimethod : public FixedObject {
    HRef<Array> methods;
    // TODO: ORef maybeName
};

struct Continuation : FlexObject<Continuation, ORef> {
    ORef method;
    Fixnum pc;

    Slice<ORef const> saves() const { return flexItems(); }
};

/// FIXME: Should have zero size but a byte is forced upon us :(
struct Unbound : public FixedObject {};

struct Var : public FixedObject {
    ORef val;
};

struct Knot : public FixedObject {
    ORef val;
};

struct Namespace : public FixedObject {
    HRef<ArrayMut> keys;
    HRef<ArrayMut> vals;
    Fixnum count;
};

// TODO: Eliminate all the other error types:
struct FatalError : public FlexObject<FatalError, ORef> {
    HRef<Symbol> name;

    Slice<ORef const> irritants() const { return flexItems(); }
};

struct UnboundError : public FixedObject {
    HRef<Symbol> name;
};

struct TypeError : public FixedObject {
    HRef<Type> type;
    ORef val;
};

struct ArityError : public FixedObject {
    HRef<Closure> callee;
    Fixnum callArgc;
};

struct InapplicableError : public FixedObject {
    HRef<Multimethod> callee;
};

} // namespace
