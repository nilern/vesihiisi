#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <bit>
#include <span>

#include "util/util.hpp"
#include "vesihiisi.h"

namespace {

struct State;

static_assert(sizeof(void*) == sizeof(uint64_t)); // Only 64-bit supported (for now)
// Could support 32-bit now that we NaN-tag. If we get native threads the non-atomicity of 64-bit
// loads and stores would complicate synchronization primitives on 32-bit though.

inline bool eq(ORef x, ORef y) { return x.bits == y.bits; }

enum class TaggedType : uint64_t {
    HEAPED = 0,
    FIXNUM = 1,
    CHAR = 2,
    BOOL = 3,
    FLONUM = 4
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

/// Reference to `Object` of type `T`
template<typename T>
struct HRef : public ORef {
    constexpr explicit HRef(T* ptr) : HRef{heapedTag | (uint64_t)ptr} {}

    static HRef<T> fromUnchecked(ORef v) { return std::bit_cast<HRef<T>>(v); }

    T* operator->() const { return std::bit_cast<T*>(bits & payloadMask); }

    T& operator*() const { return *operator->(); }

private:
    constexpr explicit HRef(uint64_t t_bits) : ORef{t_bits} {}
};

/// Access to mutable slot (of type `ORef` or `HRef<U>`) with write barrier. Because GC could move
/// the underlying slot, to be safe always use this as a temporary e.g. `obj.slot().get/set(...`.
template<typename T>
class SlotMut {
    T* slot_;
    HRef<struct Object> oref_;
    ptrdiff_t offset_;
public:
    SlotMut(struct Object* obj, T& slot) :
        slot_{&slot}, oref_{obj},
        offset_{reinterpret_cast<char*>(slot_) - reinterpret_cast<char*>(obj)}
    {}

    /// Get value of slot.
    T get() const { return *slot_; }

    /// Set slot (with write barrier).
    void set(State& state, T v);
};

/// Just to allow `const` values of types with `SlotMut`s e.g.
///
///     SlotMut<U> slot();
///     Slot<U> slot() const;
template<typename T>
class Slot {
    T const* slot_;
public:
    explicit Slot(T const& slot) : slot_{&slot} {}

    T const& get() const { return *slot_; }
};

/// Effectively `ORefSpanMut` without range checking.
template<typename T>
class SlotsMut {
    struct Object* obj_;
    T* slots_;
public:
    explicit SlotsMut(struct Object* obj, T* slots) : obj_{obj}, slots_{slots} {}

    SlotMut<T> operator[](size_t i) { return SlotMut{obj_, slots_[i]}; }
    Slot<T> operator[](size_t i) const { return Slot{slots_[i]}; }
};

using ORefSpan = std::span<ORef const>;

/// The moral equivalent of `SlotMut` for an indexed slot. Usage: `obj.items()[i].get/set(...`.
class ORefSpanMut {
    struct Object* obj_;
    std::span<ORef> span_;

public:
    ORefSpanMut(struct Object* obj, ORef* begin, size_t count) : obj_{obj}, span_{begin, count} {}

    size_t size() const { return span_.size(); }

    SlotsMut<ORef> data() { return SlotsMut{obj_, span_.data()}; }

    SlotMut<ORef> operator[](size_t i) { return SlotMut{obj_, span_[i]}; }
};

struct Scalar : public ORef {
    constexpr explicit Scalar(uint64_t t_bits) : ORef{t_bits} {}

    // TODO: Remove now that `Scalar : public ORef`:
    ORef oref() const { return ORef{bits}; }
};
static_assert(sizeof(Scalar) == sizeof(ORef));

struct Fixnum : public Scalar {
    static HRef<struct Type> reify(struct State const& state);

    static bool contains(ORef v) { return (v.bits & tagMask) == fixnumTag; }

    static bool contains(struct State const& /*state*/, ORef v) { return contains(v); }

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
    [[maybe_unused]] // TODO: certainly use
    static HRef<struct Type> reify(struct State const& state);

    static bool contains(ORef v) {
        return v.bits == nonFlonumTag // Actual NaN
            || (v.bits & nonFlonumTag) != nonFlonumTag;
    }

    static bool contains(struct State const& /*state*/, ORef v) { return contains(v); }

    constexpr explicit Flonum(double n) : Flonum{std::bit_cast<uint64_t>(n)} {}

    static Flonum fromUnchecked(ORef v) { return Flonum{v.bits}; }

    double val() const { return std::bit_cast<double>(bits); }

private:
    constexpr explicit Flonum(uint64_t t_bits) : Scalar{t_bits} {}
};

struct Char : public Scalar {
    [[maybe_unused]] // TODO: certainly use
    static HRef<struct Type> reify(struct State const& state);

    static bool contains(ORef v) {
        return (v.bits & tagMask) == charTag;
    }

    static bool contains(struct State const& /*state*/, ORef v) { return contains(v); }

    constexpr explicit Char(uint32_t c) : Char{charTag | uint64_t(c)} {}

    static Char fromUnchecked(ORef v) { return Char{v.bits}; }

    uint32_t val() const { return uint32_t(bits & payloadMask); }

private:
    constexpr explicit Char(uint64_t t_bits) : Scalar{t_bits} {}
};

struct Bool : public Scalar {
    static HRef<struct Type> reify(struct State const& state);

    static bool contains(ORef v) {
        return (v.bits & tagMask) == boolTag;
    }

    static bool contains(struct State const& /*state*/, ORef v) { return contains(v); }

    constexpr explicit Bool(bool b) : Bool{boolTag | (uint64_t)b} {}

    static Bool fromUnchecked(ORef v) { return Bool{v.bits}; }

    bool val() const { return (bool)(bits & payloadMask); }

private:
    constexpr explicit Bool(uint64_t t_bits) : Scalar{t_bits} {}
};

inline bool isHeaped(ORef v) {
    return v.bits != nonFlonumTag // Not an actual NaN
        && (v.bits & tagMask) == heapedTag;
}

inline TaggedType getTag(ORef v) {
    if (Flonum::contains(v)) { return TaggedType::FLONUM; }
    return static_cast<TaggedType>((v.bits >> payloadWidth) & 0b11);
}

constexpr ORef Default{0}; // 0.0

constexpr Bool True{true};
constexpr Bool False{false};

// Just needs to be distinguishable from both `Header` and flex count fixnum. Using 0.0 has the
// added advantage of requiring no initialization on allocation (as heap is already zeroed):
constexpr ORef AlignmentHole{0};

constexpr ORef Tombstone{boolTag | (uint64_t)false};

struct Object {
    struct Header const* header() const;
    struct Header* header();

    Object* tryForwarded() const;
    Object* canonical() {
        Object* obj = this;
        for (Object* fwdPtr = nullptr; (fwdPtr = obj->tryForwarded()); obj = fwdPtr) {}
        return obj;
    }

    void forwardTo(Object* copy);
};

inline ORef tagHeaped(Object* ptr) { return ORef{heapedTag | (uint64_t)ptr}; }

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
    std::span<Item const> flexItems() const {
        return std::span{
            static_cast<CRTPSub const*>(this)->flexData(),
            static_cast<size_t>(flexCount().val())
        };
    }
};

template<typename CRTPSub, typename Item>
struct AnyIndexedMutObject : public AnyIndexedObject<CRTPSub, Item> {
    // TODO: Align result `.data` if we go beyond 'either all slots or all bytes':
    std::span<Item> flexItemsMut() {
        return std::span{
            const_cast<Item*>(static_cast<CRTPSub*>(this)->flexData()),
            static_cast<size_t>(this->flexCount().val())
        };
    }
};

template<typename CRTPSub>
struct AnyIndexedMutObject<CRTPSub, ORef> : public AnyIndexedObject<CRTPSub, ORef> {
    // TODO: Align result `.data` if we go beyond 'either all slots or all bytes':
    ORefSpanMut flexItemsMut() {
        return ORefSpanMut{
            this,
            const_cast<ORef*>(static_cast<CRTPSub*>(this)->flexData()),
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
};

struct Type : public FixedObject<Type> {
    Fixnum const minSize;
    Fixnum const align;
    Bool const isBytes;
    Bool const hasCodePtr;
    Bool const isFlex;
    Fixnum const hash;
    HRef<struct Symbol> const name;

    Type(
        Fixnum t_minSize, Fixnum t_align, Bool t_isBytes, Bool t_hasCodePtr, Bool t_isFlex,
        Fixnum t_hash, HRef<struct Symbol> t_name
    ) :
        minSize{t_minSize}, align{t_align}, isBytes{t_isBytes}, hasCodePtr{t_hasCodePtr},
        isFlex{t_isFlex}, hash{t_hash}, name{t_name}
    {}

    Type(Type const& that) :
        minSize{that.minSize}, align{that.align}, isBytes{that.isBytes},
        hasCodePtr{that.hasCodePtr}, isFlex{that.isFlex}, hash{that.hash}, name{that.name}
    {}

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

FlexHeader const* uncheckedFlexHeader(HRef<Object> v) {
    return std::bit_cast<FlexHeader const*>(&*v) - 1;
}

// TODO: Align result if we go beyond 'either all slots or all bytes':
void const* uncheckedUntypedFlexPtr(HRef<Object> v) {
    Object const* const obj = &*v;
    size_t const minSize = (uint64_t)obj->header()->typePtr()->minSize.val();
    return static_cast<void const*>(std::bit_cast<char const*>(obj) + minSize);
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
    ORef const string;
private:
    ORef byteIdx_;
public:
    ORef byteIdx() { return byteIdx_; }
    void setByteIdx(Fixnum idx) { byteIdx_ = idx; }

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct Symbol : public FlexObject<Symbol, uint8_t> {
    Fixnum const hash;

    Symbol(Fixnum t_hash, Str name) : hash{t_hash} {
        memcpy(const_cast<uint8_t*>(flexData()), name.data, name.len);
    }

    static HRef<Type> reify(struct State const& state);

    Str name() const { return Str{flexData(), static_cast<size_t>(flexCount().val())}; }
};

// TODO: `template<typename T> struct Array<T> :`?
struct Array : public IndexedObject<Array, ORef> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    ORefSpan items() const { return flexItems(); }
};

// TODO: `template<typename T> struct Array<T> :`?
struct ArrayMut : public IndexedMutObject<ArrayMut, ORef> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    ORefSpan items() const { return flexItems(); }
    ORefSpanMut itemsMut() { return flexItemsMut(); }
};

struct ByteArray : public IndexedObject<ByteArray, uint8_t> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    std::span<uint8_t const> items() const { return flexItems(); }
};

struct ByteArrayMut : public IndexedMutObject<ByteArrayMut, uint8_t> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    std::span<uint8_t const> items() const { return flexItems(); }
    std::span<uint8_t> itemsMut() { return flexItemsMut(); }
};

struct Loc : public FixedObject<Loc> {
    HRef<String> const filename;
    Fixnum const byteIdx;

    Loc(HRef<String> t_filename, Fixnum t_byteIdx) : filename{t_filename}, byteIdx{t_byteIdx} {}

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

class Pair : public FixedObject<Pair> {
    ORef car_;
    ORef cdr_;
    ORef maybeLoc_;
public:
    SlotMut<ORef> car() { return SlotMut{this, car_}; }
    SlotMut<ORef> cdr() { return SlotMut{this, cdr_}; }
    SlotMut<ORef> maybeLoc() { return SlotMut{this, maybeLoc_}; }

    Pair(ORef car, ORef cdr, ORef maybeLoc) : car_{car}, cdr_{cdr}, maybeLoc_{maybeLoc} {}

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
    MethodCode const nativeCode;
    ORef const code;
    ORef const consts;
    Bool const hasVarArg;
    Fixnum const hash;
    ORef const maybeName;
    ORef const maybeFilenames;
    ORef const maybeSrcByteIdxs;

    Method(
        MethodCode t_nativeCode, ORef t_code, ORef t_consts, Bool t_hasVarArg, Fixnum t_hash,
        ORef t_maybeName, ORef t_maybeFilenames, ORef t_maybeSrcByteIdxs, ORefSpan t_domain
    ) :
        nativeCode{t_nativeCode}, code{t_code}, consts{t_consts}, hasVarArg{t_hasVarArg},
        hash{t_hash}, maybeName{t_maybeName}, maybeFilenames{t_maybeFilenames},
        maybeSrcByteIdxs{t_maybeSrcByteIdxs}
    {
        memcpy(const_cast<ORef*>(flexData()), t_domain.data(), t_domain.size_bytes());
    }

    static HRef<Type> reify(struct State const& state);

    ORefSpan domain() const { return flexItems(); }
    ORefSpanMut domain() { return flexItemsMut(); }
};

struct Closure : public FlexObject<Closure, ORef> {
    ORef const method;

    static HRef<Type> reify(struct State const& state);

    ORefSpan clovers() const { return flexItems(); }
};

class Multimethod : public FixedObject<Multimethod> {
    HRef<Array> methods_;
public:
    ORef const maybeName;

    Slot<HRef<Array>> methods() const { return Slot{methods_}; }
    SlotMut<HRef<Array>> methods() { return SlotMut{this, methods_}; }

    static HRef<Type> reify(struct State const& state);
};

struct Continuation : FlexObject<Continuation, ORef> {
    ORef const method;
    Fixnum const pc;

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    ORefSpan saves() const { return flexItems(); }
};

/// FIXME: Should have zero size but a byte is forced upon us :(
struct Unbound : public FixedObject<Unbound> {
    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

class Var : public FixedObject<Var> {
    ORef val_;
    ORef macroCategory_;
public:
    SlotMut<ORef> val() { return SlotMut{this, val_}; }

    Var(ORef val, ORef macroCategory) : val_{val}, macroCategory_{macroCategory} {}

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

class Knot : public FixedObject<Knot> {
    ORef val_;
public:
    SlotMut<ORef> val() { return SlotMut{this, val_}; }

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

class Namespace : public FixedObject<Namespace> {
    HRef<ArrayMut> keys_;
    HRef<ArrayMut> vals_; // OPTIMIZE: Colocate with keys
public:
    Fixnum count;

    Namespace(HRef<ArrayMut> keys, HRef<ArrayMut> vals, Fixnum t_count) :
        keys_{keys}, vals_{vals}, count{t_count} {}

    SlotMut<HRef<ArrayMut>> keys() { return SlotMut{this, keys_}; }
    SlotMut<HRef<ArrayMut>> vals() { return SlotMut{this, vals_}; }

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

    explicit InputFile(UTF8InputFile&& t_file) : file{std::move(t_file)} {}

    static bool open(State* state, HRef<InputFile>& res, HRef<String> filename);
};

// TODO: Eliminate all the other error types:
struct FatalError : public FlexObject<FatalError, ORef> {
    HRef<Symbol> const name;

    FatalError(HRef<Symbol> t_name, ORefSpan irritants) : name{t_name} {
        memcpy(const_cast<ORef*>(flexData()), irritants.data(), irritants.size_bytes());
    }

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);

    ORefSpan irritants() const { return flexItems(); }
};

struct UnboundError : public FixedObject<UnboundError> {
    HRef<Symbol> const name;

    explicit UnboundError(HRef<Symbol> t_name) : name{t_name} {}

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct TypeError : public FixedObject<TypeError> {
    HRef<Type> const type;
    ORef const val;

    TypeError(HRef<Type> t_type, ORef t_val) : type{t_type}, val{t_val} {}

    static HRef<Type> reify(struct State const& state);
};

struct ArityError : public FixedObject<ArityError> {
    HRef<Closure> const callee;
    Fixnum const callArgc;

    ArityError(HRef<Closure> t_callee, Fixnum t_callArgc) :
        callee{t_callee}, callArgc{t_callArgc} {}

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

struct InapplicableError : public FixedObject<InapplicableError> {
    HRef<Multimethod> const callee;

    explicit InapplicableError(HRef<Multimethod> t_callee) : callee{t_callee} {}

    [[maybe_unused]]
    static HRef<Type> reify(struct State const& state);
};

} // namespace
