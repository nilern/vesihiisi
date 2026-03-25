#include "value.hpp"

#include "rt.hpp"

namespace {

// TODO: Make this work in non-unity builds (no practical need for that (yet?) though):
template<typename T>
void SlotMut<T>::set(RT& state, T v) {
    if (!state.heap.writeBarrier(&*oref_)) { // OPTIMIZE: tag(in ctor)-untag(here)
        auto const orefG_ = state.pushRoot(&oref_);
        auto const vG = state.pushRoot(&v);
        collect(&state);
        slot_ = std::bit_cast<T*>(std::bit_cast<char*>(&*oref_) + offset_);
    }

    *slot_ = v;
}

HRef<Type> Flonum::reify(RT const& state) { return state.types.flonum; }

HRef<Type> Fixnum::reify(RT const& state) { return state.types.fixnum; }

HRef<Type> Char::reify(RT const& state) { return state.types.charr; }

HRef<Type> Bool::reify(RT const& state) { return state.types.booll; }

HRef<Type> Type::reify(RT const& state) { return state.types.type; }

HRef<Type> String::reify(RT const& state) { return state.types.string; }

HRef<Type> StringIterator::reify(RT const& state) { return state.types.stringIterator; }

HRef<Type> Symbol::reify(RT const& state) { return state.types.symbol; }

HRef<Symbol> Symbol::gensym(RT& rt) {
    Object* ptr = rt.heap.tryAlloc(&*reify(rt));
    if (mustCollect(ptr)) {
        collect(&rt);
        ptr = rt.heap.allocOrDie(&*reify(rt));
    }

    int const hash = rand(); // FIXME: Sloppy C `rand()`
    auto const fxHash = Fixnum{int64_t{hash}};
    return HRef{new (static_cast<Symbol*>(ptr)) Symbol{fxHash, Default}};
}

HRef<Type> Array::reify(RT const& state) { return state.types.array; }

HRef<Type> ArrayMut::reify(RT const& state) { return state.types.arrayMut; }

HRef<Type> ByteArray::reify(RT const& state) { return state.types.byteArray; }

HRef<Type> ByteArrayMut::reify(RT const& state) { return state.types.byteArrayMut; }

HRef<Type> Loc::reify(RT const& state) { return state.types.loc; }

HRef<Type> Pair::reify(RT const& state) { return state.types.pair; }

HRef<Type> EmptyList::reify(RT const& state) { return state.types.emptyList; }

HRef<Type> Method::reify(RT const& state) { return state.types.method; }

HRef<Type> Closure::reify(RT const& state) { return state.types.closure; }

HRef<Type> Multimethod::reify(RT const& state) { return state.types.multimethod; }

HRef<Type> Continuation::reify(RT const& state) { return state.types.continuation; }

HRef<Type> Unbound::reify(RT const& state) { return state.types.unbound; }

HRef<Type> Var::reify(RT const& state) { return state.types.var; }

HRef<Type> Knot::reify(RT const& state) { return state.types.knot; }

HRef<Type> Namespace::reify(RT const& state) { return state.types.ns; }

HRef<Type> End::reify(RT const& state) { return state.types.end; }

HRef<Type> Pointer::reify(RT const& state) { return state.types.pointer; }

HRef<Pointer> Pointer::create(RT& rt, void* t_val) {
    Pointer* obj = static_cast<decltype(obj)>(rt.heap.tryAlloc(&*reify(rt)));
    if (mustCollect(obj)) {
        collect(&rt);
        obj = static_cast<decltype(obj)>(rt.heap.allocOrDie(&*reify(rt)));
    }

    return HRef{new (obj) Pointer{t_val}};
}

HRef<Type> InputFile::reify(RT const& state) { return state.types.inputFile; }

HRef<Type> FatalError::reify(RT const& state) { return state.types.fatalError; }

HRef<Type> UnboundError::reify(RT const& state) { return state.types.unboundError; }

HRef<Type> TypeError::reify(RT const& state) { return state.types.typeError; }

HRef<Type> ArityError::reify(RT const& state) { return state.types.arityError; }

HRef<Type> InapplicableError::reify(RT const& state) {
    return state.types.inapplicableError;
}

bool InputFile::open(RT* state, HRef<InputFile>& res, HRef<String> filename) {
    UTF8InputFile file;
    if (!UTF8InputFile::open(file, filename->str())) { return false; }

    res = createInputFile(state, std::move(file));
    return true;
}

} // namespace
