#include "value.hpp"

#include "state.hpp"

namespace {

// TODO: Make this work in non-unity builds (no practical need for that (yet?) though):
template<typename T>
void SlotMut<T>::set(State& state, T v) {
    if (!state.heap.writeBarrier(&*oref_)) { // OPTIMIZE: tag(in ctor)-untag(here)
        auto const orefG_ = state.pushRoot(&oref_);
        auto const vG = state.pushRoot(&v);
        collect(&state);
        slot_ = reinterpret_cast<T*>(reinterpret_cast<char*>(&*oref_) + offset_);
    }

    *slot_ = v;
}

HRef<Type> Flonum::reify(State const& state) { return state.types.flonum; }

HRef<Type> Fixnum::reify(State const& state) { return state.types.fixnum; }

HRef<Type> Char::reify(State const& state) { return state.types.charr; }

HRef<Type> Bool::reify(State const& state) { return state.types.booll; }

HRef<Type> Type::reify(State const& state) { return state.types.type; }

HRef<Type> String::reify(State const& state) { return state.types.string; }

HRef<Type> StringIterator::reify(State const& state) { return state.types.stringIterator; }

HRef<Type> Symbol::reify(State const& state) { return state.types.symbol; }

HRef<Type> Array::reify(State const& state) { return state.types.array; }

HRef<Type> ArrayMut::reify(State const& state) { return state.types.arrayMut; }

HRef<Type> ByteArray::reify(State const& state) { return state.types.byteArray; }

HRef<Type> ByteArrayMut::reify(State const& state) { return state.types.byteArrayMut; }

HRef<Type> Loc::reify(State const& state) { return state.types.loc; }

HRef<Type> Pair::reify(State const& state) { return state.types.pair; }

HRef<Type> EmptyList::reify(State const& state) { return state.types.emptyList; }

HRef<Type> Method::reify(State const& state) { return state.types.method; }

HRef<Type> Closure::reify(State const& state) { return state.types.closure; }

HRef<Type> Multimethod::reify(State const& state) { return state.types.multimethod; }

HRef<Type> Continuation::reify(State const& state) { return state.types.continuation; }

HRef<Type> Unbound::reify(State const& state) { return state.types.unbound; }

HRef<Type> Var::reify(State const& state) { return state.types.var; }

HRef<Type> Knot::reify(State const& state) { return state.types.knot; }

HRef<Type> Namespace::reify(State const& state) { return state.types.ns; }

HRef<Type> End::reify(State const& state) { return state.types.end; }

HRef<Type> InputFile::reify(State const& state) { return state.types.inputFile; }

HRef<Type> FatalError::reify(State const& state) { return state.types.fatalError; }

HRef<Type> UnboundError::reify(State const& state) { return state.types.unboundError; }

HRef<Type> TypeError::reify(State const& state) { return state.types.typeError; }

HRef<Type> ArityError::reify(State const& state) { return state.types.arityError; }

HRef<Type> InapplicableError::reify(State const& state) {
    return state.types.inapplicableError;
}

bool InputFile::open(State* state, HRef<InputFile>& res, HRef<String> filename) {
    UTF8InputFile file;
    if (!UTF8InputFile::open(file, filename->str())) { return false; }

    res = createInputFile(state, std::move(file));
    return true;
}

} // namespace
