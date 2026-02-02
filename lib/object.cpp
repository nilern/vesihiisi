#include "object.hpp"

#include "state.hpp"

namespace {

HRef<Type> Type::reify(struct State const& state) { return state.types.type; }

HRef<Type> String::reify(struct State const& state) { return state.types.string; }

HRef<Type> StringIterator::reify(struct State const& state) { return state.types.stringIterator; }

HRef<Type> Symbol::reify(struct State const& state) { return state.types.symbol; }

HRef<Type> Array::reify(struct State const& state) { return state.types.array; }

HRef<Type> ArrayMut::reify(struct State const& state) { return state.types.arrayMut; }

HRef<Type> ByteArray::reify(struct State const& state) { return state.types.byteArray; }

HRef<Type> ByteArrayMut::reify(struct State const& state) { return state.types.byteArrayMut; }

HRef<Type> Loc::reify(struct State const& state) { return state.types.loc; }

HRef<Type> Pair::reify(struct State const& state) { return state.types.pair; }

HRef<Type> EmptyList::reify(struct State const& state) { return state.types.emptyList; }

HRef<Type> Method::reify(struct State const& state) { return state.types.method; }

HRef<Type> Closure::reify(struct State const& state) { return state.types.closure; }

HRef<Type> Multimethod::reify(struct State const& state) { return state.types.multimethod; }

HRef<Type> Continuation::reify(struct State const& state) { return state.types.continuation; }

HRef<Type> Unbound::reify(struct State const& state) { return state.types.unbound; }

HRef<Type> Var::reify(struct State const& state) { return state.types.var; }

HRef<Type> Knot::reify(struct State const& state) { return state.types.knot; }

HRef<Type> Namespace::reify(struct State const& state) { return state.types.ns; }

HRef<Type> End::reify(struct State const& state) { return state.types.end; }

HRef<Type> InputFile::reify(struct State const& state) { return state.types.inputFile; }

HRef<Type> FatalError::reify(struct State const& state) { return state.types.fatalError; }

HRef<Type> UnboundError::reify(struct State const& state) { return state.types.unboundError; }

HRef<Type> TypeError::reify(struct State const& state) { return state.types.typeError; }

HRef<Type> ArityError::reify(struct State const& state) { return state.types.arityError; }

HRef<Type> InapplicableError::reify(struct State const& state) {
    return state.types.inapplicableError;
}

bool InputFile::open(State* state, HRef<InputFile>& res, HRef<String> filename) {
    UTF8InputFile file;
    if (!UTF8InputFile::open(file, filename->str())) { return false; }

    res = createInputFile(state, std::move(file));
    return true;
}

} // namespace
