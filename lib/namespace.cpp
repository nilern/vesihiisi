#include "namespace.hpp"

#include "state.hpp"

namespace {

Var* tryCreateUnboundVar(Semispace* semispace, Type const* varType, HRef<Unbound> unbound) {
    Var* ptr = (Var*)semispace->tryAlloc(varType);
    if (!ptr) { return ptr; }

    return new (ptr) Var{unbound, False};
}

HRef<Var> createUnboundVar(State* state) {
    Var* ptr = (Var*)state->heap.tospace.tryAlloc(&*state->types.var);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Var*)state->heap.tospace.allocOrDie(&*state->types.var);
    }

    return HRef{new (ptr) Var{state->singletons.unbound, False}};
}

FindVarRes findVar(HRef<Namespace> ns, HRef<Symbol> name) {
    ORef const* keys = ns->keys().get()->flexData();
    size_t const h = (uintptr_t)name->hash.val();

    size_t const maxIdx = (uint64_t)ns->keys().get()->flexCount().val() - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        ORef const k = keys[i];
        if (eq(k, name)) {
            HRef<Var> const var = HRef<Var>::fromUnchecked(ns->vals().get()->flexData()[i]);
            return FindVarRes{.type = FindVarRes::NS_FOUND_VAR, .var = var};
        } else if (eq(k, Default)) {
            return FindVarRes{.type = FindVarRes::NS_FOUND_VAR_DEST_IDX, .destIndex = i};
        }
    }
}

void rehashNamespace(State* state, HRef<Namespace> const* nsHandle) {
    size_t const oldCap = (uint64_t)(*nsHandle)->keys().get()->flexCount().val();
    size_t const newCap = oldCap << 1;
    HRef<ArrayMut> newKeysRef = createArrayMut(state, Fixnum((intptr_t)newCap)); // May GC
    auto const newKeysRefG = state->pushRoot(&newKeysRef);
    HRef<ArrayMut> const newValsRef = createArrayMut(state, Fixnum((intptr_t)newCap)); // May GC

    HRef<Namespace> const ns = *nsHandle;
    ORef const* const oldKeys = ns->keys().get()->flexData();
    ORef const* const oldVals = ns->vals().get()->flexData();
    ORef* const newKeys = const_cast<ORef*>(newKeysRef->flexData()); // `const_cast` for init
    ORef* const newVals = const_cast<ORef*>(newValsRef->flexData()); // as above, so below
    for (size_t i = 0; i < oldCap; ++i) {
        ORef const k = oldKeys[i];
        if (!eq(k, Default)) {
            size_t const h = (uint64_t)HRef<Symbol>::fromUnchecked(k)->hash.val();

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const maybeK = newKeys + j;
                if (eq(*maybeK, Default)) {
                    *maybeK = k;
                    newVals[j] = oldVals[i];
                    break;
                }
            }
        }
    }

    ns->keys().set(*state, newKeysRef);
    ns->vals().set(*state, newValsRef);
}

HRef<Var> getVar(State* state, HRef<Namespace> ns, HRef<Symbol> name) {
    FindVarRes findRes = findVar(ns, name);
    switch (findRes.type) {
    case FindVarRes::NS_FOUND_VAR: return findRes.var;

    case FindVarRes::NS_FOUND_VAR_DEST_IDX: {
        size_t const newCount = (uintptr_t)ns->count.val() + 1;
        size_t const cap = (uint64_t)ns->keys().get()->flexCount().val();

        auto const nsRefG = state->pushRoot(&ns);
        auto const nameG = state->pushRoot(&name);

        if (newCount > cap >> 1) {
            rehashNamespace(state, &ns); // May GC
        }

        HRef<Var> const var = createUnboundVar(state); // May GC

        findRes = findVar(ns, name);
        assert(findRes.type == FindVarRes::NS_FOUND_VAR_DEST_IDX);
        ns->keys().get()->flexItemsMut()[findRes.destIndex].set(*state,  name);
        ns->vals().get()->flexItemsMut()[findRes.destIndex].set(*state, var);
        ns->count = Fixnum((intptr_t)newCount);

        return var;
    }

    default: {
        assert(false); // Unreachable
        return findRes.var; // HACK: Shuts up `control reaches end of non-void function`
    }
    }
}

} // namespace
