#include "namespace.hpp"

#include "state.hpp"

namespace {

Var* tryCreateUnboundVar(Semispace* semispace, Type const* varType, HRef<Unbound> unbound) {
    Var* ptr = (Var*)semispace->tryAlloc(varType);
    if (!ptr) { return ptr; }

    *ptr = Var{.val = unbound, .macroCategory = False};

    return ptr;
}

HRef<Var> createUnboundVar(State* state) {
    Var* ptr = (Var*)state->heap.tospace.tryAlloc(&*state->types.var);
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Var*)state->heap.tospace.allocOrDie(&*state->types.var);
    }

    *ptr = Var{.val = state->singletons.unbound, .macroCategory = False};

    return HRef<Var>(ptr);
}

FindVarRes findVar(HRef<Namespace> ns, HRef<Symbol> name) {
    ORef const* keys = ns->keys->flexDataMut();
    size_t const h = (uintptr_t)name->hash.val();

    size_t const maxIdx = (uint64_t)ns->keys->flexCount().val() - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        ORef const k = keys[i];
        if (eq(k, name)) {
            HRef<Var> const var = HRef<Var>::fromUnchecked(ns->vals->flexDataMut()[i]);
            return FindVarRes{.type = FindVarRes::NS_FOUND_VAR, .var = var};
        } else if (eq(k, Default)) {
            return FindVarRes{.type = FindVarRes::NS_FOUND_VAR_DEST_IDX, .destIndex = i};
        }
    }
}

void rehashNamespace(State* state, HRef<Namespace> const* nsHandle) {
    size_t const oldCap = (uint64_t)(*nsHandle)->keys->flexCount().val();
    size_t const newCap = oldCap << 1;
    HRef<ArrayMut> newKeysRef = createArrayMut(state, Fixnum((intptr_t)newCap)); // May GC
    auto const newKeysRefG = state->pushRoot(&newKeysRef);
    HRef<ArrayMut> const newValsRef = createArrayMut(state, Fixnum((intptr_t)newCap)); // May GC

    HRef<Namespace> const ns = *nsHandle;
    ORef* const oldKeys = ns->keys->flexDataMut();
    ORef* const oldVals = ns->vals->flexDataMut();
    ORef* const newKeys = newKeysRef->flexDataMut();
    ORef* const newVals = newValsRef->flexDataMut();
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

    ns->keys = newKeysRef;
    ns->vals = newValsRef;
}

HRef<Var> getVar(State* state, HRef<Namespace> ns, HRef<Symbol> name) {
    FindVarRes findRes = findVar(ns, name);
    switch (findRes.type) {
    case FindVarRes::NS_FOUND_VAR: return findRes.var;

    case FindVarRes::NS_FOUND_VAR_DEST_IDX: {
        size_t const newCount = (uintptr_t)ns->count.val() + 1;
        size_t const cap = (uint64_t)ns->keys->flexCount().val();

        auto const nsRefG = state->pushRoot(&ns);
        auto const nameG = state->pushRoot(&name);

        if (newCount > cap >> 1) {
            rehashNamespace(state, &ns); // May GC
        }

        HRef<Var> const var = createUnboundVar(state); // May GC

        findRes = findVar(ns, name);
        assert(findRes.type == FindVarRes::NS_FOUND_VAR_DEST_IDX);
        ns->keys->flexDataMut()[findRes.destIndex] = name;
        ns->vals->flexDataMut()[findRes.destIndex] = var;
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
