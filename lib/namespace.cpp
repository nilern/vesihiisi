#include "namespace.hpp"

#include "state.hpp"

namespace {

Var* tryCreateUnboundVar(Semispace* semispace, Type const* varType, HRef<Unbound> unbound) {
    Var* ptr = (Var*)semispace->tryAlloc(varType);
    if (!ptr) { return ptr; }

    *ptr = Var{.val = unbound.oref()};

    return ptr;
}

HRef<Var> createUnboundVar(State* state) {
    Var* ptr = (Var*)state->heap.tospace.tryAlloc(state->types.var.ptr());
    if (mustCollect(ptr)) {
        collect(state);
        ptr = (Var*)state->heap.tospace.allocOrDie(state->types.var.ptr());
    }

    *ptr = Var{.val = state->singletons.unbound.oref()};

    return HRef<Var>(ptr);
}

FindVarRes findVar(HRef<Namespace> nsRef, HRef<Symbol> name) {
    Namespace const* ns = nsRef.ptr();
    ORef const* keys = ns->keys.ptr()->flexDataMut();
    size_t const h = (uintptr_t)name.ptr()->hash.val();

    size_t const maxIdx = (uint64_t)ns->keys.ptr()->flexCount().val() - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        ORef const k = keys[i];
        if (eq(k, name.oref())) {
            HRef<Var> const var = HRef<Var>::fromUnchecked(ns->vals.ptr()->flexDataMut()[i]);
            return FindVarRes{.type = FindVarRes::NS_FOUND_VAR, .var = var};
        } else if (eq(k, Default)) {
            return FindVarRes{.type = FindVarRes::NS_FOUND_VAR_DEST_IDX, .destIndex = i};
        }
    }
}

void rehashNamespace(State* state, HRef<Namespace> const* nsHandle) {
    size_t const oldCap = (uint64_t)nsHandle->ptr()->keys.ptr()->flexCount().val();
    size_t const newCap = oldCap << 1;
    HRef<ArrayMut> newKeysRef = createArrayMut(state, Fixnum((intptr_t)newCap)); // May GC
    pushStackRoot(state, (ORef*)&newKeysRef);
    HRef<ArrayMut> const newValsRef = createArrayMut(state, Fixnum((intptr_t)newCap)); // May GC
    popStackRoots(state, 1);

    Namespace* const ns = nsHandle->ptr();
    ORef* const oldKeys = ns->keys.ptr()->flexDataMut();
    ORef* const oldVals = ns->vals.ptr()->flexDataMut();
    ORef* const newKeys = newKeysRef.ptr()->flexDataMut();
    ORef* const newVals = newValsRef.ptr()->flexDataMut();
    for (size_t i = 0; i < oldCap; ++i) {
        ORef const k = oldKeys[i];
        if (!eq(k, Default)) {
            size_t const h = (uint64_t)HRef<Symbol>::fromUnchecked(k).ptr()->hash.val();

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

HRef<Var> getVar(State* state, HRef<Namespace> nsRef, HRef<Symbol> name) {
    FindVarRes findRes = findVar(nsRef, name);
    switch (findRes.type) {
    case FindVarRes::NS_FOUND_VAR: return findRes.var;

    case FindVarRes::NS_FOUND_VAR_DEST_IDX: {
        Namespace* ns = nsRef.ptr();
        size_t const newCount = (uintptr_t)ns->count.val() + 1;
        size_t const cap = (uint64_t)ns->keys.ptr()->flexCount().val();

        pushStackRoot(state, (ORef*)&nsRef);
        pushStackRoot(state, (ORef*)&name);

        if (newCount > cap >> 1) {
            rehashNamespace(state, &nsRef); // May GC
        }

        HRef<Var> const var = createUnboundVar(state); // May GC

        popStackRoots(state, 2);

        ns = nsRef.ptr();
        findRes = findVar(nsRef, name);
        assert(findRes.type == FindVarRes::NS_FOUND_VAR_DEST_IDX);
        ns->keys.ptr()->flexDataMut()[findRes.destIndex] = name.oref();
        ns->vals.ptr()->flexDataMut()[findRes.destIndex] = var.oref();
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
