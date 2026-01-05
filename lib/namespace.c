#include "namespace.h"

#include "state.h"

static Var* tryCreateUnboundVar(Semispace* semispace, Type const* varType, UnboundRef unbound) {
    Var* ptr = tryAlloc(semispace, varType);
    if (!ptr) { return ptr; }

    *ptr = (Var){.val = toORef(unbound)};

    return ptr;
}

static VarRef createUnboundVar(State* state) {
    Var* ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->varType));
    if (mustCollect(ptr)) {
        collect(state);
        ptr = allocOrDie(&state->heap.tospace, typeToPtr(state->varType));
    }

    *ptr = (Var){.val = toORef(state->unbound)};

    return tagVar(ptr);
}

static FindVarRes findVar(NamespaceRef nsRef, SymbolRef name) {
    Namespace const* ns = namespaceToPtr(nsRef);
    ORef const* keys = arrayMutToPtr(ns->keys);
    size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(name)->hash);

    size_t const maxIdx = (uintptr_t)fixnumToInt(arrayMutCount(ns->keys)) - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        ORef const k = keys[i];
        if (eq(k, toORef(name))) {
            VarRef const var = uncheckedORefToVar(arrayMutToPtr(ns->vals)[i]);
            return (FindVarRes){NS_FOUND_VAR, .var = var};
        } else if (eq(k, Default)) {
            return (FindVarRes){NS_FOUND_VAR_DEST_IDX, .destIndex = i};
        }
    }
}

static void rehashNamespace(State* state, NamespaceRef const* nsHandle) {
    size_t const oldCap = (uintptr_t)fixnumToInt(arrayMutCount(namespaceToPtr(*nsHandle)->keys));
    size_t const newCap = oldCap << 1;
    ArrayMutRef newKeysRef = createArrayMut(state, tagInt((intptr_t)newCap)); // May GC
    pushStackRoot(state, (ORef*)&newKeysRef);
    ArrayMutRef const newValsRef = createArrayMut(state, tagInt((intptr_t)newCap)); // May GC
    popStackRoots(state, 1);

    Namespace* const ns = namespaceToPtr(*nsHandle);
    ORef* const oldKeys = arrayMutToPtr(ns->keys);
    ORef* const oldVals = arrayMutToPtr(ns->vals);
    ORef* const newKeys = arrayMutToPtr(newKeysRef);
    ORef* const newVals = arrayMutToPtr(newValsRef);
    for (size_t i = 0; i < oldCap; ++i) {
        ORef const k = oldKeys[i];
        if (!eq(k, Default)) {
            size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(uncheckedORefToSymbol(k))->hash);

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

static VarRef getVar(State* state, NamespaceRef nsRef, SymbolRef name) {
    FindVarRes findRes = findVar(nsRef, name);
    switch (findRes.type) {
    case NS_FOUND_VAR: return findRes.var;

    case NS_FOUND_VAR_DEST_IDX: {
        Namespace* ns = namespaceToPtr(nsRef);
        size_t const newCount = (uintptr_t)fixnumToInt(ns->count) + 1;
        size_t const cap = (uintptr_t)fixnumToInt(arrayMutCount(ns->keys));

        pushStackRoot(state, (ORef*)&nsRef);
        pushStackRoot(state, (ORef*)&name);

        if (newCount > cap >> 1) {
            rehashNamespace(state, &nsRef); // May GC
        }

        VarRef const var = createUnboundVar(state); // May GC

        popStackRoots(state, 2);

        ns = namespaceToPtr(nsRef);
        findRes = findVar(nsRef, name);
        assert(findRes.type == NS_FOUND_VAR_DEST_IDX);
        arrayMutToPtr(ns->keys)[findRes.destIndex] = toORef(name);
        arrayMutToPtr(ns->vals)[findRes.destIndex] = toORef(var);
        ns->count = tagInt((intptr_t)newCount);

        return var;
    }

    default: {
        assert(false); // Unreachable
        return findRes.var; // HACK: Shuts up `control reaches end of non-void function`
    }
    }
}
