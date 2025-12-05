static VarRef createUnboundVar(State* state) {
    Var* const ptr = tryAlloc(&state->heap.tospace, typeToPtr(state->varType));
    if (!ptr) { assert(false); } // TODO: Collect garbage here

    *ptr = (Var){.val = unboundToORef(state->unbound)};

    return tagVar(ptr);
}

typedef struct FindVarRes {
    enum {
        NS_FOUND_VAR,
        NS_FOUND_VAR_DEST_IDX
    } type;
    union {
        VarRef var;
        size_t destIndex;
    };
} FindVarRes;

static FindVarRes findVar(NamespaceRef nsRef, SymbolRef name) {
    Namespace const* ns = namespaceToPtr(nsRef);
    ORef const* keys = arrayToPtr(ns->keys);
    size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(name)->hash);

    size_t const maxIdx = (uintptr_t)fixnumToInt(arrayCount(ns->keys)) - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        ORef const k = keys[i];
        if (eq(k, symbolToORef(name))) {
            VarRef const var = uncheckedORefToVar(arrayToPtr(ns->vals)[i]);
            return (FindVarRes){NS_FOUND_VAR, .var = var};
        } else if (eq(k, fixnumToORef(Zero))) {
            return (FindVarRes){NS_FOUND_VAR_DEST_IDX, .destIndex = i};
        }
    }
}

static void rehashNamespace(State* state, NamespaceRef const* nsHandle) {
    size_t const oldCap = (uintptr_t)fixnumToInt(arrayCount(namespaceToPtr(*nsHandle)->keys));
    size_t const newCap = oldCap << 1;
    ArrayRef const* newKeysHandle =
        (ArrayRef*)pushTmp(state, arrayToORef(createArray(state, tagInt((intptr_t)newCap))));
    ArrayRef const newValsRef = createArray(state, tagInt((intptr_t)newCap));
    popTmp(state);

    Namespace* const ns = namespaceToPtr(*nsHandle);
    ORef* const oldKeys = arrayToPtr(ns->keys);
    ORef* const oldVals = arrayToPtr(ns->vals);
    ArrayRef const newKeysRef = *newKeysHandle;
    ORef* const newKeys = arrayToPtr(newKeysRef);
    ORef* const newVals = arrayToPtr(newValsRef);
    for (size_t i = 0; i < oldCap; ++i) {
        ORef const k = oldKeys[i];
        if (!eq(k, fixnumToORef(Zero))) {
            size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(uncheckedORefToSymbol(k))->hash);

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const maybeK = newKeys + j;
                if (eq(*maybeK, fixnumToORef(Zero))) {
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
        size_t const cap = (uintptr_t)fixnumToInt(arrayCount(ns->keys));

        NamespaceRef const* const nsHandle = (NamespaceRef*)pushTmp(state, namespaceToORef(nsRef));
        SymbolRef const* const nameHandle = (SymbolRef*)pushTmp(state, symbolToORef(name));

        if (newCount > cap >> 1) {
            rehashNamespace(state, nsHandle); // May GC
        }

        VarRef const var = createUnboundVar(state); // May GC

        nsRef = *nsHandle;
        ns = namespaceToPtr(nsRef);
        name = *nameHandle;
        popTmps(state, 2);
        findRes = findVar(nsRef, name);
        assert(findRes.type == NS_FOUND_VAR_DEST_IDX);
        arrayToPtr(ns->keys)[findRes.destIndex] = symbolToORef(name);
        arrayToPtr(ns->vals)[findRes.destIndex] = varToORef(var);
        ns->count = tagInt((intptr_t)newCount);

        return var;
    }

    default: {
        assert(false); // Unreachable
        return findRes.var; // HACK: Shuts up `control reaches end of non-void function`
    }
    }
}
