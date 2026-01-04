#include "primops.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "bytecode.h"
#include "dispatch.h"

static ORef getErrorHandler(State const* state) {
    ORef const v = varToPtr(state->errorHandler)->val;
    if (eq(v, unboundToORef(state->unbound))) {
        exit(EXIT_FAILURE); // FIXME
    }

    return v;
}

[[nodiscard]]
static PrimopRes primopError(State* state, ORef err) {
    state->regs[calleeReg] = getErrorHandler(state);
    state->regs[firstArgReg] = err;
    state->entryRegc = firstArgReg + 1;
    return PRIMOP_RES_TAILCALL;
}

static PrimopRes callBytecode(State* /*state*/) { return PRIMOP_RES_TAILCALL; }

static PrimopRes primopAbort(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const error = state->regs[firstArgReg];

    fputs("Runtime error: ", stderr);
    print(state, stderr, error);
    putc('\n', stderr);

    return PRIMOP_RES_ABORT;
}

static PrimopRes primopCallCC(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[calleeReg] = state->regs[firstArgReg];
    state->regs[firstArgReg] = state->regs[retContReg];
    state->entryRegc = firstArgReg + 1;
    return PRIMOP_RES_TAILCALL;
}

static PrimopRes primopContinue(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[retContReg] = state->regs[firstArgReg];
    state->regs[retReg] = state->regs[firstArgReg + 1];
    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopIdentical(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const x = state->regs[firstArgReg];
    ORef const y = state->regs[firstArgReg + 1];

    state->regs[retReg] = boolToORef(tagBool(eq(x, y)));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopMake(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    TypeRef typeRef = uncheckedORefToTypeRef(state->regs[firstArgReg]);
    uint8_t const callArity = state->entryRegc - firstArgReg;

    Type const* type = toPtr(typeRef);
    if (!unwrapBool(type->isFlex)) {
        // Alloc:
        void* ptr = tryAlloc(&state->heap.tospace, type);
        if (mustCollect(ptr)) {
            collect(state);
            typeRef = uncheckedORefToTypeRef(state->regs[firstArgReg]);
            type = toPtr(typeRef);
            ptr = allocOrDie(&state->heap.tospace, type);
        }

        // Init:
        if (!unwrapBool(type->isBytes)) {
            size_t const fieldCount = (uintptr_t)fixnumToInt(type->minSize) / sizeof(ORef);
            if (callArity - 1u != fieldCount) {
                exit(EXIT_FAILURE); // TODO: Proper error (but not really an arity error!)
            }

            {
                ORef* const fields = (ORef*)ptr;
                for (size_t i = 0; i < fieldCount; ++i) {
                    fields[i] = state->regs[firstArgReg + 1 + i];
                }
            }
        } else {
            exit(EXIT_FAILURE); // TODO:
        }

        state->regs[retReg] = tagHeaped(ptr);

        return PRIMOP_RES_CONTINUE;
    } else {
        exit(EXIT_FAILURE); // TODO
    }
}

static PrimopRes primopSlotGet(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];
    size_t const slotIdx = (uintptr_t)uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    Type const* type = toPtr(typeOf(state, v));
    if (!unwrapBool(type->isBytes)) {
        size_t const slotCount = (uintptr_t)fixnumToInt(type->minSize) / sizeof(ORef);
        if (slotIdx >= slotCount) {
            assert(false); // TODO: Proper bounds error
        }

        ORef const* const slots = (ORef const*)uncheckedORefToPtr(v);
        state->regs[retReg] = slots[slotIdx];
    } else {
        assert(false); // TODO
    }

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopMakeFlex(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    TypeRef typeRef = uncheckedORefToTypeRef(state->regs[firstArgReg]);
    Fixnum const count = uncheckedORefToFixnum(state->regs[firstArgReg + 1]);

    Type const* type = toPtr(typeRef);
    if (unwrapBool(type->isFlex)) {
        void* ptr = tryAllocFlex(&state->heap.tospace, type, count);
        if (mustCollect(ptr)) {
            collect(state);
            typeRef = uncheckedORefToTypeRef(state->regs[firstArgReg]);
            type = toPtr(typeRef);
            ptr = allocFlexOrDie(&state->heap.tospace, type, count);
        }

        state->regs[retReg] = tagHeaped(ptr);

        return PRIMOP_RES_CONTINUE;
    } else {
        exit(EXIT_FAILURE); // TODO
    }
}

static PrimopRes primopFlexCount(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];

    Type const* type = toPtr(typeOf(state, v));
    if (!unwrapBool(type->isFlex)) {
        assert(false); // TODO: Proper nonflex error
    }

    state->regs[retReg] = toORef(((FlexHeader const*)uncheckedORefToPtr(v) - 1)->length);

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFlexGet(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];
    intptr_t const i = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    Type const* type = toPtr(typeOf(state, v));
    if (!unwrapBool(type->isFlex)) {
        assert(false); // TODO: Proper nonflex error
    }
    if (unwrapBool(type->isBytes)) {
        assert(false); // TODO: Proper nonslots error
    }

    void const* const ptr = uncheckedORefToPtr(v);
    intptr_t const count = fixnumToInt(((FlexHeader const*)ptr - 1)->length);
    if (i < 0 || i >= count) {
        assert(false); // TODO: Proper bounds error
    }

    ORef const* const flexSlots = (ORef const*)((char const*)ptr + fixnumToInt(type->minSize));
    state->regs[retReg] = flexSlots[i];

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFlexCopy(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const dest = state->regs[firstArgReg];
    intptr_t const offsetS = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);
    ORef const src = state->regs[firstArgReg + 2];
    intptr_t const startS = uncheckedFixnumToInt(state->regs[firstArgReg + 3]);
    intptr_t const endS = uncheckedFixnumToInt(state->regs[firstArgReg + 4]);
    Type const* const destType = toPtr(typeOf(state, dest));
    Type const* const srcType = toPtr(typeOf(state, src));

    if (!unwrapBool(destType->isFlex)) { exit(EXIT_FAILURE); } // TODO: Proper nonflex error
    Bool const isBytesRef = destType->isBytes;
    if (!unwrapBool(srcType->isFlex)) { exit(EXIT_FAILURE); } // TODO: Proper nonflex error
    if (!eq(toORef(srcType->isBytes), toORef(isBytesRef))) {
        exit(EXIT_FAILURE); // TODO: Proper bytes-vs-slots error
    }

    size_t const destCount = (uintptr_t)fixnumToInt(uncheckedFlexCount(dest));
    size_t const srcCount = (uintptr_t)fixnumToInt(uncheckedFlexCount(src));

    if (offsetS < 0) { exit(EXIT_FAILURE); } // Negative index TODO: Proper bounds error
    size_t const offset = (uintptr_t)offsetS;
    if (offset > destCount) { exit(EXIT_FAILURE); } // TODO: Proper bounds error
    if (startS < 0) { exit(EXIT_FAILURE); } // Negative index TODO: Proper bounds error
    size_t const start = (uintptr_t)startS;
    if (start > srcCount) { exit(EXIT_FAILURE); } // TODO: Proper bounds error
    if (endS < startS) { exit(EXIT_FAILURE); } // TODO: Proper bounds error
    size_t const end = (uintptr_t)endS;

    size_t const copyCount = end - start;
    size_t const copySpace = destCount - offset;
    if (copyCount > copySpace) { exit(EXIT_FAILURE); } // TODO: Proper bounds error

    char* const destVals = uncheckedUntypedFlexPtrMut(dest);
    char const* const srcVals = uncheckedUntypedFlexPtr(src);
    size_t const elemSize = unwrapBool(isBytesRef) ? sizeof(uint8_t) : sizeof(ORef);
    memmove(destVals, srcVals, copyCount * elemSize);

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxAdd(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    state->regs[retReg] = fixnumToORef(tagInt(x + y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxSub(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    state->regs[retReg] = fixnumToORef(tagInt(x - y)); // TODO: Underflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxMul(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    state->regs[retReg] = fixnumToORef(tagInt(x * y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxQuot(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    if (y == 0) { assert(false); } // TODO: Proper error
    state->regs[retReg] = fixnumToORef(tagInt(x / y)); // TODO: Overflow check

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxLt(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    state->regs[retReg] = toORef(tagBool(x < y));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopWrite(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    print(state, stdout, state->regs[firstArgReg]);

    return PRIMOP_RES_CONTINUE; // TODO: Maybe do not return written value?
}
