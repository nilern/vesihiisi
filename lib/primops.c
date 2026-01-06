#include "primops.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdckdint.h>

#include "util/util.h"
#include "bytecode.h"
#include "dispatch.h"

static ORef getErrorHandler(State const* state) {
    ORef const v = varToPtr(state->errorHandler)->val;
    if (eq(v, toORef(state->unbound))) {
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

static PrimopRes primopApplyArray(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const callee = state->regs[firstArgReg];
    // Could also be an `<array!>`, but we "illegally" cast that here to avoid duplicating this
    // function for no actual benefit:
    ArrayRef const argsRef = uncheckedORefToArray(state->regs[firstArgReg + 1]);
    ORef const* args = toPtr(argsRef);
    size_t argc = (uintptr_t)fixnumToInt(uncheckedFlexCount(toORef(argsRef)));

    // Dispatch:
    if (!calleeClosureForArgs(state, callee, args, argc)) {
        return PRIMOP_RES_TAILCALL; // Finish panic setup
    }
    ClosureRef const closure = uncheckedORefToClosure(state->regs[calleeReg]);

    // Check domain (if not already checked by dispatch):
    ORef const maybeCalleeErr = checkDomainForArgs(state, closure, args, argc);
    if (isHeaped(maybeCalleeErr)) {
        state->regs[calleeReg] = getErrorHandler(state);
        state->regs[firstArgReg] = maybeCalleeErr;
        state->entryRegc = firstArgReg + 1;
        return PRIMOP_RES_TAILCALL;
    }

    ORef const method = toPtr(closure)->method;
    assert(isMethod(state, method));
    Method const* const methodPtr = methodToPtr(uncheckedORefToMethod(method));

    // Put args in place:
    if (!isHeaped(methodPtr->code) || !eq(toORef(methodPtr->hasVarArg), toORef(True))){
        memcpy(state->regs + firstArgReg, args, argc * sizeof(ORef));
    } else { // Non-primop with varargs:
        size_t const arity = (uintptr_t)fixnumToInt(uncheckedFlexCount(method));
        size_t const minArity = arity - 1;
        size_t const varargCount = argc - minArity;

        // Fixed args:
        memcpy(state->regs + firstArgReg, args, minArity * sizeof(ORef));

        // Varargs:
        pushStackRoot(state, (ORef*)&argsRef);
        ArrayMutRef const varargsRef = createArrayMut(state, tagInt((intptr_t)varargCount));
        popStackRoots(state, 1);
        args = toPtr(argsRef); // Post-GC reload
        memcpy(arrayMutToPtr(varargsRef), args + minArity, varargCount * sizeof(ORef));

        state->regs[firstArgReg + minArity] = toORef(varargsRef);

        argc = arity;
    }

    state->entryRegc = (uint8_t)(firstArgReg + argc);
    state->checkDomain = false;
    return PRIMOP_RES_TAILAPPLY;
}

static PrimopRes primopApplyList(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const callee = state->regs[firstArgReg];
    ORef args = state->regs[firstArgReg + 1];

    // Dispatch:
    if (!calleeClosureForArglist(state, callee, args)) {
        return PRIMOP_RES_TAILCALL; // Finish panic setup
    }
    ClosureRef const closure = uncheckedORefToClosure(state->regs[calleeReg]);

    ORef const method = toPtr(closure)->method;
    assert(isMethod(state, method));
    Method const* const methodPtr = methodToPtr(uncheckedORefToMethod(method));

    // Put args in place and check them (if not already checked by dispatch):
    size_t const arity = (uintptr_t)fixnumToInt(uncheckedFlexCount(method));
    size_t argc = 0;
    if (state->checkDomain) {
        bool const hasVarArg = unwrapBool(methodPtr->hasVarArg);
        size_t const minArity = !hasVarArg ? arity : arity - 1;

        // Fixed args:
        for (; argc < minArity; ++argc) {
            if (isPair(state, args)) {
                Pair const* const argsPair = toPtr(uncheckedORefToPair(args));

                ORef const arg = argsPair->car;

                // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                assert(isa(state, state->typeType, methodPtr->domain[argc]));
                TypeRef const type = uncheckedORefToType(methodPtr->domain[argc]);
                if (!isa(state, type, arg)) {
                    ORef const err = toORef(createTypeError(state, type, arg));
                    return primopError(state, err);
                }

                state->regs[firstArgReg + argc] = arg;

                args = argsPair->cdr;
            } else if (isEmptyList(state, args)) {
                ORef const err = // Insufficient args
                    toORef(createArityError(state, closure, tagInt((intptr_t)argc)));
                return primopError(state, err);
            } else {
                assert(false); // TODO: Proper improper args error
            }
        }

        if (!hasVarArg){ // Fixed arity => check that no more args remain:
            if (!isEmptyList(state, args)) {
                for (; true; ++argc) {
                    if (isPair(state, args)) {
                        Pair const* const argsPair = toPtr(uncheckedORefToPair(args));
                        args = argsPair->cdr;
                    } else if (isEmptyList(state, args)) {
                        break;
                    } else {
                        assert(false); // TODO: Proper improper args error
                    }
                }

                ORef const err = // Excessive args
                    toORef(createArityError(state, closure, tagInt((intptr_t)argc)));
                return primopError(state, err);
            }
        } else if (!isHeaped(methodPtr->code)) { // Primop varargs:
            assert(isa(state, state->typeType, methodPtr->domain[minArity]));
            TypeRef type = uncheckedORefToType(methodPtr->domain[minArity]);
            for (; true; ++argc) {
                if (isPair(state, args)) {
                    Pair const* const argsPair = toPtr(uncheckedORefToPair(args));

                    ORef const arg = argsPair->car;

                    // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                    if (!isa(state, type, arg)) {
                        ORef const err = toORef(createTypeError(state, type, arg));
                        return primopError(state, err);
                    }

                    state->regs[firstArgReg + argc] = arg;

                    args = argsPair->cdr;
                } else if (isEmptyList(state, args)) {
                    break;
                } else {
                    assert(false); // TODO: Proper improper args error
                }
            }
        } else { // Non-primop varargs:
            pushStackRoot(state, &args);

            assert(isa(state, state->typeType, methodPtr->domain[minArity]));
            TypeRef type = uncheckedORefToType(methodPtr->domain[minArity]);
            pushStackRoot(state, (ORef*)&type);
            size_t bufCap = 10;
            ArrayMutRef varargsBufRef = createArrayMut(state, tagInt((intptr_t)bufCap));
            pushStackRoot(state, (ORef*)&varargsBufRef);
            ORef* varargsBuf = toPtr(varargsBufRef);
            size_t varargCount = 0;
            for (size_t i = 0; true; ++i, ++varargCount) {
                if (isPair(state, args)) {
                    Pair const* argsPair = toPtr(uncheckedORefToPair(args));

                    ORef arg = argsPair->car;

                    // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                    if (!isa(state, type, arg)) {
                        ORef const err = toORef(createTypeError(state, type, arg));
                        popStackRoots(state, 3); // `&type`, `&args` & `&varargsBufRef`
                        return primopError(state, err);
                    }

                    if (i == bufCap) {
                        size_t const newBufCap = bufCap + bufCap * 2;

                        pushStackRoot(state, &arg);
                        ArrayMutRef const newVarargsBufRef =
                            createArrayMut(state, tagInt((intptr_t)newBufCap));
                        popStackRoots(state, 1);
                        argsPair = toPtr(uncheckedORefToPair(args)); // Post-GC reload
                        varargsBuf = toPtr(varargsBufRef); // Post-GC reload
                        ORef* const newVarargsBuf = toPtr(newVarargsBufRef);
                        memcpy(newVarargsBuf, varargsBuf, bufCap * sizeof(ORef));

                        bufCap = newBufCap;
                        varargsBufRef = newVarargsBufRef;
                        varargsBuf = newVarargsBuf;
                    }
                    varargsBuf[i] = arg;

                    args = argsPair->cdr;
                } else if (isEmptyList(state, args)) {
                    break;
                } else {
                    assert(false); // TODO: Proper improper args error
                }
            }

            ArrayMutRef varargsRef;
            if (varargCount != bufCap) {
                varargsRef = createArrayMut(state, tagInt((intptr_t)varargCount));
                varargsBuf = toPtr(varargsBufRef); // Post-GC reload
                memcpy(toPtr(varargsRef), varargsBuf, varargCount * sizeof(ORef));
            } else {
                varargsRef = varargsBufRef;
            }

            popStackRoots(state, 3); // `&type, `&args` & `&varargsBufRef`

            state->regs[firstArgReg + minArity] = toORef(varargsRef);

            argc = minArity + varargCount;
        }
    } else { // `state->checkDomain == false`
        bool const hasVarArg = unwrapBool(methodPtr->hasVarArg);
        size_t const minArity = !hasVarArg ? arity : arity - 1;

        // Fixed args:
        for (size_t i = 0; i < minArity; ++i) {
            // Arity already checked to be correct so `args` *must* be a pair:
            assert(isa(state, state->pairType, args));
            Pair const* const argsPair = toPtr(uncheckedORefToPair(args));

            state->regs[firstArgReg + i] = argsPair->car;

            args = argsPair->cdr;
        }

        if (hasVarArg){ // Vararg:
            if (!isHeaped(methodPtr->code)) { // Primop:
                // Arity already checked to be correct so `args` *must* be a proper list:
                for (size_t i = minArity; isPair(state, args); ++i) {
                    Pair const* const argsPair = toPtr(uncheckedORefToPair(args));

                    state->regs[firstArgReg + i] = argsPair->car;

                    args = argsPair->cdr;
                }
            } else { // Non-primop:
                pushStackRoot(state, &args);

                size_t bufCap = 10;
                ArrayMutRef varargsBufRef = createArrayMut(state, tagInt((intptr_t)bufCap));
                pushStackRoot(state, (ORef*)&varargsBufRef);
                ORef* varargsBuf = toPtr(varargsBufRef);
                size_t varargCount = 0;
                for (size_t i = 0; true; ++i, ++varargCount) {
                    if (isPair(state, args)) {
                        Pair const* argsPair = toPtr(uncheckedORefToPair(args));

                        if (i == bufCap) {
                            size_t const newBufCap = bufCap + bufCap * 2;

                            ArrayMutRef const newVarargsBufRef =
                                createArrayMut(state, tagInt((intptr_t)newBufCap));
                            argsPair = toPtr(uncheckedORefToPair(args)); // Post-GC reload
                            varargsBuf = toPtr(varargsBufRef); // Post-GC reload
                            ORef* const newVarargsBuf = toPtr(newVarargsBufRef);
                            memcpy(newVarargsBuf, varargsBuf, bufCap * sizeof(ORef));

                            bufCap = newBufCap;
                            varargsBufRef = newVarargsBufRef;
                            varargsBuf = newVarargsBuf;
                        }
                        varargsBuf[i] = argsPair->car;

                        args = argsPair->cdr;
                    } else if (isEmptyList(state, args)) {
                        break;
                    } else {
                        assert(false); // TODO: Proper improper args error
                    }
                }

                ArrayMutRef varargsRef;
                if (varargCount != bufCap) {
                    varargsRef = createArrayMut(state, tagInt((intptr_t)varargCount));
                    varargsBuf = toPtr(varargsBufRef); // Post-GC reload
                    memcpy(toPtr(varargsRef), varargsBuf, varargCount * sizeof(ORef));
                } else {
                    varargsRef = varargsBufRef;
                }

                popStackRoots(state, 2); // `&args` & `&varargsBufRef`

                state->regs[firstArgReg + minArity] = toORef(varargsRef);

                argc = minArity + varargCount;
            }
        }
    }

    state->entryRegc = (uint8_t)(firstArgReg + argc);
    state->checkDomain = false;
    return PRIMOP_RES_TAILAPPLY;
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

    state->regs[retReg] = toORef(tagBool(eq(x, y)));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopMake(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    TypeRef typeRef = uncheckedORefToType(state->regs[firstArgReg]);
    uint8_t const callArity = state->entryRegc - firstArgReg;

    Type const* type = toPtr(typeRef);
    if (!unwrapBool(type->isFlex)) {
        // Alloc:
        void* ptr = tryAlloc(&state->heap.tospace, type);
        if (mustCollect(ptr)) {
            collect(state);
            typeRef = uncheckedORefToType(state->regs[firstArgReg]);
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

    TypeRef typeRef = uncheckedORefToType(state->regs[firstArgReg]);
    Fixnum const count = uncheckedORefToFixnum(state->regs[firstArgReg + 1]);

    Type const* type = toPtr(typeRef);
    if (unwrapBool(type->isFlex)) {
        void* ptr = tryAllocFlex(&state->heap.tospace, type, count);
        if (mustCollect(ptr)) {
            collect(state);
            typeRef = uncheckedORefToType(state->regs[firstArgReg]);
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

    int64_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    int64_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    int64_t const res = x + y;
    if (((res ^ x) & (res ^ y)) >> (payloadWidth - 1)) {
        // Overflow has occurred when `x` and `y` have the same sign and the sign of the
        // result is the opposite.
        // `ckd_add` is not useful because the carry *does* fit in `int64_t`.

        ClosureRef const f = uncheckedORefToClosure(state->regs[calleeReg]);
        Fixnum const xRef = uncheckedORefToFixnum(state->regs[firstArgReg]);
        Fixnum const yRef = uncheckedORefToFixnum(state->regs[firstArgReg + 1]);
        return primopError(state, toORef(createOverflowError(state, f, xRef, yRef)));
    }

    state->regs[retReg] = toORef(tagInt(res));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxSub(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    int64_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    int64_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    int64_t const res = x - y;
    if (((x ^ y) & (res ^ x)) >> (payloadWidth - 1)) {
        // Overflow has occurred when `x` and `y` have different signs and the sign of the result
        // is different from the sign of `x` (or equivalently, the same as the sign of `y`).
        // `ckd_sub` is not useful because the carry *does* fit in `int64_t`.

        ClosureRef const f = uncheckedORefToClosure(state->regs[calleeReg]);
        Fixnum const xRef = uncheckedORefToFixnum(state->regs[firstArgReg]);
        Fixnum const yRef = uncheckedORefToFixnum(state->regs[firstArgReg + 1]);
        return primopError(state, toORef(createOverflowError(state, f, xRef, yRef)));
    }

    state->regs[retReg] = toORef(tagInt(res));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxMul(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    int64_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    int64_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    int64_t res;
    if (ckd_mul(&res, x, y)
        || (res >> payloadWidth) != ((res & (int64_t)payloadMask) >> (payloadWidth - 1))
    ) {
        // Overflow has occurred if we overflowed `int64_t` or the extra bits of `res` are not all
        // equal to the sign bit of the payload.

        ClosureRef const f = uncheckedORefToClosure(state->regs[calleeReg]);
        Fixnum const xRef = uncheckedORefToFixnum(state->regs[firstArgReg]);
        Fixnum const yRef = uncheckedORefToFixnum(state->regs[firstArgReg + 1]);
        return primopError(state, toORef(createOverflowError(state, f, xRef, yRef)));
    }

    state->regs[retReg] = toORef(tagInt(res));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFxQuot(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = uncheckedFixnumToInt(state->regs[firstArgReg]);
    intptr_t const y = uncheckedFixnumToInt(state->regs[firstArgReg + 1]);

    if (y == 0) {
        ClosureRef const f = uncheckedORefToClosure(state->regs[calleeReg]);
        Fixnum const xRef = uncheckedORefToFixnum(state->regs[firstArgReg]);
        Fixnum const yRef = uncheckedORefToFixnum(state->regs[firstArgReg + 1]);
        return primopError(state, toORef(createDivByZeroError(state, f, xRef, yRef)));
    }

    if (x == fixnumMin && y == -1) {
        // Due to two's complement `-fixnumMin == fixnumMax + 1` but this is the only overflowing
        // combination.

        ClosureRef const f = uncheckedORefToClosure(state->regs[calleeReg]);
        Fixnum const xRef = uncheckedORefToFixnum(state->regs[firstArgReg]);
        Fixnum const yRef = uncheckedORefToFixnum(state->regs[firstArgReg + 1]);
        return primopError(state, toORef(createOverflowError(state, f, xRef, yRef)));
    }

    state->regs[retReg] = toORef(tagInt(x / y));

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

static PrimopRes primopFixnumToFlonum(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const n = uncheckedFixnumToInt(state->regs[firstArgReg]);

    state->regs[retReg] = flonumToORef(tagFlonum((double)n));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFlAdd(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    double const x = uncheckedFlonumToDouble(state->regs[firstArgReg]);
    double const y = uncheckedFlonumToDouble(state->regs[firstArgReg + 1]);

    state->regs[retReg] = flonumToORef(tagFlonum(x + y));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFlSub(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    double const x = uncheckedFlonumToDouble(state->regs[firstArgReg]);
    double const y = uncheckedFlonumToDouble(state->regs[firstArgReg + 1]);

    state->regs[retReg] = flonumToORef(tagFlonum(x - y));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFlMul(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    double const x = uncheckedFlonumToDouble(state->regs[firstArgReg]);
    double const y = uncheckedFlonumToDouble(state->regs[firstArgReg + 1]);

    state->regs[retReg] = flonumToORef(tagFlonum(x * y));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopFlDiv(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    double const x = uncheckedFlonumToDouble(state->regs[firstArgReg]);
    double const y = uncheckedFlonumToDouble(state->regs[firstArgReg + 1]);

    state->regs[retReg] = flonumToORef(tagFlonum(x / y));

    return PRIMOP_RES_CONTINUE;
}

static PrimopRes primopWrite(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    print(state, stdout, state->regs[firstArgReg]);

    return PRIMOP_RES_CONTINUE; // TODO: Maybe do not return written value?
}
