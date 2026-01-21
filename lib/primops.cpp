#include "primops.hpp"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "../deps/utf8proc/utf8proc.h"

#include "util/util.hpp"
#include "bytecode.hpp"
#include "dispatch.hpp"
#include "namespace.hpp"
#include "compiler/compiler.hpp"

namespace {

ORef getErrorHandler(State const* state) {
    ORef const v = state->errorHandler.ptr()->val;
    if (eq(v, state->singletons.unbound.oref())) {
        exit(EXIT_FAILURE); // FIXME
    }

    return v;
}

[[nodiscard]]
PrimopRes primopError(State* state, ORef err) {
    state->regs[calleeReg] = getErrorHandler(state);
    state->regs[firstArgReg] = err;
    state->entryRegc = firstArgReg + 1;
    return PrimopRes::TAILCALL;
}

PrimopRes callBytecode(State* /*state*/) { return PrimopRes::TAILCALL; }

PrimopRes primopAbort(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const error = state->regs[firstArgReg];

    fputs("Runtime error: ", stderr);
    print(state, stderr, error);

    assert(isa(state, state->types.continuation, state->regs[retContReg]));
    Continuation const* const cont =
        HRef<Continuation>::fromUnchecked(state->regs[retContReg]).ptr();
    ORef const anyCaller = cont->method;
    if (isMethod(state, anyCaller)) {
        auto const caller = HRef<Method>::fromUnchecked(anyCaller);

        size_t const callerPc = uint64_t(cont->pc.val()); // FIXME: This is return PC, not *call* PC
        auto const maybeLoc = locatePc(caller, callerPc);

        ORef const maybeCallerName = caller.ptr()->maybeName;
        if (isSymbol(state, maybeCallerName)) {
            fputs(" in ", stderr);
            print(state, stderr, HRef<Symbol>::fromUnchecked(maybeCallerName));
        }

        fputs(" at ", stderr);

        if (maybeLoc.hasVal) { maybeLoc.val.print(*state, stderr); }
    } else {
        // FIXME: Exit continuation should have a method that inherits toplevel thunk location to
        // make this work.
        assert(false);
    }

    putc('\n', stderr);

    return PrimopRes::ABORT;
}

PrimopRes primopApplyArray(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const callee = state->regs[firstArgReg];
    // Could also be an `<array!>`, but we "illegally" cast that here to avoid duplicating this
    // function for no actual benefit:
    HRef<Array> const argsRef = HRef<Array>::fromUnchecked(state->regs[firstArgReg + 1]);
    ORef const* args = argsRef.ptr()->flexData();
    size_t argc = (uint64_t)argsRef.ptr()->flexCount().val();

    // Dispatch:
    if (!calleeClosureForArgs(state, callee, args, argc)) {
        return PrimopRes::TAILCALL; // Finish panic setup
    }
    HRef<Closure> const closure = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);

    // Check domain (if not already checked by dispatch):
    ORef const maybeCalleeErr = checkDomainForArgs(state, closure, args, argc);
    if (isHeaped(maybeCalleeErr)) {
        state->regs[calleeReg] = getErrorHandler(state);
        state->regs[firstArgReg] = maybeCalleeErr;
        state->entryRegc = firstArgReg + 1;
        return PrimopRes::TAILCALL;
    }

    ORef const method = closure.ptr()->method;
    assert(isMethod(state, method));
    Method const* const methodPtr = HRef<Method>::fromUnchecked(method).ptr();

    // Put args in place:
    if (!isHeaped(methodPtr->code) || !methodPtr->hasVarArg.val()){
        memcpy(state->regs + firstArgReg, args, argc * sizeof(ORef));
    } else { // Non-primop with varargs:
        size_t const arity = (uint64_t)methodPtr->flexCount().val();
        size_t const minArity = arity - 1;
        size_t const varargCount = argc - minArity;

        // Fixed args:
        memcpy(state->regs + firstArgReg, args, minArity * sizeof(ORef));

        // Varargs:
        pushStackRoot(state, (ORef*)&argsRef);
        HRef<ArrayMut> const varargsRef = createArrayMut(state, Fixnum((intptr_t)varargCount));
        popStackRoots(state, 1);
        args = argsRef.ptr()->flexData(); // Post-GC reload
        memcpy((void*)varargsRef.ptr()->flexData(), args + minArity, varargCount * sizeof(ORef));

        state->regs[firstArgReg + minArity] = varargsRef.oref();

        argc = arity;
    }

    state->entryRegc = (uint8_t)(firstArgReg + argc);
    state->checkDomain = false;
    return PrimopRes::TAILAPPLY;
}

PrimopRes primopApplyList(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const callee = state->regs[firstArgReg];
    ORef args = state->regs[firstArgReg + 1];

    // Dispatch:
    if (!calleeClosureForArglist(state, callee, args)) {
        return PrimopRes::TAILCALL; // Finish panic setup
    }
    HRef<Closure> const closure = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);

    ORef const method = closure.ptr()->method;
    assert(isMethod(state, method));
    Method const* const methodPtr = HRef<Method>::fromUnchecked(method).ptr();

    // Put args in place and check them (if not already checked by dispatch):
    size_t const arity = (uint64_t)methodPtr->flexCount().val();
    size_t argc = 0;
    if (state->checkDomain) {
        bool const hasVarArg = methodPtr->hasVarArg.val();
        size_t const minArity = !hasVarArg ? arity : arity - 1;

        // Fixed args:
        for (; argc < minArity; ++argc) {
            if (isPair(state, args)) {
                Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

                ORef const arg = argsPair->car;

                // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                assert(isa(state, state->types.type, methodPtr->domain()[argc]));
                HRef<Type> const type = HRef<Type>::fromUnchecked(methodPtr->domain()[argc]);
                if (!isa(state, type, arg)) {
                    ORef const err = createTypeError(state, type, arg).oref();
                    return primopError(state, err);
                }

                state->regs[firstArgReg + argc] = arg;

                args = argsPair->cdr;
            } else if (isEmptyList(state, args)) {
                ORef const err = // Insufficient args
                    createArityError(state, closure, Fixnum((intptr_t)argc)).oref();
                return primopError(state, err);
            } else {
                assert(false); // TODO: Proper improper args error
            }
        }

        if (!hasVarArg){ // Fixed arity => check that no more args remain:
            if (!isEmptyList(state, args)) {
                for (; true; ++argc) {
                    if (isPair(state, args)) {
                        Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();
                        args = argsPair->cdr;
                    } else if (isEmptyList(state, args)) {
                        break;
                    } else {
                        assert(false); // TODO: Proper improper args error
                    }
                }

                ORef const err = // Excessive args
                    createArityError(state, closure, Fixnum((intptr_t)argc)).oref();
                return primopError(state, err);
            }
        } else if (!isHeaped(methodPtr->code)) { // Primop varargs:
            assert(isa(state, state->types.type, methodPtr->domain()[minArity]));
            HRef<Type> type = HRef<Type>::fromUnchecked(methodPtr->domain()[minArity]);
            for (; true; ++argc) {
                if (isPair(state, args)) {
                    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

                    ORef const arg = argsPair->car;

                    // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                    if (!isa(state, type, arg)) {
                        ORef const err = createTypeError(state, type, arg).oref();
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

            assert(isa(state, state->types.type, methodPtr->domain()[minArity]));
            HRef<Type> type = HRef<Type>::fromUnchecked(methodPtr->domain()[minArity]);
            pushStackRoot(state, (ORef*)&type);
            size_t bufCap = 10;
            HRef<ArrayMut> varargsBufRef = createArrayMut(state, Fixnum((intptr_t)bufCap));
            pushStackRoot(state, (ORef*)&varargsBufRef);
            ORef* varargsBuf = varargsBufRef.ptr()->flexDataMut();
            size_t varargCount = 0;
            for (size_t i = 0; true; ++i, ++varargCount) {
                if (isPair(state, args)) {
                    Pair const* argsPair = HRef<Pair>::fromUnchecked(args).ptr();

                    ORef arg = argsPair->car;

                    // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                    if (!isa(state, type, arg)) {
                        ORef const err = createTypeError(state, type, arg).oref();
                        popStackRoots(state, 3); // `&type`, `&args` & `&varargsBufRef`
                        return primopError(state, err);
                    }

                    if (i == bufCap) {
                        size_t const newBufCap = bufCap + bufCap * 2;

                        pushStackRoot(state, &arg);
                        HRef<ArrayMut> const newVarargsBufRef =
                            createArrayMut(state, Fixnum((intptr_t)newBufCap));
                        popStackRoots(state, 1);
                        argsPair = HRef<Pair>::fromUnchecked(args).ptr(); // Post-GC reload
                        varargsBuf = varargsBufRef.ptr()->flexDataMut(); // Post-GC reload
                        ORef* const newVarargsBuf = newVarargsBufRef.ptr()->flexDataMut();
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

            HRef<ArrayMut> const varargsRef = [&](){ // IIFE
                if (varargCount != bufCap) {
                    HRef<ArrayMut> const varargsRef =
                        createArrayMut(state, Fixnum((intptr_t)varargCount));
                    varargsBuf = varargsBufRef.ptr()->flexDataMut(); // Post-GC reload
                    memcpy((void*)varargsRef.ptr()->flexData(), varargsBuf,
                           varargCount * sizeof(ORef));
                    return varargsRef;
                } else {
                    return varargsBufRef;
                }
            }();

            popStackRoots(state, 3); // `&type, `&args` & `&varargsBufRef`

            state->regs[firstArgReg + minArity] = varargsRef.oref();

            argc = minArity + varargCount;
        }
    } else { // `state->checkDomain == false`
        bool const hasVarArg = methodPtr->hasVarArg.val();
        size_t const minArity = !hasVarArg ? arity : arity - 1;

        // Fixed args:
        for (size_t i = 0; i < minArity; ++i) {
            // Arity already checked to be correct so `args` *must* be a pair:
            assert(isa(state, state->types.pair, args));
            Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

            state->regs[firstArgReg + i] = argsPair->car;

            args = argsPair->cdr;
        }

        if (hasVarArg){ // Vararg:
            if (!isHeaped(methodPtr->code)) { // Primop:
                // Arity already checked to be correct so `args` *must* be a proper list:
                for (size_t i = minArity; isPair(state, args); ++i) {
                    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

                    state->regs[firstArgReg + i] = argsPair->car;

                    args = argsPair->cdr;
                }
            } else { // Non-primop:
                pushStackRoot(state, &args);

                size_t bufCap = 10;
                HRef<ArrayMut> varargsBufRef = createArrayMut(state, Fixnum((intptr_t)bufCap));
                pushStackRoot(state, (ORef*)&varargsBufRef);
                ORef* varargsBuf = varargsBufRef.ptr()->flexDataMut();
                size_t varargCount = 0;
                for (size_t i = 0; true; ++i, ++varargCount) {
                    if (isPair(state, args)) {
                        Pair const* argsPair = HRef<Pair>::fromUnchecked(args).ptr();

                        if (i == bufCap) {
                            size_t const newBufCap = bufCap + bufCap * 2;

                            HRef<ArrayMut> const newVarargsBufRef =
                                createArrayMut(state, Fixnum((intptr_t)newBufCap));
                            argsPair = HRef<Pair>::fromUnchecked(args).ptr(); // Post-GC reload
                            varargsBuf = varargsBufRef.ptr()->flexDataMut(); // Post-GC reload
                            ORef* const newVarargsBuf = newVarargsBufRef.ptr()->flexDataMut();
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

                HRef<ArrayMut> const varargsRef = [&](){ // IIFE
                    if (varargCount != bufCap) {
                        HRef<ArrayMut> const varargsRef =
                            createArrayMut(state, Fixnum((intptr_t)varargCount));
                        varargsBuf = varargsBufRef.ptr()->flexDataMut(); // Post-GC reload
                        memcpy(varargsRef.ptr()->flexDataMut(), varargsBuf,
                               varargCount * sizeof(ORef));
                        return varargsRef;
                    } else {
                        return varargsBufRef;
                    }
                }();

                popStackRoots(state, 2); // `&args` & `&varargsBufRef`

                state->regs[firstArgReg + minArity] = varargsRef.oref();

                argc = minArity + varargCount;
            }
        }
    }

    state->entryRegc = (uint8_t)(firstArgReg + argc);
    state->checkDomain = false;
    return PrimopRes::TAILAPPLY;
}

PrimopRes primopCallCC(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[calleeReg] = state->regs[firstArgReg];
    state->regs[firstArgReg] = state->regs[retContReg];
    state->entryRegc = firstArgReg + 1;
    return PrimopRes::TAILCALL;
}

PrimopRes primopContinue(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[retContReg] = state->regs[firstArgReg];
    state->regs[retReg] = state->regs[firstArgReg + 1];
    return PrimopRes::CONTINUE;
}

PrimopRes primopIdentical(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const x = state->regs[firstArgReg];
    ORef const y = state->regs[firstArgReg + 1];

    state->regs[retReg] = Bool(eq(x, y)).oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopTypeOf(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[retReg] = typeOf(state, state->regs[firstArgReg]);

    return PrimopRes::CONTINUE;
}

PrimopRes primopMakeSlotsType(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const name = HRef<Symbol>::fromUnchecked(state->regs[firstArgReg]);
    auto const slotCount = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
    auto const isFlex = Bool::fromUnchecked(state->regs[firstArgReg + 2]);

    state->regs[retReg] = createSlotsType(state, name, slotCount, isFlex);

    return PrimopRes::CONTINUE;
}

PrimopRes primopMake(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    HRef<Type> typeRef = HRef<Type>::fromUnchecked(state->regs[firstArgReg]);
    uint8_t const callArity = state->entryRegc - firstArgReg;

    Type const* type = typeRef.ptr();
    if (!type->isFlex.val()) {
        // Alloc:
        Object* ptr = state->heap.tospace.tryAlloc(type);
        if (mustCollect(ptr)) {
            collect(state);
            typeRef = HRef<Type>::fromUnchecked(state->regs[firstArgReg]);
            type = typeRef.ptr(); // Post-GC reload
            ptr = state->heap.tospace.allocOrDie(type);
        }

        // Init:
        if (!type->isBytes.val()) {
            size_t const fieldCount = (uint64_t)type->minSize.val() / sizeof(ORef);
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

        state->regs[retReg] = HRef<Object>(ptr).oref();

        return PrimopRes::CONTINUE;
    } else {
        exit(EXIT_FAILURE); // TODO
    }
}

PrimopRes primopSlotGet(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];
    size_t const slotIdx = (uint64_t)Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    Type const* type = typeOf(state, v).ptr();
    if (!type->isBytes.val()) {
        size_t const slotCount = (uintptr_t)type->minSize.val() / sizeof(ORef);
        if (slotIdx >= slotCount) {
            assert(false); // TODO: Proper bounds error
        }

        ORef const* const slots = (ORef const*)uncheckedORefToPtr(v);
        state->regs[retReg] = slots[slotIdx];
    } else {
        assert(false); // TODO
    }

    return PrimopRes::CONTINUE;
}

PrimopRes primopSlotSet(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];
    size_t const slotIdx = (uint64_t)Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();
    ORef const slotV = state->regs[firstArgReg + 2];

    Type const* type = typeOf(state, v).ptr();
    if (!type->isBytes.val()) {
        size_t const slotCount = (uintptr_t)type->minSize.val() / sizeof(ORef);
        if (slotIdx >= slotCount) {
            assert(false); // TODO: Proper bounds error
        }

        ORef* const slots = (ORef*)uncheckedORefToPtr(v);
        slots[slotIdx] = slotV;
    } else {
        assert(false); // TODO
    }

    state->regs[retReg] = slotV; // Beats returning `v`; at least consistent with e.g. `def` atm

    return PrimopRes::CONTINUE;
}

PrimopRes primopMakeFlex(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    HRef<Type> typeRef = HRef<Type>::fromUnchecked(state->regs[firstArgReg]);
    Fixnum const count = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);

    Type const* type = typeRef.ptr();
    if (type->isFlex.val()) {
        Object* ptr = state->heap.tospace.tryAllocFlex(type, count);
        if (mustCollect(ptr)) {
            collect(state);
            typeRef = HRef<Type>::fromUnchecked(state->regs[firstArgReg]);
            type = typeRef.ptr();
            ptr = state->heap.tospace.allocFlexOrDie(type, count);
        }

        state->regs[retReg] = HRef<Object>(ptr).oref();

        return PrimopRes::CONTINUE;
    } else {
        exit(EXIT_FAILURE); // TODO
    }
}

PrimopRes primopFlexCount(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];

    Type const* type = typeOf(state, v).ptr();
    if (!type->isFlex.val()) {
        assert(false); // TODO: Proper nonflex error
    }

    state->regs[retReg] = ((FlexHeader const*)uncheckedORefToPtr(v) - 1)->count.oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFlexGet(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];
    int64_t const i = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    Type const* type = typeOf(state, v).ptr();
    if (!type->isFlex.val()) {
        assert(false); // TODO: Proper nonflex error
    }
    if (type->isBytes.val()) {
        assert(false); // TODO: Proper nonslots error
    }

    void const* const ptr = uncheckedORefToPtr(v);
    int64_t const count = ((FlexHeader const*)ptr - 1)->count.val();
    if (i < 0 || i >= count) {
        assert(false); // TODO: Proper bounds error
    }

    ORef const* const flexSlots = (ORef const*)((char const*)ptr + type->minSize.val());
    state->regs[retReg] = flexSlots[i];

    return PrimopRes::CONTINUE;
}

PrimopRes primopFlexSet(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];
    int64_t const i = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();
    ORef const iv = state->regs[firstArgReg + 2];

    Type const* type = typeOf(state, v).ptr();
    if (!type->isFlex.val()) {
        assert(false); // TODO: Proper nonflex error
    }
    if (type->isBytes.val()) {
        assert(false); // TODO: Proper nonslots error
    }

    void const* const ptr = uncheckedORefToPtr(v);
    int64_t const count = ((FlexHeader const*)ptr - 1)->count.val();
    if (i < 0 || i >= count) {
        assert(false); // TODO: Proper bounds error
    }

    ORef* const flexSlots = (ORef*)((char const*)ptr + type->minSize.val());
    flexSlots[i] = iv;
    state->regs[retReg] = iv; // Once again most convenient and consistent to just return this

    return PrimopRes::CONTINUE;
}

PrimopRes primopFlexCopy(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const dest = state->regs[firstArgReg];
    intptr_t const offsetS = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();
    ORef const src = state->regs[firstArgReg + 2];
    intptr_t const startS = Fixnum::fromUnchecked(state->regs[firstArgReg + 3]).val();
    intptr_t const endS = Fixnum::fromUnchecked(state->regs[firstArgReg + 4]).val();
    Type const* const destType = typeOf(state, dest).ptr();
    Type const* const srcType = typeOf(state, src).ptr();

    if (!destType->isFlex.val()) { exit(EXIT_FAILURE); } // TODO: Proper nonflex error
    Bool const isBytesRef = destType->isBytes;
    if (!srcType->isFlex.val()) { exit(EXIT_FAILURE); } // TODO: Proper nonflex error
    if (!eq(srcType->isBytes.oref(), isBytesRef.oref())) {
        exit(EXIT_FAILURE); // TODO: Proper bytes-vs-slots error
    }

    size_t const destCount = (uintptr_t)uncheckedFlexHeader(dest)->count.val();
    size_t const srcCount = (uintptr_t)uncheckedFlexHeader(src)->count.val();

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

    char* const destVals = (char*)uncheckedUntypedFlexPtrMut(dest);
    char const* const srcVals = (char const*)uncheckedUntypedFlexPtr(src);
    size_t const elemSize = isBytesRef.val() ? sizeof(uint8_t) : sizeof(ORef);
    memmove(destVals, srcVals, copyCount * elemSize);

    return PrimopRes::CONTINUE;
}

PrimopRes primopFlexClone(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef src = state->regs[firstArgReg];
    intptr_t const startS = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();
    intptr_t const endS = Fixnum::fromUnchecked(state->regs[firstArgReg + 2]).val();
    HRef<Type> typeRef = typeOf(state, src);
    Type const* type = typeRef.ptr();

    if (!type->isFlex.val()) { exit(EXIT_FAILURE); } // TODO: Proper nonflex error

    size_t const srcCount = (uintptr_t)uncheckedFlexHeader(src)->count.val();

    if (startS < 0) { exit(EXIT_FAILURE); } // Negative index TODO: Proper bounds error
    size_t const start = (uintptr_t)startS;
    if (endS < 0) { exit(EXIT_FAILURE); } // Negative index TODO: Proper bounds error
    size_t const end = (uintptr_t)endS;
    if (end > srcCount) { exit(EXIT_FAILURE); } // TODO: Proper bounds error
    if (start > end) { exit(EXIT_FAILURE); } // TODO: Proper bounds error

    size_t const copyCount = end - start;

    Object* dest = state->heap.tospace.tryAllocFlex(type, Fixnum{int64_t(copyCount)});
    if (mustCollect(dest)) {
        pushStackRoot(state, &src);
        pushStackRoot(state, &typeRef);
        collect(state);
        popStackRoots(state, 2);
        type = typeRef.ptr();
        dest = state->heap.tospace.allocFlexOrDie(type, Fixnum{int64_t(copyCount)});
    }

    auto const minSize = size_t(type->minSize.val());
    size_t const elemSize = type->isBytes.val() ? sizeof(uint8_t) : sizeof(ORef);
    memcpy(dest, (char*)uncheckedUntypedFlexPtr(src) + start * elemSize,
           minSize + copyCount * elemSize);

    state->regs[retReg] = HRef{dest};

    return PrimopRes::CONTINUE;
}

PrimopRes primopFxAdd(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    int64_t const x = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();
    int64_t const y = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    int64_t const res = x + y;
    if (((res ^ x) & (res ^ y)) >> (payloadWidth - 1)) {
        // Overflow has occurred when `x` and `y` have the same sign and the sign of the
        // result is the opposite.
        // `ckd_add` is not useful because the carry *does* fit in `int64_t`.

        HRef<Closure> const f = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
        Fixnum const xRef = Fixnum::fromUnchecked(state->regs[firstArgReg]);
        Fixnum const yRef = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
        return primopError(state, createOverflowError(state, f, xRef, yRef).oref());
    }

    state->regs[retReg] = Fixnum{res}.oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFxSub(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    int64_t const x = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();
    int64_t const y = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    int64_t const res = x - y;
    if (((x ^ y) & (res ^ x)) >> (payloadWidth - 1)) {
        // Overflow has occurred when `x` and `y` have different signs and the sign of the result
        // is different from the sign of `x` (or equivalently, the same as the sign of `y`).
        // `ckd_sub` is not useful because the carry *does* fit in `int64_t`.

        HRef<Closure> const f = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
        Fixnum const xRef = Fixnum::fromUnchecked(state->regs[firstArgReg]);
        Fixnum const yRef = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
        return primopError(state, createOverflowError(state, f, xRef, yRef).oref());
    }

    state->regs[retReg] = Fixnum{res}.oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFxMul(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    int64_t const x = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();
    int64_t const y = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

// TODO: Improve portability e.g. MSVC intsafe.h `LongLongMult`:
// This is unfortunate, but C++26 `ckd_mul` seems to have even fewer supported compilers:
#if defined __has_builtin && __has_builtin(__builtin_smull_overflow)
    int64_t res;
    if (__builtin_smull_overflow(x, y, &res)
        || (res >> payloadWidth) != ((res & (int64_t)payloadMask) >> (payloadWidth - 1))
    ) {
        // Overflow has occurred if we overflowed `int64_t` or the extra bits of `res` are not all
        // equal to the sign bit of the payload.

        HRef<Closure> const f = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
        Fixnum const xRef = Fixnum::fromUnchecked(state->regs[firstArgReg]);
        Fixnum const yRef = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
        return primopError(state, createOverflowError(state, f, xRef, yRef).oref());
    }
#endif
// else should not compile due to missing `res`

    state->regs[retReg] = Fixnum{res}.oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFxQuot(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();
    intptr_t const y = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    if (y == 0) {
        HRef<Closure> const f = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
        Fixnum const xRef = Fixnum::fromUnchecked(state->regs[firstArgReg]);
        Fixnum const yRef = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
        return primopError(state, createDivByZeroError(state, f, xRef, yRef).oref());
    }

    if (x == Fixnum::min && y == -1) {
        // Due to two's complement `-fixnumMin == fixnumMax + 1` but this is the only overflowing
        // combination.

        HRef<Closure> const f = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
        Fixnum const xRef = Fixnum::fromUnchecked(state->regs[firstArgReg]);
        Fixnum const yRef = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
        return primopError(state, createOverflowError(state, f, xRef, yRef).oref());
    }

    state->regs[retReg] = Fixnum{x / y}.oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFxLt(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const x = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();
    intptr_t const y = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Bool(x < y).oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFixnumToFlonum(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    intptr_t const n = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();

    state->regs[retReg] = Flonum((double)n).oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFlAdd(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    double const x = Flonum::fromUnchecked(state->regs[firstArgReg]).val();
    double const y = Flonum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Flonum(x + y).oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFlSub(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    double const x = Flonum::fromUnchecked(state->regs[firstArgReg]).val();
    double const y = Flonum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Flonum(x - y).oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFlMul(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    double const x = Flonum::fromUnchecked(state->regs[firstArgReg]).val();
    double const y = Flonum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Flonum(x * y).oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopFlDiv(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    double const x = Flonum::fromUnchecked(state->regs[firstArgReg]).val();
    double const y = Flonum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Flonum(x / y).oref();

    return PrimopRes::CONTINUE;
}

PrimopRes primopCharToInteger(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    uint32_t const c = Char::fromUnchecked(state->regs[firstArgReg]).val();

    state->regs[retReg] = Fixnum{int64_t(c)};

    return PrimopRes::CONTINUE;
}

PrimopRes primopCharLt(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    uint32_t const c1 = Char::fromUnchecked(state->regs[firstArgReg]).val();
    uint32_t const c2 = Char::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Bool{c1 < c2};

    return PrimopRes::CONTINUE;
}

PrimopRes primopCharIsAlphabetic(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const c = int32_t(Char::fromUnchecked(state->regs[firstArgReg]).val());

    utf8proc_category_t const cat = utf8proc_category(c);
    bool const isAlphabetic = (UTF8PROC_CATEGORY_LU <= cat && cat <= UTF8PROC_CATEGORY_LO)
                              || cat == UTF8PROC_CATEGORY_NL;
    state->regs[retReg] = Bool{isAlphabetic};

    return PrimopRes::CONTINUE;
}

PrimopRes primopCharIsNumeric(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const c = Char::fromUnchecked(state->regs[firstArgReg]).val();

    state->regs[retReg] = Bool{utf8proc_category(int32_t(c)) == UTF8PROC_CATEGORY_ND};
    return PrimopRes::CONTINUE;
}

PrimopRes primopCharIsWhitespace(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const c = Char::fromUnchecked(state->regs[firstArgReg]).val();

    utf8proc_category_t const cat = utf8proc_category(int32_t(c));
    bool const isWhitespace =
        (UTF8PROC_CATEGORY_ZS <= cat && cat <= UTF8PROC_CATEGORY_ZP) // Space cats
        || (9 <= c && c <= 13) || c == 133; // Whitespace ctrls
    state->regs[retReg] = Bool{isWhitespace};

    return PrimopRes::CONTINUE;
}

PrimopRes primopArrayMutToString(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto vs = HRef<ArrayMut>::fromUnchecked(state->regs[firstArgReg]);
    pushStackRoot(state, &vs);

    auto const cpCount = size_t(vs.ptr()->flexCount().val());
    auto tmpRef = createByteArrayMut(state, Fixnum{int64_t(cpCount * 4)});
    pushStackRoot(state, &tmpRef);
    auto tmp = tmpRef.ptr();
    auto const cps = vs.ptr()->flexData();
    auto const tmpData = tmp->flexDataMut();
    ssize_t stringSize = 0;
    for (size_t i = 0; i < cpCount; ++i) {
        ORef const v = cps[i];
        if (!isChar(v)) {
            return primopError(state, createTypeError(state, state->types.charr, v));
        }
        auto const cp = Char::fromUnchecked(v).val();

        stringSize += utf8proc_encode_char(int32_t(cp), tmpData + stringSize);
    }

    String* const res = allocString(state, Fixnum{stringSize});
    tmp = tmpRef.ptr(); // Post-GC reload
    memcpy(const_cast<uint8_t*>(res->flexData()), tmp->flexData(), size_t(stringSize));

    state->regs[retReg] = HRef{res};

    popStackRoots(state, 2);
    return PrimopRes::CONTINUE;
}

PrimopRes primopStringIteratorPeek(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    StringIterator* const iter =
        HRef<StringIterator>::fromUnchecked(state->regs[firstArgReg]).ptr();

    ORef const maybeString = iter->string;
    if (!isString(state, maybeString)) {
        return primopError(state, createTypeError(state, state->types.string, maybeString));
    }
    auto const string = HRef<String>::fromUnchecked(maybeString);
    ORef const maybeByteIdx = iter->byteIdx;
    if (!isFixnum(maybeByteIdx)) {
        return primopError(state, createTypeError(state, state->types.fixnum, maybeString));
    }
    ssize_t const byteIdx = Fixnum::fromUnchecked(maybeByteIdx).val();
    ssize_t const cap = string.ptr()->flexCount().val();

    if (byteIdx >= cap) {
        state->regs[retReg] = state->singletons.end;
        return PrimopRes::CONTINUE;
    }

    int32_t maybeCp;
    [[maybe_unused]] ssize_t cpWidth =
        utf8proc_iterate(string.ptr()->flexData() + byteIdx, cap, &maybeCp);
    assert(cpWidth > 0); // Strings should always have been created from valid UTF-8
    auto const cp = uint32_t(maybeCp);

    state->regs[retReg] = Char{cp};
    return PrimopRes::CONTINUE;
}

// TODO: Very similar to `primopStringIteratorPeek`, but would it make sense to abstract out what is
// mostly sanity checks on the iterator?
PrimopRes primopStringIteratorNext(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    StringIterator* const iter =
        HRef<StringIterator>::fromUnchecked(state->regs[firstArgReg]).ptr();

    ORef const maybeString = iter->string;
    if (!isString(state, maybeString)) {
        return primopError(state, createTypeError(state, state->types.string, maybeString));
    }
    auto const string = HRef<String>::fromUnchecked(maybeString);
    ORef const maybeByteIdx = iter->byteIdx;
    if (!isFixnum(maybeByteIdx)) {
        return primopError(state, createTypeError(state, state->types.fixnum, maybeString));
    }
    ssize_t const byteIdx = Fixnum::fromUnchecked(maybeByteIdx).val();
    ssize_t const cap = string.ptr()->flexCount().val();

    if (byteIdx >= cap) {
        state->regs[retReg] = state->singletons.end;
        return PrimopRes::CONTINUE;
    }

    int32_t maybeCp;
    ssize_t cpWidth = utf8proc_iterate(string.ptr()->flexData() + byteIdx, cap, &maybeCp);
    assert(cpWidth > 0); // Strings should always have been created from valid UTF-8
    auto const cp = uint32_t(maybeCp);

    iter->byteIdx = Fixnum{int64_t(byteIdx) + cpWidth};
    state->regs[retReg] = Char{cp};
    return PrimopRes::CONTINUE;
}

PrimopRes primopStringToSymbol(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const str = HRef<String>::fromUnchecked(state->regs[firstArgReg]);

    state->regs[retReg] = internHeaped(state, str);

    return PrimopRes::CONTINUE;
}

PrimopRes primopOpenInputFile(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const filename = HRef<String>::fromUnchecked(state->regs[firstArgReg]);

    ORef port = Default;
    if (!InputFile::open(state, static_cast<HRef<InputFile>&>(port), filename)) {
        exit(EXIT_FAILURE); // TODO
    }

    state->regs[retReg] = port;

    return PrimopRes::CONTINUE;
}

PrimopRes primopClosePort(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const port = HRef<InputFile>::fromUnchecked(state->regs[firstArgReg]);

    port.ptr()->file.close();

    return PrimopRes::CONTINUE; // Implicitly returns `port`
}

PrimopRes primopPeekChar(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const port = HRef<InputFile>::fromUnchecked(state->regs[firstArgReg]);

    auto const maybeCp = port.ptr()->file.peec();
    if (maybeCp == EOF) {
        state->regs[retReg] = state->singletons.end;
        return PrimopRes::CONTINUE;
    }
    if (maybeCp < EOF) { exit(EXIT_FAILURE); } // TODO
    auto const cp = uint32_t(maybeCp);

    state->regs[retReg] = Char{cp};
    return PrimopRes::CONTINUE;
}

PrimopRes primopReadChar(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const port = HRef<InputFile>::fromUnchecked(state->regs[firstArgReg]);

    auto const maybeCp = port.ptr()->file.getc();
    if (maybeCp == EOF) {
        state->regs[retReg] = state->singletons.end;
        return PrimopRes::CONTINUE;
    }
    if (maybeCp < EOF) { exit(EXIT_FAILURE); } // TODO
    auto const cp = uint32_t(maybeCp);

    state->regs[retReg] = Char{cp};

    return PrimopRes::CONTINUE;
}

PrimopRes primopWrite(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    print(state, stdout, state->regs[firstArgReg]);

    return PrimopRes::CONTINUE; // TODO: Maybe do not return written value?
}

PrimopRes primopWriteChar(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    uint32_t const c = Char::fromUnchecked(state->regs[firstArgReg]).val();

    uint8_t buf[4];
    ssize_t const width = utf8proc_encode_char(c, buf);
    // TODO: Avoid POSIX format specifier extension:
    printf("%.*s", (int)width, buf);

    return PrimopRes::CONTINUE; // TODO: Maybe do not return written value?
}

PrimopRes primopWriteString(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    String const* const str = HRef<String>::fromUnchecked(state->regs[firstArgReg]).ptr();

    // TODO: Avoid POSIX format spec extension:
    printf("%.*s", int(str->flexCount().val()), str->flexData());

    return PrimopRes::CONTINUE; // TODO: Maybe do not return written value?
}

PrimopRes primopFlushOutputPort(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    if (fflush(stdout) == EOF) {
        state->regs[retReg] = False;
        return PrimopRes::CONTINUE;
    }

    state->regs[retReg] = True;
    return PrimopRes::CONTINUE;
}

PrimopRes primopCurrentSecond(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[retReg] = Fixnum{(int64_t)time(nullptr)};

    return PrimopRes::CONTINUE;
}

PrimopRes primopCurrentJiffy(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[retReg] = Fixnum{(int64_t)clock()};

    return PrimopRes::CONTINUE;
}

PrimopRes primopJiffiesPerSecond(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    state->regs[retReg] = Fixnum{(int64_t)CLOCKS_PER_SEC};

    return PrimopRes::CONTINUE;
}

PrimopRes primopResolve(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    auto const name = HRef<Symbol>::fromUnchecked(state->regs[firstArgReg]);

    FindVarRes const findRes = findVar(state->ns, name);

    state->regs[retReg] = [&]() -> ORef { // IIFE
        switch (findRes.type) {
        case FindVarRes::NS_FOUND_VAR: return findRes.var;
        case FindVarRes::NS_FOUND_VAR_DEST_IDX: return False;
        default: return Default; // Unreachable
        }
    }();
    return PrimopRes::CONTINUE;
}

PrimopRes primopEval(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const expr = state->regs[firstArgReg];
    auto const loc = HRef<Loc>::fromUnchecked(state->regs[firstArgReg + 1]);
    bool const debug = Bool::fromUnchecked(state->regs[firstArgReg + 2]).val();

    CompilationRes const compilationRes =
        compile(state, expr, HRef<Loc>::fromUnchecked(loc), debug);
    if (!compilationRes.success) {
        exit(EXIT_FAILURE); // TODO
    }
    auto const method = compilationRes.val;

    state->regs[calleeReg] = allocClosure(state, method, Fixnum{0l});
    state->entryRegc = calleeReg + 1;
    return PrimopRes::TAILCALL;
}

PrimopRes primopExit(State* state) {
    ORef const maybeErr = checkDomain(state);
    if (isHeaped(maybeErr)) { return primopError(state, maybeErr); }

    ORef const v = state->regs[firstArgReg];

    int const exitCode = isFixnum(v)
        ? int(Fixnum::fromUnchecked(v).val())
        : !eq(v, False) ? EXIT_SUCCESS : EXIT_FAILURE;

    exit(exitCode);
}

} // namespace
