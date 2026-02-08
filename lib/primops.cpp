#include "primops.hpp"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <filesystem>

#include "../deps/utf8proc/utf8proc.h"

#include "util/util.hpp"
#include "write.hpp"
#include "namespace.hpp"
#include "dispatch.hpp"
#include "compiler/compiler.hpp"

namespace {

ORef getErrorHandler(RT const* state) {
    ORef const v = state->errorHandler->val().get();
    if (eq(v, state->singletons.unbound)) {
        PANIC("Unbound *error-handler*");
    }

    return v;
}

PrimopRes primopError(RT* state, ORef err) {
    state->regs[calleeReg] = getErrorHandler(state);
    state->regs[firstArgReg] = err;
    state->entryRegc = firstArgReg + 1;
    return PrimopRes::ERROR;
}

PrimopRes primopArityError(RT* state, HRef<Closure> callee, size_t argc) {
    return primopError(state, createArityError(state, callee, Fixnum{int64_t(argc)}));
}

PrimopRes primopTypeError(RT* state, HRef<Type> type, ORef v) {
    return primopError(state, createTypeError(state, type, v));
}

PrimopRes callBytecode(RT* /*state*/) { return PrimopRes::TAILCALL; }

PrimopRes primopAbort(RT* state) {
    switch (checkDomain(state)) {
    case DomainCheckRes::OK: break;
    case DomainCheckRes::MISSPECULATION: return PrimopRes::MISSPECULATION;
    case DomainCheckRes::ERROR: return PrimopRes::ERROR;
    }

    ORef const error = state->regs[firstArgReg];

    fputs("Runtime error: ", stderr);
    write(state, stderr, error);

    assert(isa(state, state->types.continuation, state->regs[retContReg]));
    auto const cont = HRef<Continuation>::fromUnchecked(state->regs[retContReg]);
    ORef const anyCaller = cont->method;
    if (isa<Method>(*state, anyCaller)) {
        auto const caller = HRef<Method>::fromUnchecked(anyCaller);

        size_t const retPc = uint64_t(cont->pc.val());
        auto const maybeCallLoc = locateCallerPc(state, caller, retPc);

        ORef const maybeCallerName = caller->maybeName;
        if (isa<Symbol>(*state, maybeCallerName)) {
            fputs(" in ", stderr);
            write(state, stderr, HRef<Symbol>::fromUnchecked(maybeCallerName));
        }

        fputs(" at ", stderr);

        if (maybeCallLoc.hasVal) { maybeCallLoc.val.print(*state, stderr); }
    } else {
        // FIXME: Exit continuation should have a method that inherits toplevel thunk location to
        // make this work.
        assert(false);
    }

    putc('\n', stderr);

    return PrimopRes::ABORT;
}

PrimopRes PrimopApplyArray::uncheckedInvoke(RT* state) {
    ORef const callee = state->regs[firstArgReg];
    // Could also be an `<array!>`, but we "illegally" cast that here to avoid duplicating this
    // function for no actual benefit:
    HRef<Array> argsRef = HRef<Array>::fromUnchecked(state->regs[firstArgReg + 1]);
    ORef const* args = argsRef->flexData();
    size_t argc = (uint64_t)argsRef->flexCount().val();

    // Dispatch:
    if (!calleeClosureForArgs(state, callee, args, argc)) {
        return PrimopRes::ERROR; // Finish panic setup
    }
    assert(isa<Closure>(*state, state->regs[calleeReg]));
    HRef<Closure> const closure = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);

    // Check domain (if not already checked by dispatch):
    switch (checkDomainForArgs(state, closure, args, argc)) {
    case DomainCheckRes::OK: break;
    case DomainCheckRes::MISSPECULATION: return PrimopRes::MISSPECULATION;
    case DomainCheckRes::ERROR: return PrimopRes::ERROR;
    }

    ORef const method = closure->method;
    assert(isa<Method>(*state, method));
    auto const methodRef = HRef<Method>::fromUnchecked(method);

    // Put args in place:
    if (!isHeaped(methodRef->code) || !methodRef->hasVarArg.val()){
        memcpy(state->regs + firstArgReg, args, argc * sizeof(ORef));
    } else { // Non-primop with varargs:
        size_t const arity = (uint64_t)methodRef->flexCount().val();
        size_t const minArity = arity - 1;
        size_t const varargCount = argc - minArity;

        // Fixed args:
        memcpy(state->regs + firstArgReg, args, minArity * sizeof(ORef));

        // Varargs:
        auto const argsRefG = state->pushRoot(&argsRef);
        HRef<ArrayMut> const varargsRef = createArrayMut(state, Fixnum((intptr_t)varargCount));
        args = argsRef->flexData(); // Post-GC reload
        memcpy((void*)varargsRef->flexData(), args + minArity, varargCount * sizeof(ORef));

        state->regs[firstArgReg + minArity] = varargsRef;

        argc = arity;
    }

    state->entryRegc = (uint8_t)(firstArgReg + argc);
    state->domainChecking = RT::DomainChecking::SKIP;
    return PrimopRes::TAILAPPLY;
}

PrimopRes PrimopApplyArrayMut::uncheckedInvoke(RT* state) {
    return PrimopApplyArray::uncheckedInvoke(state);
}

PrimopRes PrimopApplyList::uncheckedInvoke(RT* state) {
    ORef const callee = state->regs[firstArgReg];
    ORef args = state->regs[firstArgReg + 1];

    // Dispatch:
    if (!calleeClosureForArglist(state, callee, args)) {
        return PrimopRes::ERROR; // Finish panic setup
    }
    HRef<Closure> const closure = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);

    ORef const method = closure->method;
    assert(isa<Method>(*state, method));
    auto const methodRef = HRef<Method>::fromUnchecked(method);

    // Put args in place and check them (if not already checked by dispatch):
    size_t const arity = (uint64_t)methodRef->flexCount().val();
    size_t argc = 0;
    if (state->domainChecking != RT::DomainChecking::SKIP) {
        bool const hasVarArg = methodRef->hasVarArg.val();
        size_t const minArity = !hasVarArg ? arity : arity - 1;

        // Fixed args:
        for (; argc < minArity; ++argc) {
            if (isa<Pair>(*state, args)) {
                auto const argsPair = HRef<Pair>::fromUnchecked(args);

                ORef const arg = argsPair->car().get();

                // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                assert(isa<Type>(*state, methodRef->domain()[argc].get()));
                HRef<Type> const type = HRef<Type>::fromUnchecked(methodRef->domain()[argc].get());
                if (!isa(state, type, arg)) {
                    ORef const err = createTypeError(state, type, arg);
                    return primopError(state, err);
                }

                state->regs[firstArgReg + argc] = arg;

                args = argsPair->cdr().get();
            } else if (isEmptyList(state, args)) {
                ORef const err = // Insufficient args
                    createArityError(state, closure, Fixnum((intptr_t)argc));
                return primopError(state, err);
            } else {
                assert(false); // TODO: Proper improper args error
            }
        }

        if (!hasVarArg){ // Fixed arity => check that no more args remain:
            if (!isEmptyList(state, args)) {
                for (; true; ++argc) {
                    if (isa<Pair>(*state, args)) {
                        auto const argsPair = HRef<Pair>::fromUnchecked(args);
                        args = argsPair->cdr().get();
                    } else if (isEmptyList(state, args)) {
                        break;
                    } else {
                        assert(false); // TODO: Proper improper args error
                    }
                }

                ORef const err = // Excessive args
                    createArityError(state, closure, Fixnum((intptr_t)argc));
                return primopError(state, err);
            }
        } else if (!isHeaped(methodRef->code)) { // Primop varargs:
            assert(isa<Type>(*state, methodRef->domain()[minArity].get()));
            HRef<Type> type = HRef<Type>::fromUnchecked(methodRef->domain()[minArity].get());
            for (; true; ++argc) {
                if (isa<Pair>(*state, args)) {
                    auto const argsPair = HRef<Pair>::fromUnchecked(args);

                    ORef const arg = argsPair->car().get();

                    // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                    if (!isa(state, type, arg)) {
                        ORef const err = createTypeError(state, type, arg);
                        return primopError(state, err);
                    }

                    state->regs[firstArgReg + argc] = arg;

                    args = argsPair->cdr().get();
                } else if (isEmptyList(state, args)) {
                    break;
                } else {
                    assert(false); // TODO: Proper improper args error
                }
            }
        } else { // Non-primop varargs:
            auto const argsG = state->pushRoot(&args);

            assert(isa<Type>(*state, methodRef->domain()[minArity].get()));
            HRef<Type> type = HRef<Type>::fromUnchecked(methodRef->domain()[minArity].get());
            auto const typeG = state->pushRoot(&type);
            size_t bufCap = 10;
            HRef<ArrayMut> varargsBufRef = createArrayMut(state, Fixnum((intptr_t)bufCap));
            auto const varargsBufRefG = state->pushRoot(&varargsBufRef);
            auto varargsBuf = const_cast<ORef*>(varargsBufRef->items().data()); // Cast for init
            size_t varargCount = 0;
            for (size_t i = 0; true; ++i, ++varargCount) {
                if (isa<Pair>(*state, args)) {
                    auto argsPair = HRef<Pair>::fromUnchecked(args);

                    ORef arg = argsPair->car().get();

                    // OPTIMIZE: Skip type check if no typed params (= not a specialization):
                    if (!isa(state, type, arg)) {
                        ORef const err = createTypeError(state, type, arg);
                        return primopError(state, err);
                    }

                    if (i == bufCap) {
                        size_t const newBufCap = bufCap + bufCap * 2;

                        auto const argG = state->pushRoot(&arg);
                        HRef<ArrayMut> const newVarargsBufRef =
                            createArrayMut(state, Fixnum((intptr_t)newBufCap));
                        argsPair = HRef<Pair>::fromUnchecked(args); // Post-GC reload
                        varargsBuf =
                            const_cast<ORef*>(varargsBufRef->items().data()); // Post-GC reload
                        ORef* const newVarargsBuf =
                            const_cast<ORef*>(newVarargsBufRef->items().data()); // Cast for init
                        memcpy(newVarargsBuf, varargsBuf, bufCap * sizeof(ORef));

                        bufCap = newBufCap;
                        varargsBufRef = newVarargsBufRef;
                        varargsBuf = newVarargsBuf;
                    }
                    varargsBuf[i] = arg;

                    args = argsPair->cdr().get();
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
                    varargsBuf = const_cast<ORef*>(varargsBufRef->items().data()); // Post-GC reload
                    memcpy(const_cast<ORef*>(varargsRef->flexData()), varargsBuf,
                           varargCount * sizeof(ORef));
                    return varargsRef;
                } else {
                    return varargsBufRef;
                }
            }();

            state->regs[firstArgReg + minArity] = varargsRef;

            argc = minArity + varargCount;
        }
    } else { // `state->domainChecking == RT::DomainChecking::SKIP`
        bool const hasVarArg = methodRef->hasVarArg.val();
        size_t const minArity = !hasVarArg ? arity : arity - 1;

        // Fixed args:
        for (size_t i = 0; i < minArity; ++i) {
            // Arity already checked to be correct so `args` *must* be a pair:
            assert(isa(state, state->types.pair, args));
            auto const argsPair = HRef<Pair>::fromUnchecked(args);

            state->regs[firstArgReg + i] = argsPair->car().get();

            args = argsPair->cdr().get();
        }

        if (hasVarArg){ // Vararg:
            if (!isHeaped(methodRef->code)) { // Primop:
                // Arity already checked to be correct so `args` *must* be a proper list:
                for (size_t i = minArity; isa<Pair>(*state, args); ++i) {
                    auto const argsPair = HRef<Pair>::fromUnchecked(args);

                    state->regs[firstArgReg + i] = argsPair->car().get();

                    args = argsPair->cdr().get();
                }
            } else { // Non-primop:
                auto const argsG = state->pushRoot(&args);

                size_t bufCap = 10;
                HRef<ArrayMut> varargsBufRef = createArrayMut(state, Fixnum((intptr_t)bufCap));
                auto const varargsBufRefG = state->pushRoot(&varargsBufRef);
                ORef* varargsBuf =
                    const_cast<ORef*>(varargsBufRef->items().data()); // Cast for init
                size_t varargCount = 0;
                for (size_t i = 0; true; ++i, ++varargCount) {
                    if (isa<Pair>(*state, args)) {
                        auto argsPair = HRef<Pair>::fromUnchecked(args);

                        if (i == bufCap) {
                            size_t const newBufCap = bufCap + bufCap * 2;

                            HRef<ArrayMut> const newVarargsBufRef =
                                createArrayMut(state, Fixnum((intptr_t)newBufCap));
                            argsPair = HRef<Pair>::fromUnchecked(args); // Post-GC reload
                            varargsBuf =
                                const_cast<ORef*>(varargsBufRef->items().data()); // Post-GC reload
                            ORef* const newVarargsBuf =
                                const_cast<ORef*>(newVarargsBufRef->items().data());
                            memcpy(newVarargsBuf, varargsBuf, bufCap * sizeof(ORef));

                            bufCap = newBufCap;
                            varargsBufRef = newVarargsBufRef;
                            varargsBuf = newVarargsBuf;
                        }
                        varargsBuf[i] = argsPair->car().get();

                        args = argsPair->cdr().get();
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
                        varargsBuf =
                            const_cast<ORef*>(varargsBufRef->items().data()); // Post-GC reload
                        memcpy(const_cast<ORef*>(varargsRef->flexData()), varargsBuf,
                               varargCount * sizeof(ORef));
                        return varargsRef;
                    } else {
                        return varargsBufRef;
                    }
                }();

                state->regs[firstArgReg + minArity] = varargsRef;

                argc = minArity + varargCount;
            }
        }
    }

    state->entryRegc = (uint8_t)(firstArgReg + argc);
    state->domainChecking = RT::DomainChecking::SKIP;
    return PrimopRes::TAILAPPLY;
}

PrimopRes PrimopCallCC::uncheckedInvoke(RT* state) {
    state->regs[calleeReg] = state->regs[firstArgReg];
    state->regs[firstArgReg] = state->regs[retContReg];
    state->entryRegc = firstArgReg + 1;
    return PrimopRes::TAILCALL;
}

PrimopRes PrimopContinue::uncheckedInvoke(RT* state) {
    state->regs[retContReg] = state->regs[firstArgReg];
    state->regs[retReg] = state->regs[firstArgReg + 1];
    return PrimopRes::CONTINUE;
}

PrimopRes PrimopIdentical::uncheckedInvoke(RT* state) {
    ORef const x = state->regs[firstArgReg];
    ORef const y = state->regs[firstArgReg + 1];

    state->regs[retReg] = Bool(eq(x, y));

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopTypeOf::uncheckedInvoke(RT* state) {
    state->regs[retReg] = typeOf(state, state->regs[firstArgReg]);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopMakeSlotsType::uncheckedInvoke(RT* state) {
    auto const name = HRef<Symbol>::fromUnchecked(state->regs[firstArgReg]);
    auto const slotCount = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
    auto const isFlex = Bool::fromUnchecked(state->regs[firstArgReg + 2]);

    state->regs[retReg] = createSlotsType(state, name, slotCount, isFlex);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopMake::uncheckedInvoke(RT* state) {
    HRef<Type> type = HRef<Type>::fromUnchecked(state->regs[firstArgReg]);
    uint8_t const callArity = state->entryRegc - firstArgReg;

    if (!type->isFlex.val()) {
        // Alloc:
        Object* ptr = state->heap.tryAlloc(&*type);
        if (mustCollect(ptr)) {
            collect(state);
            type = HRef<Type>::fromUnchecked(state->regs[firstArgReg]);
            ptr = state->heap.allocOrDie(&*type);
        }

        // Init:
        if (!type->isBytes.val()) {
            size_t const fieldCount = (uint64_t)type->minSize.val() / sizeof(ORef);
            if (callArity - 1u != fieldCount) {
                // TODO: Proper error (but not really an arity error!):
                PANIC("Constructor arity %d != %lu", callArity - 1u, fieldCount);
            }

            {
                ORef* const fields = (ORef*)ptr;
                for (size_t i = 0; i < fieldCount; ++i) {
                    fields[i] = state->regs[firstArgReg + 1 + i];
                }
            }
        } else {
            PANIC("TODO");
        }

        state->regs[retReg] = HRef<Object>(ptr);

        return PrimopRes::CONTINUE;
    } else {
        PANIC("TODO");
    }
}

PrimopRes PrimopSlotGet::uncheckedInvoke(RT* state) {
    ORef const v = state->regs[firstArgReg];
    size_t const slotIdx = (uint64_t)Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    Type const* const type = typePtrOf(state, v);
    if (!type->isBytes.val()) {
        auto const obj = HRef<Object>::fromUnchecked(v);

        size_t const slotCount = (uintptr_t)type->minSize.val() / sizeof(ORef);
        if (slotIdx >= slotCount) {
            assert(false); // TODO: Proper bounds error
        }

        auto const slots = reinterpret_cast<ORef const*>(&*obj);
        state->regs[retReg] = slots[slotIdx];
    } else {
        assert(false); // TODO
    }

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopSlotSet::uncheckedInvoke(RT* state) {
    ORef const v = state->regs[firstArgReg];
    size_t const slotIdx = (uint64_t)Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();
    ORef slotV = state->regs[firstArgReg + 2];

    Type const* const type = typePtrOf(state, v);
    if (!type->isBytes.val()) {
        auto const obj = HRef<Object>::fromUnchecked(v);

        size_t const slotCount = (uintptr_t)type->minSize.val() / sizeof(ORef);
        if (slotIdx >= slotCount) {
            assert(false); // TODO: Proper bounds error
        }

        auto slots = SlotsMut{&*obj, reinterpret_cast<ORef*>(&*obj)};
        slots[slotIdx].set(*state, slotV);
        slotV = state->regs[firstArgReg + 2]; // Reload in case write barrier caused GC
    } else {
        assert(false); // TODO
    }

    // Beats returning `v`; at least consistent with e.g. `def` atm
    state->regs[retReg] = slotV;

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopMakeFlex::uncheckedInvoke(RT* state) {
    HRef<Type> type = HRef<Type>::fromUnchecked(state->regs[firstArgReg]);
    Fixnum const count = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);

    if (type->isFlex.val()) {
        Object* ptr = state->heap.tryAllocFlex(&*type, count);
        if (mustCollect(ptr)) {
            collect(state);
            type = HRef<Type>::fromUnchecked(state->regs[firstArgReg]);
            ptr = state->heap.allocFlexOrDie(&*type, count);
        }

        state->regs[retReg] = HRef<Object>(ptr);

        return PrimopRes::CONTINUE;
    } else {
        PANIC("TODO");
    }
}

PrimopRes PrimopFlexCount::uncheckedInvoke(RT* state) {
    ORef const v = state->regs[firstArgReg];

    Type const* const type = typePtrOf(state, v);
    if (!type->isFlex.val()) {
        assert(false); // TODO: Proper nonflex error
    }
    auto const obj = HRef<Object>::fromUnchecked(v);

    state->regs[retReg] = uncheckedFlexHeader(obj)->count;

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFlexGet::uncheckedInvoke(RT* state) {
    ORef const v = state->regs[firstArgReg];
    int64_t const i = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    Type const* const type = typePtrOf(state, v);
    if (!type->isFlex.val()) {
        assert(false); // TODO: Proper nonflex error
    }
    if (type->isBytes.val()) {
        assert(false); // TODO: Proper nonslots error
    }
    auto const obj = HRef<Object>::fromUnchecked(v);

    void const* const ptr = &*obj;
    int64_t const count = ((FlexHeader const*)ptr - 1)->count.val();
    if (i < 0 || i >= count) {
        assert(false); // TODO: Proper bounds error
    }

    ORef const* const flexSlots = (ORef const*)((char const*)ptr + type->minSize.val());
    state->regs[retReg] = flexSlots[i];

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFlexSet::uncheckedInvoke(RT* state) {
    ORef const v = state->regs[firstArgReg];
    int64_t const i = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();
    ORef const iv = state->regs[firstArgReg + 2];

    Type const* const type = typePtrOf(state, v);
    if (!type->isFlex.val()) {
        assert(false); // TODO: Proper nonflex error
    }
    if (type->isBytes.val()) {
        assert(false); // TODO: Proper nonslots error
    }

    Object* const ptr = &*HRef<Object>::fromUnchecked(v);
    int64_t const count = ((FlexHeader const*)ptr - 1)->count.val();
    if (i < 0 || i >= count) {
        assert(false); // TODO: Proper bounds error
    }

    auto flexSlots = SlotsMut{ptr, (ORef*)((char const*)ptr + type->minSize.val())};
    flexSlots[size_t(i)].set(*state, iv);
    // Once again most convenient and consistent to just return this
    state->regs[retReg] = state->regs[firstArgReg + 2]; // Reload in case write barrier caused GC

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFlexCopy::uncheckedInvoke(RT* state) {
    ORef const dest = state->regs[firstArgReg];
    intptr_t const offsetS = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();
    ORef const src = state->regs[firstArgReg + 2];
    intptr_t const startS = Fixnum::fromUnchecked(state->regs[firstArgReg + 3]).val();
    intptr_t const endS = Fixnum::fromUnchecked(state->regs[firstArgReg + 4]).val();
    Type const* const destType = typePtrOf(state, dest);
    Type const* const srcType = typePtrOf(state, src);

    if (!destType->isFlex.val()) { PANIC("TODO: Proper nonflex error"); }
    Bool const isBytesRef = destType->isBytes;
    if (!srcType->isFlex.val()) { PANIC("TODO: Proper nonflex error"); }
    if (!eq(srcType->isBytes, isBytesRef)) {
        PANIC("TODO: Proper bytes-vs-slots error");
    }
    auto destObj = HRef<Object>::fromUnchecked(dest);
    auto srcObj = HRef<Object>::fromUnchecked(src);

    size_t const destCount = (uintptr_t)uncheckedFlexHeader(destObj)->count.val();
    size_t const srcCount = (uintptr_t)uncheckedFlexHeader(srcObj)->count.val();

    if (offsetS < 0) { PANIC("TODO: Proper bounds error"); } // Negative index
    size_t const offset = (uintptr_t)offsetS;
    if (offset > destCount) { PANIC("TODO: Proper bounds error"); }
    if (startS < 0) { PANIC("TODO: Proper bounds error"); } // Negative index
    size_t const start = (uintptr_t)startS;
    if (start > srcCount) { PANIC("TODO: Proper bounds error"); }
    if (endS < startS) { PANIC("TODO: Proper bounds error"); }
    size_t const end = (uintptr_t)endS;

    size_t const copyCount = end - start;
    size_t const copySpace = destCount - offset;
    if (copyCount > copySpace) { PANIC("TODO: Proper bounds error"); }

    if (!destType->isBytes.val()) {
        if (!state->heap.writeBarrier(&*destObj)) {
            auto const destG = state->pushRoot(&destObj);
            auto const srcG = state->pushRoot(&srcObj);
            collect(state);
        }
    }

    auto const destVals = (char*)uncheckedUntypedFlexPtr(destObj);
    auto const srcVals = (char const*)uncheckedUntypedFlexPtr(srcObj);
    size_t const elemSize = isBytesRef.val() ? sizeof(uint8_t) : sizeof(ORef);
    memmove(destVals, srcVals, copyCount * elemSize);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFlexClone::uncheckedInvoke(RT* state) {
    ORef const src = state->regs[firstArgReg];
    intptr_t const startS = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();
    intptr_t const endS = Fixnum::fromUnchecked(state->regs[firstArgReg + 2]).val();
    HRef<Type> type = typeOf(state, src);

    if (!type->isFlex.val()) { PANIC("TODO: Proper nonflex error"); }
    auto srcObj = HRef<Object>::fromUnchecked(src);

    size_t const srcCount = (uintptr_t)uncheckedFlexHeader(srcObj)->count.val();

    if (startS < 0) { PANIC("TODO: Proper bounds error"); } // Negative index
    size_t const start = (uintptr_t)startS;
    if (endS < 0) { PANIC("TODO: Proper bounds error"); } // Negative index
    size_t const end = (uintptr_t)endS;
    if (end > srcCount) { PANIC("TODO: Proper bounds error"); }
    if (start > end) { PANIC("TODO: Proper bounds error"); }

    size_t const copyCount = end - start;

    Object* dest = state->heap.tryAllocFlex(&*type, Fixnum{int64_t(copyCount)});
    if (mustCollect(dest)) {
        auto const srcObjG = state->pushRoot(&srcObj);
        auto const typeRefG = state->pushRoot(&type);
        collect(state);
        dest = state->heap.allocFlexOrDie(&*type, Fixnum{int64_t(copyCount)});
    }

    auto const minSize = size_t(type->minSize.val());
    size_t const elemSize = type->isBytes.val() ? sizeof(uint8_t) : sizeof(ORef);
    memcpy(dest, (char*)uncheckedUntypedFlexPtr(srcObj) + start * elemSize,
           minSize + copyCount * elemSize);

    state->regs[retReg] = HRef{dest};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFxAdd::uncheckedInvoke(RT* state) {
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
        return primopError(state, createOverflowError(state, f, xRef, yRef));
    }

    state->regs[retReg] = Fixnum{res};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFxSub::uncheckedInvoke(RT* state) {
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
        return primopError(state, createOverflowError(state, f, xRef, yRef));
    }

    state->regs[retReg] = Fixnum{res};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFxMul::uncheckedInvoke(RT* state) {
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
        return primopError(state, createOverflowError(state, f, xRef, yRef));
    }
#endif
// else should not compile due to missing `res`

    state->regs[retReg] = Fixnum{res};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFxQuot::uncheckedInvoke(RT* state) {
    intptr_t const x = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();
    intptr_t const y = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    if (y == 0) {
        HRef<Closure> const f = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
        Fixnum const xRef = Fixnum::fromUnchecked(state->regs[firstArgReg]);
        Fixnum const yRef = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
        return primopError(state, createDivByZeroError(state, f, xRef, yRef));
    }

    if (x == Fixnum::min && y == -1) {
        // Due to two's complement `-fixnumMin == fixnumMax + 1` but this is the only overflowing
        // combination.

        HRef<Closure> const f = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
        Fixnum const xRef = Fixnum::fromUnchecked(state->regs[firstArgReg]);
        Fixnum const yRef = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]);
        return primopError(state, createOverflowError(state, f, xRef, yRef));
    }

    state->regs[retReg] = Fixnum{x / y};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFxLt::uncheckedInvoke(RT* state) {
    intptr_t const x = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();
    intptr_t const y = Fixnum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Bool(x < y);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFixnumToFlonum::uncheckedInvoke(RT* state) {
    intptr_t const n = Fixnum::fromUnchecked(state->regs[firstArgReg]).val();

    state->regs[retReg] = Flonum((double)n);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFlAdd::uncheckedInvoke(RT* state) {
    double const x = Flonum::fromUnchecked(state->regs[firstArgReg]).val();
    double const y = Flonum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Flonum(x + y);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFlSub::uncheckedInvoke(RT* state) {
    double const x = Flonum::fromUnchecked(state->regs[firstArgReg]).val();
    double const y = Flonum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Flonum(x - y);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFlMul::uncheckedInvoke(RT* state) {
    double const x = Flonum::fromUnchecked(state->regs[firstArgReg]).val();
    double const y = Flonum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Flonum(x * y);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFlDiv::uncheckedInvoke(RT* state) {
    double const x = Flonum::fromUnchecked(state->regs[firstArgReg]).val();
    double const y = Flonum::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Flonum(x / y);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopCharLt::uncheckedInvoke(RT* state) {
    uint32_t const c1 = Char::fromUnchecked(state->regs[firstArgReg]).val();
    uint32_t const c2 = Char::fromUnchecked(state->regs[firstArgReg + 1]).val();

    state->regs[retReg] = Bool{c1 < c2};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopCharToInteger::uncheckedInvoke(RT* state) {
    uint32_t const c = Char::fromUnchecked(state->regs[firstArgReg]).val();

    state->regs[retReg] = Fixnum{int64_t(c)};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopCharIsAlphabetic::uncheckedInvoke(RT* state) {
    auto const c = int32_t(Char::fromUnchecked(state->regs[firstArgReg]).val());

    utf8proc_category_t const cat = utf8proc_category(c);
    bool const isAlphabetic = (UTF8PROC_CATEGORY_LU <= cat && cat <= UTF8PROC_CATEGORY_LO)
                              || cat == UTF8PROC_CATEGORY_NL;
    state->regs[retReg] = Bool{isAlphabetic};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopCharIsNumeric::uncheckedInvoke(RT* state) {
    auto const c = Char::fromUnchecked(state->regs[firstArgReg]).val();

    state->regs[retReg] = Bool{utf8proc_category(int32_t(c)) == UTF8PROC_CATEGORY_ND};
    return PrimopRes::CONTINUE;
}

PrimopRes PrimopCharIsWhitespace::uncheckedInvoke(RT* state) {
    auto const c = Char::fromUnchecked(state->regs[firstArgReg]).val();

    utf8proc_category_t const cat = utf8proc_category(int32_t(c));
    bool const isWhitespace =
        (UTF8PROC_CATEGORY_ZS <= cat && cat <= UTF8PROC_CATEGORY_ZP) // Space cats
        || (9 <= c && c <= 13) || c == 133; // Whitespace ctrls
    state->regs[retReg] = Bool{isWhitespace};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopArrayMutToString::uncheckedInvoke(RT* state) {
    auto vs = HRef<ArrayMut>::fromUnchecked(state->regs[firstArgReg]);
    auto const vsG = state->pushRoot(&vs);

    auto const cpCount = size_t(vs->flexCount().val());
    auto tmp = createByteArrayMut(state, Fixnum{int64_t(cpCount * 4)});
    auto const tmpRefG = state->pushRoot(&tmp);
    auto const cps = vs->flexData();
    auto const tmpData = const_cast<uint8_t*>(tmp->flexData());
    ssize_t stringSize = 0;
    for (size_t i = 0; i < cpCount; ++i) {
        ORef const v = cps[i];
        if (!Char::contains(v)) {
            return primopError(state, createTypeError(state, state->types.charr, v));
        }
        auto const cp = Char::fromUnchecked(v).val();

        stringSize += utf8proc_encode_char(int32_t(cp), tmpData + stringSize);
    }

    String* const res = allocString(state, Fixnum{stringSize});
    memcpy(const_cast<uint8_t*>(res->flexData()), tmp->flexData(), size_t(stringSize));

    state->regs[retReg] = HRef{res};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopStringIteratorPeek::uncheckedInvoke(RT* state) {
    auto const iter = HRef<StringIterator>::fromUnchecked(state->regs[firstArgReg]);

    ORef const maybeString = iter->string;
    if (!isa<String>(*state, maybeString)) {
        return primopError(state, createTypeError(state, state->types.string, maybeString));
    }
    auto const string = HRef<String>::fromUnchecked(maybeString);
    ORef const maybeByteIdx = iter->byteIdx();
    if (!Fixnum::contains(maybeByteIdx)) {
        return primopError(state, createTypeError(state, state->types.fixnum, maybeString));
    }
    ssize_t const byteIdx = Fixnum::fromUnchecked(maybeByteIdx).val();
    ssize_t const cap = string->flexCount().val();

    if (byteIdx >= cap) {
        state->regs[retReg] = state->singletons.end;
        return PrimopRes::CONTINUE;
    }

    int32_t maybeCp;
    [[maybe_unused]] ssize_t cpWidth =
        utf8proc_iterate(string->flexData() + byteIdx, cap, &maybeCp);
    assert(cpWidth > 0); // Strings should always have been created from valid UTF-8
    auto const cp = uint32_t(maybeCp);

    state->regs[retReg] = Char{cp};
    return PrimopRes::CONTINUE;
}

// TODO: Very similar to `primopStringIteratorPeek`, but would it make sense to abstract out what is
// mostly sanity checks on the iterator?
PrimopRes PrimopStringIteratorNext::uncheckedInvoke(RT* state) {
    auto const iter = HRef<StringIterator>::fromUnchecked(state->regs[firstArgReg]);

    ORef const maybeString = iter->string;
    if (!isa<String>(*state, maybeString)) {
        return primopError(state, createTypeError(state, state->types.string, maybeString));
    }
    auto const string = HRef<String>::fromUnchecked(maybeString);
    ORef const maybeByteIdx = iter->byteIdx();
    if (!Fixnum::contains(maybeByteIdx)) {
        return primopError(state, createTypeError(state, state->types.fixnum, maybeString));
    }
    ssize_t const byteIdx = Fixnum::fromUnchecked(maybeByteIdx).val();
    ssize_t const cap = string->flexCount().val();

    if (byteIdx >= cap) {
        state->regs[retReg] = state->singletons.end;
        return PrimopRes::CONTINUE;
    }

    int32_t maybeCp;
    ssize_t cpWidth = utf8proc_iterate(string->flexData() + byteIdx, cap, &maybeCp);
    assert(cpWidth > 0); // Strings should always have been created from valid UTF-8
    auto const cp = uint32_t(maybeCp);

    iter->setByteIdx(Fixnum{int64_t(byteIdx) + cpWidth});
    state->regs[retReg] = Char{cp};
    return PrimopRes::CONTINUE;
}

PrimopRes PrimopStringToSymbol::uncheckedInvoke(RT* state) {
    auto const str = HRef<String>::fromUnchecked(state->regs[firstArgReg]);

    state->regs[retReg] = internHeaped(state, str);

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopFileExists::uncheckedInvoke(RT* state) {
    auto const filename = HRef<String>::fromUnchecked(state->regs[firstArgReg]);

    auto const cppFilename = std::string_view{
        reinterpret_cast<char const*>(filename->str().data), filename->str().len};

    state->regs[retReg] = Bool{std::filesystem::exists(cppFilename)};
    return PrimopRes::CONTINUE;
}

PrimopRes PrimopOpenInputFile::uncheckedInvoke(RT* state) {
    auto const filename = HRef<String>::fromUnchecked(state->regs[firstArgReg]);

    ORef port = Default;
    if (!InputFile::open(state, static_cast<HRef<InputFile>&>(port), filename)) {
        PANIC("TODO");
    }

    state->regs[retReg] = port;

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopClosePort::uncheckedInvoke(RT* state) {
    auto const port = HRef<InputFile>::fromUnchecked(state->regs[firstArgReg]);

    port->file.close();

    return PrimopRes::CONTINUE; // Implicitly returns `port`
}

PrimopRes PrimopPeekChar::uncheckedInvoke(RT* state) {
    auto const port = HRef<InputFile>::fromUnchecked(state->regs[firstArgReg]);

    auto const maybeCp = port->file.peec();
    if (maybeCp == EOF) {
        state->regs[retReg] = state->singletons.end;
        return PrimopRes::CONTINUE;
    }
    if (maybeCp < EOF) { PANIC("TODO"); }
    auto const cp = uint32_t(maybeCp);

    state->regs[retReg] = Char{cp};
    return PrimopRes::CONTINUE;
}

PrimopRes PrimopReadChar::uncheckedInvoke(RT* state) {
    auto const port = HRef<InputFile>::fromUnchecked(state->regs[firstArgReg]);

    auto const maybeCp = port->file.getc();
    if (maybeCp == EOF) {
        state->regs[retReg] = state->singletons.end;
        return PrimopRes::CONTINUE;
    }
    if (maybeCp < EOF) { PANIC("TODO"); }
    auto const cp = uint32_t(maybeCp);

    state->regs[retReg] = Char{cp};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopWrite::uncheckedInvoke(RT* state) {
    write(state, stdout, state->regs[firstArgReg]);

    return PrimopRes::CONTINUE; // TODO: Maybe do not return written value?
}

PrimopRes PrimopWriteChar::uncheckedInvoke(RT* state) {
    uint32_t const c = Char::fromUnchecked(state->regs[firstArgReg]).val();

    uint8_t buf[4];
    ssize_t const width = utf8proc_encode_char(c, buf);
    // TODO: Avoid POSIX format specifier extension:
    printf("%.*s", (int)width, buf);

    return PrimopRes::CONTINUE; // TODO: Maybe do not return written value?
}

PrimopRes PrimopWriteString::uncheckedInvoke(RT* state) {
    auto const str = HRef<String>::fromUnchecked(state->regs[firstArgReg]);

    // TODO: Avoid POSIX format spec extension:
    printf("%.*s", int(str->flexCount().val()), str->flexData());

    return PrimopRes::CONTINUE; // TODO: Maybe do not return written value?
}

PrimopRes PrimopFlushOutputPort::uncheckedInvoke(RT* state) {
    if (fflush(stdout) == EOF) {
        state->regs[retReg] = False;
        return PrimopRes::CONTINUE;
    }

    state->regs[retReg] = True;
    return PrimopRes::CONTINUE;
}

PrimopRes PrimopCurrentSecond::uncheckedInvoke(RT* state) {
    state->regs[retReg] = Fixnum{(int64_t)time(nullptr)};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopCurrentJiffy::uncheckedInvoke(RT* state) {
    state->regs[retReg] = Fixnum{(int64_t)clock()};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopJiffiesPerSecond::uncheckedInvoke(RT* state) {
    state->regs[retReg] = Fixnum{(int64_t)CLOCKS_PER_SEC};

    return PrimopRes::CONTINUE;
}

PrimopRes PrimopResolve::uncheckedInvoke(RT* state) {
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

PrimopRes PrimopEval::uncheckedInvoke(RT* state) {
    ORef const expr = state->regs[firstArgReg];
    auto const loc = HRef<Loc>::fromUnchecked(state->regs[firstArgReg + 1]);
    bool const debug = Bool::fromUnchecked(state->regs[firstArgReg + 2]).val();

    CompilationRes const compilationRes =
        compile(state, expr, HRef<Loc>::fromUnchecked(loc), debug);
    if (!compilationRes.success) {
        PANIC("TODO");
    }
    auto const method = compilationRes.val;

    state->regs[calleeReg] = allocClosure(state, method, Fixnum{0l});
    state->entryRegc = calleeReg + 1;
    return PrimopRes::TAILCALL;
}

PrimopRes PrimopContinuationCallLoc::uncheckedInvoke(RT* state) {
    auto const cont = HRef<Continuation>::fromUnchecked(state->regs[firstArgReg]);

    if (!isa<Method>(*state, cont->method)) { PANIC("TODO"); }
    auto const method = HRef<Method>::fromUnchecked(cont->method);
    Maybe<ZLoc> const maybeLoc = locateCallerPc(state, method, size_t(cont->pc.val()));

    state->regs[retReg] = [&]() -> ORef {
        if (maybeLoc.hasVal && isa<String>(*state, maybeLoc.val.maybeFilename)) {
            return createLoc(state, HRef<String>::fromUnchecked(maybeLoc.val.maybeFilename),
                             Fixnum{int64_t(maybeLoc.val.srcByteIdx)});
        } else {
            return False;
        }
    }();
    return PrimopRes::CONTINUE;
}

PrimopRes PrimopExit::uncheckedInvoke(RT* state) {
    ORef const v = state->regs[firstArgReg];

    int const exitCode = Fixnum::contains(v)
        ? int(Fixnum::fromUnchecked(v).val())
        : !eq(v, False) ? EXIT_SUCCESS : EXIT_FAILURE;

    exit(exitCode);
}

} // namespace
