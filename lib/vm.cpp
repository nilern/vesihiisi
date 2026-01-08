#include <stdbit.h>
#include <string.h>

#include "vesihiisi.h"
#include "state.hpp"
#include "bytecode.hpp"
#include "dispatch.hpp"
#include "primops.hpp"
#include "namespace.hpp"
#include "flyweights.hpp"

namespace {

VMRes run(State* state, HRef<Closure> selfRef) {
    // TODO: Debug index & type checks & bytecode verifier

    {
        Closure const* const self = selfRef.ptr();
        ORef const method = self->method;
        Method const* const methodPtr = HRef<Method>::fromUnchecked(method).ptr();
        assert(isHeaped(methodPtr->code));
        state->method = method;
        state->code = HRef<ByteArray>::fromUnchecked(methodPtr->code).ptr()->flexData();
        state->pc = 0;
        state->consts = HRef<ArrayMut>::fromUnchecked(methodPtr->consts).ptr()->flexData();
        state->regs[calleeReg] = selfRef.oref();
        state->regs[retContReg] = state->singletons.exit.oref(); // Return continuation
        state->entryRegc = 2;
    }

    for (;/*ever*/;) {
        switch ((Opcode)state->code[state->pc++]) {
        case OP_MOVE: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            state->regs[destReg] = state->regs[srcReg];
        }; continue;

        case OP_SWAP: {
            uint8_t const reg1 = state->code[state->pc++];
            uint8_t const reg2 = state->code[state->pc++];

            ORef const tmp = state->regs[reg1];
            state->regs[reg1] = state->regs[reg2];
            state->regs[reg2] = tmp;
        }; continue;

        case OP_DEF: {
            uint8_t const constIdx = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            ORef const c = state->consts[constIdx];
            if (!isSymbol(state, c)) {
                assert(false); // TODO: Lazily linked Var
            }

            HRef<Symbol> const name = HRef<Symbol>::fromUnchecked(c);
            HRef<Var> var = getVar(state, state->ns, name);

            var.ptr()->val = state->regs[srcReg];
        }; continue;

        case OP_GLOBAL: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            ORef const c = state->consts[constIdx];
            if (!isSymbol(state, c)) {
                assert(false); // TODO: Lazily linked Var
            }

            HRef<Symbol> const name = HRef<Symbol>::fromUnchecked(c);
            FindVarRes const findRes = findVar(state->ns, name);
            if (findRes.type != FindVarRes::NS_FOUND_VAR) {
                // FIXME: Signal that this is a "fatal" (i.e. noncontinuable) error as
                // constructing a working continuation at an arbitrary instruction like this
                // would take a lot of effort while actually using that to recover from this
                // would be a terrible idea. Currently `retContReg` probably holds the return
                // continuation of the current function; to support stack traces we probably
                // have to ensure that it does. But that continuation is definitely not a
                // correct current continuation.
                state->regs[calleeReg] = getErrorHandler(state);
                state->regs[firstArgReg] = createUnboundError(state, name).oref();
                state->entryRegc = firstArgReg + 1;
                goto apply;
            }

            HRef<Var> var = findRes.var;

            ORef const v = var.ptr()->val;
            if (eq(v, state->singletons.unbound.oref())) {
                assert(false); // FIXME: use of unbound var
            }
            state->regs[destReg] = v;
        }; continue;

        case OP_CONST: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            state->regs[destReg] = state->consts[constIdx];
        }; continue;

        case OP_SPECIALIZE: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];
            uint8_t const typeSetByteCount = state->code[state->pc++];
            size_t typeCount = 0;
            // OPTIMIZE:
            for (size_t i = 0; i < typeSetByteCount; ++i) {
                typeCount += stdc_count_ones(state->code[state->pc++]);
            }

            // OPTIMIZE:
            HRef<ArrayMut> const types = createArrayMut(state, Fixnum((intptr_t)typeCount));
            {
                size_t const end = state->pc;
                size_t const start = end - typeSetByteCount;
                for (size_t byteIdx = 0, typeIdx = 0; byteIdx < typeSetByteCount; ++byteIdx) {
                    uint8_t const byte = state->code[start + byteIdx];
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> (UINT8_WIDTH - 1 - bitIdx)) & 1) {
                            size_t const regIdx = UINT8_WIDTH * byteIdx + bitIdx;
                            ORef const maybeType = state->regs[regIdx];
                            if (!isType(state, maybeType)) {
                                return VMRes{}; // TODO: Signal type error properly
                            }
                            types.ptr()->itemsMut()[typeIdx++] = maybeType;
                        }
                    }
                }
            }
            assert(isMethod(state, state->consts[constIdx]));
            HRef<Method> const generic = HRef<Method>::fromUnchecked(state->consts[constIdx]);
            HRef<Method> const method = specialize(state, generic, types);

            state->regs[destReg] = method.oref();
        }; continue;

        case OP_KNOT: {
            uint8_t const destReg = state->code[state->pc++];

            state->regs[destReg] = allocKnot(state).oref();
        }; continue;

        case OP_KNOT_INIT: {
            uint8_t const knotReg = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            assert(isa(state, state->types.knot, state->regs[knotReg]));
            Knot* const knot = HRef<Knot>::fromUnchecked(state->regs[knotReg]).ptr();
            knot->val = state->regs[srcReg];
        }; continue;

        case OP_KNOT_GET: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const knotReg = state->code[state->pc++];

            assert(isa(state, state->types.knot, state->regs[knotReg]));
            Knot const* const knot = HRef<Knot>::fromUnchecked(state->regs[knotReg]).ptr();
            state->regs[destReg] = knot->val;
        }; continue;

        case OP_BR: {
            uint16_t displacement = state->code[state->pc++];
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | state->code[state->pc++];

            state->pc += displacement;
        }; continue;

        case OP_BRF: {
            uint8_t const condReg = state->code[state->pc++];
            uint16_t displacement = state->code[state->pc++];
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | state->code[state->pc++];

            if (!Bool::fromUnchecked(state->regs[condReg]).val()) {
                state->pc += displacement;
            }
        }; continue;

        case OP_RET: {
            assert(eq(typeOf(state, state->regs[retContReg]).oref(),
                      state->types.continuation.oref()));
            HRef<Continuation> const retRef =
                HRef<Continuation>::fromUnchecked(state->regs[retContReg]);
            Continuation const* const ret = retRef.ptr();
            ORef const method = ret->method;
            if (isHeaped(method)) {
                assert(isMethod(state, method));
                Method const* const methodPtr = HRef<Method>::fromUnchecked(method).ptr();
                state->method = method;
                state->code = HRef<ByteArray>::fromUnchecked(methodPtr->code).ptr()->flexData();
                state->pc = (size_t)ret->pc.val();
                state->consts = HRef<ArrayMut>::fromUnchecked(methodPtr->consts).ptr()->flexData();
            } else { // Exit
                return VMRes{.val = state->regs[retReg], .success = true};
            }
        }; continue;

        case OP_CLOSURE: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const methodReg = state->code[state->pc++];
            uint8_t const cloverSetByteCount = state->code[state->pc++];
            size_t cloverCount = 0;
            // OPTIMIZE:
            for (size_t i = 0; i < cloverSetByteCount; ++i) {
                cloverCount += stdc_count_ones(state->code[state->pc++]);
            }

            HRef<Method> const method = HRef<Method>::fromUnchecked(state->regs[methodReg]);
            HRef<Closure> const closure = allocClosure(state, method, Fixnum((intptr_t)cloverCount));
            // TODO: DRY wrt. OP_CALL:
            // OPTIMIZE:
            {
                size_t const end = state->pc;
                size_t const start = end - cloverSetByteCount;
                for (size_t byteIdx = 0, cloverIdx = 0; byteIdx < cloverSetByteCount; ++byteIdx) {
                    uint8_t const byte = state->code[start + byteIdx];
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> (UINT8_WIDTH - 1 - bitIdx)) & 1) {
                            ORef* const cloverPtr =
                                (ORef*)closure.ptr()->clovers().data + cloverIdx++;
                            size_t const regIdx = UINT8_WIDTH * byteIdx + bitIdx;
                            *cloverPtr = state->regs[regIdx];
                        }
                    }
                }
            }

            state->regs[destReg] = closure.oref();
        }; continue;

        case OP_CLOVER: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const closureReg = state->code[state->pc++];
            uint8_t const cloverIdx = state->code[state->pc++];

            // OPTIMIZE: Separate OP_CONT_CLOVER:
            ORef const anyClosure = state->regs[closureReg];
            if (!isClosure(state, anyClosure)) {
                Continuation const* const cont =
                    HRef<Continuation>::fromUnchecked(anyClosure).ptr();
                state->regs[destReg] = cont->saves()[cloverIdx];
            } else {
                Closure const* const closure = HRef<Closure>::fromUnchecked(anyClosure).ptr();
                state->regs[destReg] = closure->clovers()[cloverIdx];
            }
        }; continue;

        case OP_CALL: {
            uint8_t const regCount  = state->code[state->pc++];
            uint8_t const cloverSetByteCount = state->code[state->pc++];
            size_t cloverCount = 0;
            // OPTIMIZE:
            for (size_t i = 0; i < cloverSetByteCount; ++i) {
                cloverCount += stdc_count_ones(state->code[state->pc++]);
            }

            HRef<Method> const callerMethod = HRef<Method>::fromUnchecked(state->method);
            HRef<Continuation> const cont = allocContinuation(
                state, callerMethod, Fixnum((intptr_t)state->pc), Fixnum((intptr_t)cloverCount)
            );
            // TODO: DRY wrt. OP_CLOSURE:
            // OPTIMIZE:
            {
                size_t const end = state->pc;
                size_t const start = end - cloverSetByteCount;
                for (size_t byteIdx = 0, cloverIdx = 0; byteIdx < cloverSetByteCount; ++byteIdx) {
                    uint8_t const byte = state->code[start + byteIdx];
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> (UINT8_WIDTH - 1 - bitIdx)) & 1) {
                            ORef* const cloverPtr =
                                (ORef*)cont.ptr()->saves().data + cloverIdx++;
                            size_t const regIdx = UINT8_WIDTH * byteIdx + bitIdx;
                            *cloverPtr = state->regs[regIdx];
                        }
                    }
                }
            }

            state->regs[retContReg] = cont.oref();

            state->entryRegc = regCount;
            goto apply;
        }; continue;

        case OP_TAILCALL: {
            uint8_t const regCount = state->code[state->pc++];

            state->entryRegc = regCount;
            goto apply;
        }; continue;
        }

        apply:
        bool trampoline = true;
        while (trampoline) {
            // Do not need return value here as a call is set up even in case of error:
            calleeClosure(state, state->regs[calleeReg]);
            Closure const* closure = HRef<Closure>::fromUnchecked(state->regs[calleeReg]).ptr();

            ORef method = closure->method;
            assert(isMethod(state, method));
            Method const* methodPtr = HRef<Method>::fromUnchecked(method).ptr();
            if (isHeaped(methodPtr->code)) {
                state->method = method;
                state->code = HRef<ByteArray>::fromUnchecked(methodPtr->code).ptr()->flexData();
                state->pc = 0;
                state->consts = HRef<ArrayMut>::fromUnchecked(methodPtr->consts).ptr()->flexData();

                ORef const maybeErr = checkDomain(state);
                if (isHeaped(maybeErr)) {
                    state->regs[calleeReg] = getErrorHandler(state);
                    state->regs[firstArgReg] = maybeErr;
                    state->entryRegc = firstArgReg + 1;
                    continue;
                }

                if (methodPtr->hasVarArg.val()) {
                    size_t const arity = methodPtr->domain().count;
                    size_t const minArity = arity - 1;
                    uint8_t const callArgc = state->entryRegc - firstArgReg;
                    size_t const varargCount = callArgc - minArity;

                    HRef<ArrayMut> const varargsRef =
                        createArrayMut(state, Fixnum((intptr_t)varargCount));
                    memcpy((void*)varargsRef.ptr()->flexData(),
                           state->regs + firstArgReg + minArity, varargCount * sizeof(ORef));

                    state->regs[firstArgReg + minArity] = varargsRef.oref();
                }

                trampoline = false;
            } else {
                applyPrimop:
                switch (methodPtr->nativeCode(state)) {
                case PrimopRes::CONTINUE: { // TODO: DRY wrt. OP_RET:
                    assert(eq(typeOf(state, state->regs[retContReg]).oref(),
                              state->types.continuation.oref()));
                    HRef<Continuation> const retRef =
                        HRef<Continuation>::fromUnchecked(state->regs[retContReg]);
                    Continuation const* const ret = retRef.ptr();
                    ORef const method = ret->method;
                    if (isHeaped(method)) {
                        assert(isMethod(state, method));
                        Method const* const methodPtr = HRef<Method>::fromUnchecked(method).ptr();
                        state->method = method;
                        state->code =
                            HRef<ByteArray>::fromUnchecked(methodPtr->code).ptr()->flexData();
                        state->pc = (size_t)ret->pc.val();
                        state->consts =
                            HRef<ArrayMut>::fromUnchecked(methodPtr->consts).ptr()->flexData();
                        trampoline = false;
                    } else { // Exit
                        return VMRes{.val = state->regs[retReg], .success = true};
                    }
                }; break;

                case PrimopRes::TAILCALL: break; // All is in place, just keep trampolining

                // TODO: DRY with loop head:
                case PrimopRes::TAILAPPLY: {
                    closure = HRef<Closure>::fromUnchecked(state->regs[calleeReg]).ptr();
                    method = closure->method;
                    assert(isMethod(state, method));
                    methodPtr = HRef<Method>::fromUnchecked(method).ptr();
                    if (isHeaped(methodPtr->code)) {
                        state->method = method;
                        state->code =
                            HRef<ByteArray>::fromUnchecked(methodPtr->code).ptr()->flexData();
                        state->pc = 0;
                        state->consts =
                            HRef<ArrayMut>::fromUnchecked(methodPtr->consts).ptr()->flexData();

                        state->checkDomain = true;

                        trampoline = false;
                    } else {
                        goto applyPrimop;
                    }
                }; break;

                case PrimopRes::ABORT: return VMRes{};
                }
            }
        }
    }
}

} // namespace
