#include <stdbit.h>
#include <string.h>

#include "vesihiisi.h"
#include "state.h"
#include "bytecode.h"
#include "dispatch.h"
#include "primops.h"
#include "namespace.h"
#include "flyweights.h"

static VMRes run(State* state, ClosureRef selfRef) {
    // TODO: Debug index & type checks & bytecode verifier

    {
        Closure const* const self = closureToPtr(selfRef);
        ORef const method = self->method;
        Method const* const methodPtr = methodToPtr(uncheckedORefToMethod(method));
        assert(isHeaped(methodPtr->code));
        state->method = method;
        state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
        state->pc = 0;
        state->consts = arrayMutToPtr(uncheckedORefToArrayMut(methodPtr->consts));
        state->regs[calleeReg] = toORef(selfRef);
        state->regs[retContReg] = toORef(state->exit); // Return continuation
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

            VarRef var;
            ORef const c = state->consts[constIdx];
            if (!isSymbol(state, c)) {
                assert(false); // TODO: Lazily linked Var
            } else {
                SymbolRef const name = uncheckedORefToSymbol(c);
                var = getVar(state, state->ns, name);
            }

            varToPtr(var)->val = state->regs[srcReg];
        }; continue;

        case OP_GLOBAL: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            VarRef var;
            ORef const c = state->consts[constIdx];
            if (!isSymbol(state, c)) {
                assert(false); // TODO: Lazily linked Var
                var = tagVar((Var*)nullptr); // HACK: Shuts up warning about maybe uninit `var`.
            } else {
                SymbolRef const name = uncheckedORefToSymbol(c);
                FindVarRes const findRes = findVar(state->ns, name);
                if (findRes.type != NS_FOUND_VAR) {
                    // FIXME: Signal that this is a "fatal" (i.e. noncontinuable) error as
                    // constructing a working continuation at an arbitrary instruction like this
                    // would take a lot of effort while actually using that to recover from this
                    // would be a terrible idea. Currently `retContReg` probably holds the return
                    // continuation of the current function; to support stack traces we probably
                    // have to ensure that it does. But that continuation is definitely not a
                    // correct current continuation.
                    state->regs[calleeReg] = getErrorHandler(state);
                    state->regs[firstArgReg] = toORef(createUnboundError(state, name));
                    state->entryRegc = firstArgReg + 1;
                    goto apply;
                }

                var = findRes.var;
            }

            ORef const v = varToPtr(var)->val;
            if (eq(v, toORef(state->unbound))) {
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
            ArrayMutRef const types = createArrayMut(state, tagInt((intptr_t)typeCount));
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
                                return (VMRes){}; // TODO: Signal type error properly
                            }
                            arrayMutToPtr(types)[typeIdx++] = maybeType;
                        }
                    }
                }
            }
            assert(isMethod(state, state->consts[constIdx]));
            MethodRef const generic = uncheckedORefToMethod(state->consts[constIdx]);
            MethodRef const method = specialize(state, generic, types);

            state->regs[destReg] = toORef(method);
        }; continue;

        case OP_KNOT: {
            uint8_t const destReg = state->code[state->pc++];

            state->regs[destReg] = toORef(allocKnot(state));
        }; continue;

        case OP_KNOT_INIT: {
            uint8_t const knotReg = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            assert(isa(state, state->knotType, state->regs[knotReg]));
            Knot* const knot = toPtr(uncheckedORefToKnot(state->regs[knotReg]));
            knot->val = state->regs[srcReg];
        }; continue;

        case OP_KNOT_GET: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const knotReg = state->code[state->pc++];

            assert(isa(state, state->knotType, state->regs[knotReg]));
            Knot const* const knot = toPtr(uncheckedORefToKnot(state->regs[knotReg]));
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

            if (eq(state->regs[condReg], toORef(False))) {
                state->pc += displacement;
            }
        }; continue;

        case OP_RET: {
            assert(eq(toORef(typeOf(state, state->regs[retContReg])),
                      toORef(state->continuationType)));
            ContinuationRef const retRef = uncheckedORefToContinuation(state->regs[retContReg]);
            Continuation const* const ret = continuationToPtr(retRef);
            ORef const method = ret->method;
            if (isHeaped(method)) {
                assert(isMethod(state, method));
                Method const* const methodPtr = methodToPtr(uncheckedORefToMethod(method));
                state->method = method;
                state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                state->pc = (size_t)fixnumToInt(ret->pc);
                state->consts = arrayMutToPtr(uncheckedORefToArrayMut(methodPtr->consts));
            } else { // Exit
                return (VMRes){.success = true, .val = state->regs[retReg]};
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

            MethodRef const method = uncheckedORefToMethod(state->regs[methodReg]);
            ClosureRef const closure = allocClosure(state, method, tagInt((intptr_t)cloverCount));
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
                                (ORef*)closureToPtr(closure)->clovers + cloverIdx++;
                            size_t const regIdx = UINT8_WIDTH * byteIdx + bitIdx;
                            *cloverPtr = state->regs[regIdx];
                        }
                    }
                }
            }

            state->regs[destReg] = toORef(closure);
        }; continue;

        case OP_CLOVER: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const closureReg = state->code[state->pc++];
            uint8_t const cloverIdx = state->code[state->pc++];

            // OPTIMIZE: Separate OP_CONT_CLOVER:
            ORef const anyClosure = state->regs[closureReg];
            if (!isClosure(state, anyClosure)) {
                Continuation const* const cont =
                    continuationToPtr(uncheckedORefToContinuation(anyClosure));
                state->regs[destReg] = cont->saves[cloverIdx];
            } else {
                Closure const* const closure = closureToPtr(uncheckedORefToClosure(anyClosure));
                state->regs[destReg] = closure->clovers[cloverIdx];
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

            MethodRef const callerMethod = uncheckedORefToMethod(state->method);
            ContinuationRef const cont = allocContinuation(
                state, callerMethod, tagInt((intptr_t)state->pc), tagInt((intptr_t)cloverCount)
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
                                (ORef*)continuationToPtr(cont)->saves + cloverIdx++;
                            size_t const regIdx = UINT8_WIDTH * byteIdx + bitIdx;
                            *cloverPtr = state->regs[regIdx];
                        }
                    }
                }
            }

            state->regs[retContReg] = toORef(cont);

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
            Closure const* closure = closureToPtr(uncheckedORefToClosure(state->regs[calleeReg]));

            ORef method = closure->method;
            assert(isMethod(state, method));
            Method const* methodPtr = methodToPtr(uncheckedORefToMethod(method));
            if (isHeaped(methodPtr->code)) {
                state->method = method;
                state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                state->pc = 0;
                state->consts = arrayMutToPtr(uncheckedORefToArrayMut(methodPtr->consts));

                ORef const maybeErr = checkDomain(state);
                if (isHeaped(maybeErr)) {
                    state->regs[calleeReg] = getErrorHandler(state);
                    state->regs[firstArgReg] = maybeErr;
                    state->entryRegc = firstArgReg + 1;
                    continue;
                }

                if (eq(toORef(methodPtr->hasVarArg), toORef(True))) {
                    size_t const arity = (uintptr_t)fixnumToInt(uncheckedFlexCount(method));
                    size_t const minArity = arity - 1;
                    uint8_t const callArgc = state->entryRegc - firstArgReg;
                    size_t const varargCount = callArgc - minArity;

                    ArrayMutRef const varargsRef =
                        createArrayMut(state, tagInt((intptr_t)varargCount));
                    memcpy(arrayMutToPtr(varargsRef), state->regs + firstArgReg + minArity,
                           varargCount * sizeof(ORef));

                    state->regs[firstArgReg + minArity] = toORef(varargsRef);
                }

                trampoline = false;
            } else {
                applyPrimop:
                switch (methodPtr->nativeCode(state)) {
                case PRIMOP_RES_CONTINUE: { // TODO: DRY wrt. OP_RET:
                    assert(eq(toORef(typeOf(state, state->regs[retContReg])),
                            toORef(state->continuationType)));
                    ContinuationRef const retRef =
                        uncheckedORefToContinuation(state->regs[retContReg]);
                    Continuation const* const ret = continuationToPtr(retRef);
                    ORef const method = ret->method;
                    if (isHeaped(method)) {
                        assert(isMethod(state, method));
                        Method const* const methodPtr =
                            methodToPtr(uncheckedORefToMethod(method));
                        state->method = method;
                        state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                        state->pc = (size_t)fixnumToInt(ret->pc);
                        state->consts = arrayMutToPtr(uncheckedORefToArrayMut(methodPtr->consts));
                        trampoline = false;
                    } else { // Exit
                        return (VMRes){.val = state->regs[retReg], .success = true};
                    }
                }; break;

                case PRIMOP_RES_TAILCALL: break; // All is in place, just keep trampolining

                // TODO: DRY with loop head:
                case PRIMOP_RES_TAILAPPLY: {
                    closure = closureToPtr(uncheckedORefToClosure(state->regs[calleeReg]));
                    method = closure->method;
                    assert(isMethod(state, method));
                    methodPtr = methodToPtr(uncheckedORefToMethod(method));
                    if (isHeaped(methodPtr->code)) {
                        state->method = method;
                        state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                        state->pc = 0;
                        state->consts = arrayMutToPtr(uncheckedORefToArrayMut(methodPtr->consts));

                        state->checkDomain = true;

                        trampoline = false;
                    } else {
                        goto applyPrimop;
                    }
                }; break;

                case PRIMOP_RES_ABORT: return (VMRes){};
                }
            }
        }
    }
}
