#include <stdbit.h>
#include <string.h>

#include "vesihiisi.h"
#include "rt.hpp"
#include "bytecode.hpp"
#include "dispatch.hpp"
#include "primops.hpp"
#include "namespace.hpp"
#include "flyweights.hpp"

namespace {

#if !defined(VSHS_COMPUTED_GOTO)
#if defined(__GNUC__)
#define VSHS_COMPUTED_GOTO true
#else
#define VSHS_COMPUTED_GOTO false
#endif
#endif

typedef struct VMRes {
    ORef val;
    bool success;
} VMRes;

VMRes run(RT* state, HRef<Closure> self) {
    // TODO: Debug index & type checks & bytecode verifier

    {
        ORef const anyMethod = self->method;
        auto const method = HRef<Method>::fromUnchecked(anyMethod);
        assert(isHeaped(method->code));
        state->setMethod(method);
        state->pc = 0;
        state->regs[calleeReg] = self;
        state->regs[retContReg] = state->singletons.exit; // Return continuation
        state->entryRegc = 2;
    }

#if VSHS_COMPUTED_GOTO
    // The order is vital. Too bad we cannot use e.g. `[OP_MOVE] = &&L_OP_MOVE` like in C since '99:
    static constexpr void const* opcode_handlers[] = {
        &&L_OP_MOVE,
        &&L_OP_SWAP,
        &&L_OP_DEFINE,
        &&L_OP_GLOBAL_SET,
        &&L_OP_GLOBAL,
        &&L_OP_CONST,
        &&L_OP_SPECIALIZE,
        &&L_OP_KNOT,
        &&L_OP_KNOT_INIT,
        &&L_OP_KNOT_GET,
        &&L_OP_BRF,
        &&L_OP_BR,
        &&L_OP_RET,
        &&L_OP_CLOSURE,
        &&L_OP_CLOVER,
        // TODO: &&L_OP_OP_CONT_CLOVER / &&L_OP_OP_RESTORE
        &&L_OP_CALL,
        &&L_OP_TAILCALL
    };

#define VM_DISPATCH(opcode) goto* opcode_handlers[opcode];
#define VM_CASE(opcode) L_##opcode:
#define VM_CONTINUE VM_DISPATCH((Opcode)state->code[state->pc++])
#else
#define VM_DISPATCH(opcode) switch (opcode)
#define VM_CASE(opcode) case opcode:
#define VM_CONTINUE goto eval
#endif

    for (auto inlineCacheIdx = std::optional<uint8_t>{}; /*ever*/;) {
#if !VSHS_COMPUTED_GOTO
    eval:
#endif
        VM_DISPATCH((Opcode)state->code[state->pc++]) {
        VM_CASE(OP_MOVE) {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            state->regs[destReg] = state->regs[srcReg];
        }; VM_CONTINUE;

        VM_CASE(OP_SWAP) {
            uint8_t const reg1 = state->code[state->pc++];
            uint8_t const reg2 = state->code[state->pc++];

            ORef const tmp = state->regs[reg1];
            state->regs[reg1] = state->regs[reg2];
            state->regs[reg2] = tmp;
        }; VM_CONTINUE;

        VM_CASE(OP_DEFINE) {
            uint8_t const constIdx = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            ORef c = state->consts[constIdx].get();
            if (isa<Symbol>(*state, c)) { // Link:
                c = getVar(state, state->ns, HRef<Symbol>::fromUnchecked(c));
                auto const cG = state->pushRoot(&c);

                state->consts[constIdx].set(*state, c);
            }
            HRef<Var> const var = HRef<Var>::fromUnchecked(c);

            var->val().set(*state, state->regs[srcReg]);
        }; VM_CONTINUE;

        VM_CASE(OP_GLOBAL_SET) {
            uint8_t const constIdx = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            ORef c = state->consts[constIdx].get();
            if (isa<Symbol>(*state, c)) { // Link:
                auto const name = HRef<Symbol>::fromUnchecked(c);
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
                    state->regs[firstArgReg] = createUnboundError(state, name);
                    state->entryRegc = firstArgReg + 1;
                    inlineCacheIdx = std::nullopt;
                    goto apply;
                }
                c = findRes.var;
                auto const cG = state->pushRoot(&c);

                state->consts[constIdx].set(*state, c);
            }
            auto const var = HRef<Var>::fromUnchecked(c);

            var->val().set(*state, state->regs[srcReg]);
        }; VM_CONTINUE;

        VM_CASE(OP_GLOBAL) {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            ORef c = state->consts[constIdx].get();
            if (isa<Symbol>(*state, c)) { // Link:
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
                    state->regs[firstArgReg] = createUnboundError(state, name);
                    state->entryRegc = firstArgReg + 1;
                    inlineCacheIdx = std::nullopt;
                    goto apply;
                }
                c = findRes.var;
                auto const cG = state->pushRoot(&c);

                state->consts[constIdx].set(*state, c);
            }
            auto const var = HRef<Var>::fromUnchecked(c);

            ORef const v = var->val().get();
            if (eq(v, state->singletons.unbound)) {
                assert(false); // FIXME: use of unbound var
            }
            state->regs[destReg] = v;
        }; VM_CONTINUE;

        VM_CASE(OP_CONST) {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            state->regs[destReg] = state->consts[constIdx].get();
        }; VM_CONTINUE;

        VM_CASE(OP_SPECIALIZE) {
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
                            if (!isa<Type>(*state, maybeType)) {
                                return VMRes{}; // TODO: Signal type error properly
                            }
                            const_cast<ORef*>(types->items().data())[typeIdx++] = maybeType;
                        }
                    }
                }
            }
            assert(isa<Method>(*state, state->consts[constIdx].get()));
            HRef<Method> const generic = HRef<Method>::fromUnchecked(state->consts[constIdx].get());
            HRef<Method> const method = specialize(state, generic, types);

            state->regs[destReg] = method;
        }; VM_CONTINUE;

        VM_CASE(OP_KNOT) {
            uint8_t const destReg = state->code[state->pc++];

            state->regs[destReg] = allocKnot(state);
        }; VM_CONTINUE;

        VM_CASE(OP_KNOT_INIT) {
            uint8_t const knotReg = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            assert(isa(state, state->types.knot, state->regs[knotReg]));
            auto const knot = HRef<Knot>::fromUnchecked(state->regs[knotReg]);
            knot->val().set(*state,  state->regs[srcReg]);
        }; VM_CONTINUE;

        VM_CASE(OP_KNOT_GET) {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const knotReg = state->code[state->pc++];

            assert(isa(state, state->types.knot, state->regs[knotReg]));
            auto const knot = HRef<Knot>::fromUnchecked(state->regs[knotReg]);
            state->regs[destReg] = knot->val().get();
        }; VM_CONTINUE;

        VM_CASE(OP_BR) {
            uint16_t displacement = state->code[state->pc++];
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | state->code[state->pc++];

            state->pc += displacement;
        }; VM_CONTINUE;

        VM_CASE(OP_BRF) {
            uint8_t const condReg = state->code[state->pc++];
            uint16_t displacement = state->code[state->pc++];
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | state->code[state->pc++];

            if (eq(state->regs[condReg], False)) {
                state->pc += displacement;
            }
        }; VM_CONTINUE;

        VM_CASE(OP_RET) goto kontinue;

        VM_CASE(OP_CLOSURE) {
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
                            auto const cloverPtr = // `const_cast` for init:
                                const_cast<ORef*>(closure->clovers().data()) + cloverIdx++;
                            size_t const regIdx = UINT8_WIDTH * byteIdx + bitIdx;
                            *cloverPtr = state->regs[regIdx];
                        }
                    }
                }
            }

            state->regs[destReg] = closure;
        }; VM_CONTINUE;

        VM_CASE(OP_CLOVER) {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const closureReg = state->code[state->pc++];
            uint8_t const cloverIdx = state->code[state->pc++];

            // OPTIMIZE: Separate OP_CONT_CLOVER:
            ORef const anyClosure = state->regs[closureReg];
            if (!isa<Closure>(*state, anyClosure)) {
                auto const cont = HRef<Continuation>::fromUnchecked(anyClosure);
                state->regs[destReg] = cont->saves()[cloverIdx];
            } else {
                auto const closure = HRef<Closure>::fromUnchecked(anyClosure);
                state->regs[destReg] = closure->clovers()[cloverIdx];
            }
        }; VM_CONTINUE;

        VM_CASE(OP_CALL) {
            inlineCacheIdx = std::optional{state->code[state->pc++]};
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
                            auto const cloverPtr = // `const_cast` for init:
                                const_cast<ORef*>(cont->saves().data()) + cloverIdx++;
                            size_t const regIdx = UINT8_WIDTH * byteIdx + bitIdx;
                            *cloverPtr = state->regs[regIdx];
                        }
                    }
                }
            }

            state->regs[retContReg] = cont;

            state->entryRegc = regCount;
            goto apply;
        }; VM_CONTINUE;

        VM_CASE(OP_TAILCALL) {
            inlineCacheIdx = std::optional{state->code[state->pc++]};
            uint8_t const regCount = state->code[state->pc++];

            state->entryRegc = regCount;
            goto apply;
        }; VM_CONTINUE;
        }

    apply: for (;/*ever*/;) {
        ORef const originalCallee = state->regs[calleeReg];
        // Do not need return value here as a call is set up even in case of error:
        calleeClosure(state, originalCallee, inlineCacheIdx);

        auto method = [&](){
            assert(isa<Closure>(*state, state->regs[calleeReg]));
            auto closure = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
            assert(isa<Method>(*state, closure->method));
            return HRef<Method>::fromUnchecked(closure->method);
        }();
        if (isHeaped(method->code)) { // Bytecode method:
            // Check domain:
            switch (checkDomain(state)) {
            case DomainCheckRes::OK: break;

            case DomainCheckRes::MISSPECULATION: {
                // `originalCallee` is valid since speculation does not allocate or do write
                // barriers:
                state->regs[calleeReg] = originalCallee;
                // These writes do invalidate `originalCallee`:
                state->consts[*inlineCacheIdx].set(*state, Default);
                state->consts[*inlineCacheIdx + 1].set(*state, Default);
                inlineCacheIdx = std::nullopt;
            }; continue;

            case DomainCheckRes::ERROR: {
                inlineCacheIdx = std::nullopt;
            }; continue;
            }

            if (method->hasVarArg.val()) { // Reify varargs:
                size_t const arity = method->domain().size();
                size_t const minArity = arity - 1;
                uint8_t const callArgc = state->entryRegc - firstArgReg;
                size_t const varargCount = callArgc - minArity;

                HRef<ArrayMut> const varargsRef =
                    createArrayMut(state, Fixnum((intptr_t)varargCount));
                memcpy((void*)varargsRef->flexData(),
                       state->regs + firstArgReg + minArity, varargCount * sizeof(ORef));

                state->regs[firstArgReg + minArity] = varargsRef;
            }

            // Jump to beginning:
            state->setMethod(method);
            state->pc = 0;

            VM_CONTINUE;
        } else {
            applyPrimop:
            switch (method->nativeCode(state)) {
            case PrimopRes::CONTINUE: // Returned:
                goto kontinue;

            case PrimopRes::TAILCALL: { // Set up another call in its place:
                inlineCacheIdx = std::nullopt;
            }; break; // All is in place, just keep trampolining

            // TODO: DRY with loop head:
            case PrimopRes::TAILAPPLY: {
                method = [&](){
                    assert(isa<Closure>(*state, state->regs[calleeReg]));
                    auto closure = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
                    assert(isa<Method>(*state, closure->method));
                    return HRef<Method>::fromUnchecked(closure->method);
                }();
                if (isHeaped(method->code)) {
                    state->setMethod(method);
                    state->pc = 0;

                    state->domainChecking = RT::DomainChecking::CHECK;

                    VM_CONTINUE;
                } else {
                    goto applyPrimop;
                }
            }; break;

            case PrimopRes::MISSPECULATION: {
                // `originalCallee` is valid since speculation does not allocate or do write
                // barriers:
                state->regs[calleeReg] = originalCallee;
                // These writes do invalidate `originalCallee`:
                state->consts[*inlineCacheIdx].set(*state, Default);
                state->consts[*inlineCacheIdx + 1].set(*state, Default);
                inlineCacheIdx = std::nullopt;
            }; break;

            case PrimopRes::ERROR: { // Set up an error call in its place:
                inlineCacheIdx = std::nullopt;
            }; break; // All is in place, just keep trampolining

            case PrimopRes::ABORT: return VMRes{};
            }
        }
    }

    kontinue: {
        assert(isa<Continuation>(*state, state->regs[retContReg]));
        auto const ret = HRef<Continuation>::fromUnchecked(state->regs[retContReg]);
        ORef const anyMethod = ret->method;
        if (isHeaped(anyMethod)) { // Return to bytecode method:
            assert(isa<Method>(*state, anyMethod));
            state->setMethod(HRef<Method>::fromUnchecked(anyMethod));
            state->pc = (size_t)ret->pc.val();
        } else { // Exit:
            return VMRes{.val = state->regs[retReg], .success = true};
        }

        VM_CONTINUE;
    }
    }
}

} // namespace
