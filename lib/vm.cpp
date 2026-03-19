#include "vm.hpp"

#include <stdbit.h>
#include <string.h>

#include "vesihiisi.h"
#include "rt.hpp"
#include "bytecode.hpp"
#include "dispatch.hpp"
#include "primops.hpp"
#include "namespace.hpp"
#include "flyweights.hpp"
#include "jit.hpp"

namespace {

#if !defined(VSHS_COMPUTED_GOTO)
#if defined(__GNUC__)
#define VSHS_COMPUTED_GOTO true
#else
#define VSHS_COMPUTED_GOTO false
#endif
#endif

VMRes run(RT* rt, HRef<Closure> self) {
    // TODO: Debug index & type checks & bytecode verifier

    {
        ORef const anyMethod = self->method;
        auto const method = HRef<Method>::fromUnchecked(anyMethod);
        assert(isHeaped(method->code));
        rt->setMethod(method);
        rt->pc = Method::entryPc();
        rt->regs[calleeReg] = self;
        rt->regs[retContReg] = rt->singletons.exit; // Return continuation
        rt->entryRegc = 2;
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
        &&L_OP_UNSPILL,
        &&L_OP_CALL,
        &&L_OP_TAILCALL,
        &&L_OP_FFICALL
    };

#define VM_DISPATCH(opcode) goto* opcode_handlers[opcode];
#define VM_CASE(opcode) L_##opcode:
#define VM_CONTINUE VM_DISPATCH(static_cast<Opcode>(rt->code[rt->pc++]))
#else
#define VM_DISPATCH(opcode) switch (opcode)
#define VM_CASE(opcode) case opcode:
#define VM_CONTINUE goto eval
#endif

    for (auto inlineCacheIdx = std::optional<uint8_t>{}; /*ever*/;) {
#if !VSHS_COMPUTED_GOTO
    eval:
#endif
        VM_DISPATCH(static_cast<Opcode>(rt->code[rt->pc++])) {
        VM_CASE(OP_MOVE) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const srcReg = rt->code[rt->pc++];

            rt->regs[destReg] = rt->regs[srcReg];
        }; VM_CONTINUE;

        VM_CASE(OP_SWAP) {
            uint8_t const reg1 = rt->code[rt->pc++];
            uint8_t const reg2 = rt->code[rt->pc++];

            ORef const tmp = rt->regs[reg1];
            rt->regs[reg1] = rt->regs[reg2];
            rt->regs[reg2] = tmp;
        }; VM_CONTINUE;

        VM_CASE(OP_DEFINE) {
            uint8_t const constIdx = rt->code[rt->pc++];
            uint8_t const srcReg = rt->code[rt->pc++];

            ORef c = rt->consts[constIdx].get();
            if (isa<Symbol>(*rt, c)) { // Link:
                c = getVar(rt, rt->ns, HRef<Symbol>::fromUnchecked(c));
                auto const cG = rt->pushRoot(&c);

                rt->consts[constIdx].set(*rt, c);
            }
            HRef<Var> const var = HRef<Var>::fromUnchecked(c);

            var->val().set(*rt, rt->regs[srcReg]);
        }; VM_CONTINUE;

        VM_CASE(OP_GLOBAL_SET) {
            uint8_t const constIdx = rt->code[rt->pc++];
            uint8_t const srcReg = rt->code[rt->pc++];

            ORef c = rt->consts[constIdx].get();
            if (isa<Symbol>(*rt, c)) { // Link:
                auto const name = HRef<Symbol>::fromUnchecked(c);
                FindVarRes const findRes = findVar(rt->ns, name);
                if (findRes.type != FindVarRes::NS_FOUND_VAR) {
                    // FIXME: Signal that this is a "fatal" (i.e. noncontinuable) error as
                    // constructing a working continuation at an arbitrary instruction like this
                    // would take a lot of effort while actually using that to recover from this
                    // would be a terrible idea. Currently `retContReg` probably holds the return
                    // continuation of the current function; to support stack traces we probably
                    // have to ensure that it does. But that continuation is definitely not a
                    // correct current continuation.
                    rt->regs[calleeReg] = getErrorHandler(rt);
                    rt->regs[firstArgReg] = createUnboundError(rt, name);
                    rt->entryRegc = firstArgReg + 1;
                    inlineCacheIdx = std::nullopt;
                    goto apply;
                }
                c = findRes.var;
                auto const cG = rt->pushRoot(&c);

                rt->consts[constIdx].set(*rt, c);
            }
            auto const var = HRef<Var>::fromUnchecked(c);

            var->val().set(*rt, rt->regs[srcReg]);
        }; VM_CONTINUE;

        VM_CASE(OP_GLOBAL) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const constIdx = rt->code[rt->pc++];

            ORef c = rt->consts[constIdx].get();
            if (isa<Symbol>(*rt, c)) { // Link:
                HRef<Symbol> const name = HRef<Symbol>::fromUnchecked(c);
                FindVarRes const findRes = findVar(rt->ns, name);
                if (findRes.type != FindVarRes::NS_FOUND_VAR) {
                    // FIXME: Signal that this is a "fatal" (i.e. noncontinuable) error as
                    // constructing a working continuation at an arbitrary instruction like this
                    // would take a lot of effort while actually using that to recover from this
                    // would be a terrible idea. Currently `retContReg` probably holds the return
                    // continuation of the current function; to support stack traces we probably
                    // have to ensure that it does. But that continuation is definitely not a
                    // correct current continuation.
                    rt->regs[calleeReg] = getErrorHandler(rt);
                    rt->regs[firstArgReg] = createUnboundError(rt, name);
                    rt->entryRegc = firstArgReg + 1;
                    inlineCacheIdx = std::nullopt;
                    goto apply;
                }
                c = findRes.var;
                auto const cG = rt->pushRoot(&c);

                rt->consts[constIdx].set(*rt, c);
            }
            auto const var = HRef<Var>::fromUnchecked(c);

            ORef const v = var->val().get();
            if (eq(v, rt->singletons.unbound)) {
                assert(false); // FIXME: use of unbound var
            }
            rt->regs[destReg] = v;
        }; VM_CONTINUE;

        VM_CASE(OP_CONST) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const constIdx = rt->code[rt->pc++];

            rt->regs[destReg] = rt->consts[constIdx].get();
        }; VM_CONTINUE;

        VM_CASE(OP_SPECIALIZE) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const constIdx = rt->code[rt->pc++];
            uint8_t const typeSetByteCount = rt->code[rt->pc++];
            size_t const typesStartIdx = rt->pc;
            size_t typeCount = 0;
            // OPTIMIZE:
            for (uint8_t const byte : std::span{rt->code + typesStartIdx, typeSetByteCount}) {
                typeCount += stdc_count_ones(byte);
            }
            rt->pc += typeSetByteCount;

            // OPTIMIZE: Allocate types to contiguous registers instead of allocating temporary
            // array:
            HRef<Array> const types = createArray(rt, Fixnum{intptr_t(typeCount)});
            {
                auto typeSlot = const_cast<ORef*>(types->items().data());
                size_t regIdx = 0;
                for (uint8_t const byte : std::span{rt->code + typesStartIdx, typeSetByteCount}) {
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> bitIdx) & 1) {
                            ORef const maybeType = rt->regs[regIdx];
                            if (!isa<Type>(*rt, maybeType)) {
                                return VMRes{}; // TODO: Signal type error properly
                            }
                            *typeSlot++ = maybeType;
                        }

                        ++regIdx;
                    }
                }
            }
            assert(isa<Method>(*rt, rt->consts[constIdx].get()));
            HRef<Method> const generic = HRef<Method>::fromUnchecked(rt->consts[constIdx].get());
            HRef<Method> const method = specialize(rt, generic, types);

            rt->regs[destReg] = method;
        }; VM_CONTINUE;

        VM_CASE(OP_KNOT) {
            uint8_t const destReg = rt->code[rt->pc++];

            rt->regs[destReg] = allocKnot(rt);
        }; VM_CONTINUE;

        VM_CASE(OP_KNOT_INIT) {
            uint8_t const knotReg = rt->code[rt->pc++];
            uint8_t const srcReg = rt->code[rt->pc++];

            assert(isa(rt, rt->types.knot, rt->regs[knotReg]));
            auto const knot = HRef<Knot>::fromUnchecked(rt->regs[knotReg]);
            knot->val().set(*rt, rt->regs[srcReg]);
        }; VM_CONTINUE;

        VM_CASE(OP_KNOT_GET) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const knotReg = rt->code[rt->pc++];

            assert(isa(rt, rt->types.knot, rt->regs[knotReg]));
            auto const knot = HRef<Knot>::fromUnchecked(rt->regs[knotReg]);
            rt->regs[destReg] = knot->val().get();
        }; VM_CONTINUE;

        VM_CASE(OP_BR) {
            uint16_t displacement = rt->code[rt->pc++];
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | rt->code[rt->pc++];

            rt->pc += displacement;
        }; VM_CONTINUE;

        VM_CASE(OP_BRF) {
            uint8_t const condReg = rt->code[rt->pc++];
            uint16_t displacement = rt->code[rt->pc++];
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | rt->code[rt->pc++];

            if (eq(rt->regs[condReg], False)) {
                rt->pc += displacement;
            }
        }; VM_CONTINUE;

        VM_CASE(OP_RET) goto kontinue;

        VM_CASE(OP_CLOSURE) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const methodReg = rt->code[rt->pc++];
            uint8_t const closesByteCount = rt->code[rt->pc++];
            size_t const closesStartIdx = rt->pc;
            size_t cloverCount = 0;
            // OPTIMIZE:
            for (uint8_t const byte : std::span{rt->code + closesStartIdx, closesByteCount}) {
                cloverCount += stdc_count_ones(byte);
            }
            rt->pc += closesByteCount;

            HRef<Method> const method = HRef<Method>::fromUnchecked(rt->regs[methodReg]);
            Closure* const closure = allocClosure(rt, method, Fixnum{int64_t(cloverCount)});
            // TODO: DRY wrt. OP_CALL:
            {
                ORef* clover = const_cast<ORef*>(closure->clovers().data());
                size_t regIdx = 0;
                for (uint8_t const byte : std::span{rt->code + closesStartIdx, closesByteCount}) {
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> bitIdx) & 1) {
                            *clover++ = rt->regs[regIdx];
                        }

                        ++regIdx;
                    }
                }
            }

            rt->regs[destReg] = HRef{closure};
        }; VM_CONTINUE;

        VM_CASE(OP_CLOVER) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const closureReg = rt->code[rt->pc++];
            uint8_t const cloverIdx = rt->code[rt->pc++];

            assert(isa<Closure>(*rt, rt->regs[closureReg]));
            auto const closure = HRef<Closure>::fromUnchecked(rt->regs[closureReg]);
            rt->regs[destReg] = closure->clovers()[cloverIdx];
        }; VM_CONTINUE;

        VM_CASE(OP_UNSPILL) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const contReg = rt->code[rt->pc++];
            uint8_t const cloverIdx = rt->code[rt->pc++];

            assert(isa<Continuation>(*rt, rt->regs[contReg]));
            auto const cont = HRef<Continuation>::fromUnchecked(rt->regs[contReg]);
            rt->regs[destReg] = cont->saves()[cloverIdx];
        }; VM_CONTINUE;

        VM_CASE(OP_CALL) {
            inlineCacheIdx = std::optional{rt->code[rt->pc++]};
            uint8_t const regCount  = rt->code[rt->pc++];
            uint8_t const saveSetByteCount = rt->code[rt->pc++];
            size_t const savesStartIdx = rt->pc;
            size_t saveCount = 0;
            // OPTIMIZE:
            for (uint8_t const byte : std::span{rt->code + savesStartIdx, saveSetByteCount}) {
                saveCount += stdc_count_ones(byte);
            }
            rt->pc += saveSetByteCount;

            HRef<Method> const callerMethod = HRef<Method>::fromUnchecked(rt->method);
            Continuation* const cont = allocContinuation(
                rt, callerMethod, Fixnum{int64_t(rt->pc)}, Fixnum{int64_t(saveCount)}
            );
            // TODO: DRY wrt. OP_CLOSURE:
            {
                ORef* spillSlot = const_cast<ORef*>(cont->saves().data());
                size_t regIdx = 0;
                for (uint8_t const byte : std::span{rt->code + savesStartIdx, saveSetByteCount}) {
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> bitIdx) & 1) {
                            *spillSlot++ = rt->regs[regIdx];
                        }

                        ++regIdx;
                    }
                }
            }

            rt->regs[retContReg] = HRef{cont};

            rt->entryRegc = regCount;
            goto apply;
        }; VM_CONTINUE;

        VM_CASE(OP_TAILCALL) {
            inlineCacheIdx = std::optional{rt->code[rt->pc++]};
            uint8_t const regCount = rt->code[rt->pc++];

            rt->entryRegc = regCount;
            goto apply;
        }; VM_CONTINUE;

        VM_CASE(OP_FFICALL) {
            uint8_t const destReg = rt->code[rt->pc++];
            uint8_t const codomainReg = rt->code[rt->pc++];
            uint8_t const argc = rt->code[rt->pc++];
            uint8_t const unboxingsByteCount = rt->code[rt->pc++];
            assert(1 + argc <= unboxingsByteCount * UINT8_WIDTH);
            uint8_t const* unboxings = &rt->code[rt->pc];
            rt->pc += unboxingsByteCount;

            ORef const* codomainPtr = &rt->regs[codomainReg];
            ORef const anyCodomain = *codomainPtr;
            if (!isa<Type>(*rt, anyCodomain)) { // TODO: DRY type checks like this
                rt->regs[calleeReg] = getErrorHandler(rt);
                rt->regs[firstArgReg] = createTypeError(rt, Type::reify(*rt), anyCodomain);
                rt->entryRegc = firstArgReg + 1;
                inlineCacheIdx = std::nullopt;
                goto apply;
            }
            auto const codomain = HRef<Type>::fromUnchecked(anyCodomain);
            bool const fRet = eq(codomain, rt->types.flonum);
            if (codomain->isFlex.val()) { PANIC("Flex FFI return type"); } // TODO: Proper error
            if (size_t(codomain->minSize.val()) > sizeof(uint64_t)) {
                PANIC("FFI return type size %ld > word size %lu\n",
                      codomain->minSize.val(), sizeof(uint64_t));
            }
            ORef const* fPtr = codomainPtr + 1;
            ORef const anyF = *fPtr;
            if (!isa<Pointer>(*rt, anyF)) { // TODO: DRY type checks like this
                rt->regs[calleeReg] = getErrorHandler(rt);
                rt->regs[firstArgReg] = createTypeError(rt, Pointer::reify(*rt), anyF);
                rt->entryRegc = firstArgReg + 1;
                inlineCacheIdx = std::nullopt;
                goto apply;
            }
            auto const f = HRef<Pointer>::fromUnchecked(anyF);
            ORef const* args = fPtr + 1;

            uint64_t const rawRes = callForeign(f->val, fRet, unboxings, args, argc);

            auto const res = [&]() -> ORef {
                if (fRet) { return Flonum{std::bit_cast<double>(rawRes)}; }

                auto const boxRet = bool(unboxings[0] & 0b1);
                if(!boxRet) { return tag(*rt, codomain, rawRes); }

                Object* const obj = rt->alloc(codomain);
                *std::bit_cast<uint64_t*>(obj) = rawRes;
                return HRef{obj};
            }();

            rt->regs[destReg] = res;
        }; VM_CONTINUE;
        }

    apply: for (;/*ever*/;) {
        ORef const originalCallee = rt->regs[calleeReg];
        // Do not need return value here as a call is set up even in case of error:
        calleeClosure(rt, originalCallee, inlineCacheIdx);

        applyClosure:
        auto method = [&](){
            assert(isa<Closure>(*rt, rt->regs[calleeReg]));
            auto closure = HRef<Closure>::fromUnchecked(rt->regs[calleeReg]);
            assert(isa<Method>(*rt, closure->method));
            return HRef<Method>::fromUnchecked(closure->method);
        }();
        applyMethod:
        switch (method->nativeCode()(rt)) {
        case PrimopRes::INTERPRET: VM_CONTINUE;

        case PrimopRes::CALL_BYTECODE: { // Bytecode method:
            int64_t const callCount = method->callCount.val() + 1;
            method->callCount = Fixnum{callCount};
            if (uint64_t(callCount) == rt->jitThreshold) {
                jitCompile(*rt, *method);
                goto applyMethod;
            }

            // Check domain:
            switch (checkDomain(rt)) {
            case DomainCheckRes::OK: break;

            case DomainCheckRes::MISSPECULATION: {
                // `originalCallee` is valid since speculation does not allocate or do write
                // barriers:
                rt->regs[calleeReg] = originalCallee;
                // These writes do invalidate `originalCallee`:
                rt->consts[*inlineCacheIdx].set(*rt, Default);
                rt->consts[*inlineCacheIdx + 1].set(*rt, Default);
                inlineCacheIdx = std::nullopt;
            }; continue;

            case DomainCheckRes::ERROR: {
                inlineCacheIdx = std::nullopt;
            }; continue;
            }

            if (method->hasVarArg.val()) { // Reify varargs:
                size_t const arity = method->domain().size();
                size_t const minArity = arity - 1;
                uint8_t const callArgc = rt->entryRegc - firstArgReg;
                size_t const varargCount = callArgc - minArity;

                HRef<ArrayMut> const varargsRef = createArrayMut(rt, Fixnum{intptr_t(varargCount)});
                ORef const* const begin = rt->regs + firstArgReg + minArity;
                ORef const* const end = begin + varargCount;
                std::copy(begin, end, const_cast<ORef*>(varargsRef->flexData()));

                rt->regs[firstArgReg + minArity] = varargsRef;
            }

            // Jump to beginning:
            rt->setMethod(method);
            rt->pc = Method::entryPc();
        }; VM_CONTINUE;

        case PrimopRes::CONTINUE: // Returned:
            goto kontinue;

        case PrimopRes::TAILCALL: { // Set up another call in its place:
            inlineCacheIdx = std::nullopt;
        }; break; // All is in place, just keep trampolining

        case PrimopRes::TAILAPPLY: goto applyClosure;

        case PrimopRes::MISSPECULATION: {
            // `originalCallee` is valid since speculation does not allocate or do write
            // barriers:
            rt->regs[calleeReg] = originalCallee;
            // These writes do invalidate `originalCallee`:
            rt->consts[*inlineCacheIdx].set(*rt, Default);
            rt->consts[*inlineCacheIdx + 1].set(*rt, Default);
            inlineCacheIdx = std::nullopt;
        }; break;

        case PrimopRes::ERROR: { // Set up an error call in its place:
            inlineCacheIdx = std::nullopt;
        }; break; // All is in place, just keep trampolining

        case PrimopRes::ABORT: return VMRes{};

        case PrimopRes::EXIT_VM: return VMRes{.val = rt->regs[retReg], .success = true};
        }
    }

    kontinue: {
        assert(isa<Continuation>(*rt, rt->regs[retContReg]));
        auto const ret = HRef<Continuation>::fromUnchecked(rt->regs[retContReg]);
        HRef<Method> const method = ret->method;
        auto const retPc = size_t(ret->pc.val());

        rt->setMethod(method);
        rt->pc = retPc;
        MethodCode const nativeReturnCode = *reinterpret_cast<MethodCode const*>(rt->code + rt->pc);
        rt->pc += sizeof(MethodCode);

        PrimopRes const res = nativeReturnCode(rt);
        switch (res) {
        case PrimopRes::INTERPRET: VM_CONTINUE;

        case PrimopRes::CALL_BYTECODE: goto apply;

        case PrimopRes::CONTINUE: // Returned:
            goto kontinue;

        case PrimopRes::EXIT_VM: return VMRes{.val = rt->regs[retReg], .success = true};

        default: PANIC("Unreachable code reached: %lu", uintptr_t(res));
        }
    }
    }
}

} // namespace
