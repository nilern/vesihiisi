#include "jit.hpp"

#include <algorithm>

#include "asmjit/x86.h"

#include "util/smallmap.hpp"
#include "bytecode.hpp"
#include "write.hpp"

namespace {

class X64SYSVJIT {
    RT* rt_;
    asmjit::FileLogger logger_;
    asmjit::CodeHolder code_;
    asmjit::x86::Assembler as_;
    SmallMap<size_t, asmjit::Label> labels_;

    void untagging(asmjit::x86::Gp const& dest, asmjit::x86::Gp const& src);

    void constLoad(asmjit::x86::Gp const& cReg, uint8_t constIdx);

    void heapedCheck(
        asmjit::x86::Gp const& v, asmjit::x86::Gp const& tagReg, asmjit::Label const& onImmediate);

    void naturalize(std::span<uint8_t const> bytecode);

public:
    X64SYSVJIT(RT& rt) :
        rt_{&rt}, logger_{stdout}, code_{},
        // HACK: `asmjit::x86::Assembler` cannot be copied or even moved, but we need to
        // `code_.init()` before constructing the assembler so use the comma operator to make that
        // possible in initializer list. Oh the joys of C++ initialization:
        as_{(code_.init(rt_->jit.environment(), rt_->jit.cpu_features()), &code_)},
        labels_{}
    {
        if (!eq(rt.debug->val().get(), False)) { code_.set_logger(&logger_); }
    }

    static constexpr asmjit::x86::Gp rtReg = asmjit::x86::rdi;
    static constexpr asmjit::x86::Gp retReg = asmjit::x86::rax;

    void jitMethod(Method& method);
};

/// `dest = src & payloadMask;`
void X64SYSVJIT::untagging(asmjit::x86::Gp const& dest, asmjit::x86::Gp const& src) {
    as_.movabs(dest, payloadMask);
    as_.and_(dest, src);
}

/// `ORef const c = rt->consts[constIdx].get();`
void X64SYSVJIT::constLoad(asmjit::x86::Gp const& cReg, uint8_t constIdx) {
    using namespace asmjit;

    as_.mov(cReg, x86::Mem{rtReg, int32_t(rt_->constsOffset())});
    size_t const constOffset = sizeof(ORef) * constIdx;
    as_.mov(cReg, x86::Mem{cReg, int32_t(constOffset)});
}

/// `if (!isHeaped(v)) goto onImmediate;` given `ORef v = ...`
void X64SYSVJIT::heapedCheck(
    asmjit::x86::Gp const& v, asmjit::x86::Gp const& tagReg, asmjit::Label const& onImmediate
) {
    using namespace asmjit;

    as_.movabs(tagReg, nonFlonumTag);
    as_.cmp(v, tagReg); // Actual NaN?
    as_.je(onImmediate);
    as_.test(v, tagReg); // `(callee.bits & tagMask) == heapedTag`?
    as_.jne(onImmediate);
}

void X64SYSVJIT::naturalize(std::span<uint8_t const> bytecode) {
    using namespace asmjit;

    auto const end = bytecode.end();
    for (
        auto it = bytecode.begin() + static_cast<decltype(end)::difference_type>(Method::entryPc());
        it != end;
    ) {
        { // If this has been a `br(f)` target, bind label here:
            auto const pc = size_t(std::distance(bytecode.begin(), it));
            std::optional<Label> const label = labels_.tryGet(pc);
            if (label) {
                as_.bind(*label);
            }
        }

        // TODO: JIT-compile the remaining bytecodes:
        switch (static_cast<Opcode>(*it++)) {
        case OP_MOVE: {
            uint8_t const destVReg = *it++;
            uint8_t const srcVReg = *it++;

            x86::Gp const tmpReg = x86::rax;
            // rt->pc += 3;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 3);` was
            // storing an incorrect value for some reason :(:
            as_.mov(tmpReg, 3);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            // rt->regs[destReg] = rt->regs[srcReg];
            size_t const srcOffset = rt_->regsOffset() + sizeof(ORef) * srcVReg;
            as_.mov(tmpReg, x86::Mem{rtReg, int32_t(srcOffset)});
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, tmpReg);
        }; break;

        case OP_SWAP:
        case OP_DEFINE:
        case OP_GLOBAL_SET: {
            as_.mov(retReg, PrimopRes::INTERPRET);
            as_.ret();
            return;
        }; break;

        case OP_GLOBAL: {
            uint8_t const destVReg = *it++;
            uint8_t const constIdx = *it++;

            // ORef const c = rt->consts[constIdx].get();
            x86::Gp const cReg = x86::rax;
            constLoad(cReg, constIdx);

            // if (!isHeaped(c)) goto interpret;
            auto const interpret = Label{};
            heapedCheck(cReg, x86::r11, interpret);
            // Object* const obj = &*HRef<Object>::fromUnchecked(c);
            x86::Gp const objReg = x86::r11;
            untagging(objReg, cReg);
            // HRef<Type> const type = obj->header()->type();
            x86::Gp const typeReg = x86::r10;
            as_.movabs(typeReg, heapedTag);
            as_.or_(typeReg, x86::Mem{objReg, int32_t(Object::typeOffset())});
            // if (!eq(type, rt->types.var)) goto interpret;
            size_t const varTypeOffset = rt_->typeOffset(offsetof(NamedTypes, var));
            as_.cmp(typeReg, x86::Mem{rtReg, int32_t(varTypeOffset)});
            as_.jne(interpret);
            // auto const var = static_cast<Var*>(obj);

            // ORef const v = var->val().get();
            x86::Gp const vReg = x86::r11;
            as_.mov(vReg, x86::Mem{objReg, int32_t(Var::valOffset())});
            // if (eq(v, rt->singletons.unbound)) goto interpret;
            size_t const unboundOffset = rt_->singletonOffset(offsetof(NamedSingletons, unbound));
            as_.cmp(vReg, x86::Mem{rtReg, int32_t(unboundOffset)});
            as_.je(interpret);
            // rt->regs[destReg] = v;
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, vReg);

            // rt->pc += 3;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 3);` was
            // storing an incorrect value for some reason :(:
            x86::Gp const tmpReg = x86::rax;
            as_.mov(tmpReg, 3);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            auto const done = Label{};
            as_.jmp(done);

            as_.bind(interpret);
            as_.mov(retReg, PrimopRes::INTERPRET);
            as_.ret();

            as_.bind(done);
        }; break;

        case OP_CONST: {
            uint8_t const destVReg = *it++;
            uint8_t const constIdx = *it++;

            x86::Gp const tmpReg = x86::rax;
            // rt->pc += 3;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 3);` was
            // storing an incorrect value for some reason :(:
            as_.mov(tmpReg, 3);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            // rt->regs[destReg] = rt->consts[constIdx].get();
            as_.mov(tmpReg, x86::Mem{rtReg, int32_t(rt_->constsOffset())});
            size_t const constOffset = sizeof(ORef) * constIdx;
            as_.mov(tmpReg, x86::Mem{tmpReg, int32_t(constOffset)});
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, tmpReg);
        }; break;

        case OP_SPECIALIZE:
        case OP_KNOT:
        case OP_KNOT_INIT:
        case OP_KNOT_GET: {
            as_.mov(retReg, PrimopRes::INTERPRET);
            as_.ret();
            return;
        }; break;

        case OP_BRF: {
            uint8_t const condVReg = *it++;
            uint16_t displacement = *it++;
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | *it++;

            auto const dest = asmjit::Label{};
            size_t const destPc = size_t(std::distance(bytecode.begin(), it)) + displacement;
            labels_.set(destPc, dest);

            x86::Gp const tmpReg = x86::rax;
            // rt->pc += 4;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 4);` was
            // storing an incorrect value for some reason :(:
            as_.mov(tmpReg, 4);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            // if (eq(rt->regs[condReg], False)) {
            x86::Gp const condReg = x86::rax;
            size_t const condOffset = rt_->regsOffset() + sizeof(ORef) * condVReg;
            as_.mov(condReg, x86::Mem{rtReg, int32_t(condOffset)});
            x86::Gp const falseReg = x86::r11;
            as_.movabs(falseReg, False.bits);
            as_.cmp(condReg, falseReg);
            auto const truthyLabel = asmjit::Label{};
            as_.jne(truthyLabel);
            //     rt->pc += displacement;
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, displacement);
            as_.jmp(dest);
            // }
            as_.bind(truthyLabel);
        }; break;

        case OP_BR: {
            uint16_t displacement = *it++;
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | *it++;

            auto const dest = asmjit::Label{};
            size_t const destPc = size_t(std::distance(bytecode.begin(), it)) + displacement;
            labels_.set(destPc, dest);

            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 3 + displacement);
            as_.jmp(dest);
        }; break;

        case OP_RET: {
            // Continuation* const ret = &*HRef<Continuation>::fromUnchecked(rt->regs[retContReg]);
            x86::Gp const retReg = x86::r11;
            size_t const retOffset = rt_->regsOffset() + sizeof(ORef) * retContReg;
            as_.movabs(retReg, payloadMask);
            as_.and_(retReg, x86::Mem{rtReg, int32_t(retOffset)});

            // HRef<Method> const method = ret->method;
            x86::Gp const methodReg = x86::r10;
            as_.mov(methodReg, x86::Mem{retReg, offsetof(Continuation, method)});

            // auto retPc = size_t(ret->pc.val());
            x86::Gp const pcReg = x86::r9;
            as_.movabs(pcReg, payloadMask);
            as_.and_(pcReg, x86::Mem{retReg, offsetof(Continuation, pc)});

            // rt->method = method;
            as_.mov(x86::Mem{rtReg, int32_t(rt_->methodOffset())}, methodReg);
            // Method* const methodPtr = &*method;
            // uint8_t const* code = HRef<ByteArray>::fromUnchecked(methodPtr->code)->flexData();
            // rt->code = code;
            x86::Gp const codeReg = x86::r11;
            as_.movabs(codeReg, payloadMask);
            as_.and_(methodReg, codeReg);
            as_.and_(codeReg, x86::Mem{methodReg, offsetof(Method, code)});
            as_.mov(x86::Mem{rtReg, int32_t(rt_->codeOffset())}, codeReg);
            // rt->consts = HRef<ArrayMut>::fromUnchecked(method->consts)->itemsMut().data();
            x86::Gp const constsReg = x86::r8;
            as_.movabs(constsReg, payloadMask);
            as_.and_(constsReg, x86::Mem{methodReg, offsetof(Method, consts)});
            size_t const constsObjOffset = rt_->constsOffset();
            as_.mov(x86::Mem{rtReg, int32_t(constsObjOffset)}, constsReg);
            // OPTIMIZE: `SlotsMut<ORef>::slots_` seems redundant for `RT::consts`:
            size_t const constsSlotsOffset = constsObjOffset + SlotsMut<ORef>::slotsOffset;
            as_.mov(x86::Mem{rtReg, int32_t(constsSlotsOffset)}, constsReg);
            // MethodCode const nativeReturnCode = *reinterpret_cast<MethodCode const*>(code + pc);
            x86::Gp const destReg = x86::rax;
            as_.mov(destReg, x86::Mem{codeReg, pcReg, 0, 0});
            // retPc += sizeof(MethodCode);
            // rt->pc = retPc;
            as_.add(pcReg, sizeof(MethodCode));
            as_.mov(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, pcReg);
            as_.jmp(destReg);
        }; break;

        case OP_CLOSURE: {
            as_.mov(retReg, PrimopRes::INTERPRET);
            as_.ret();
            return;
        }; break;

        case OP_CLOVER: {
            uint8_t const destVReg = *it++;
            uint8_t const closureVReg = *it++;
            uint8_t const cloverIdxVReg = *it++;

            x86::Gp const tmpReg = x86::rax;
            // rt->pc += 4;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 4);` was
            // storing an incorrect value for some reason :(:
            as_.mov(tmpReg, 4);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            // auto const closure = HRef<Closure>::fromUnchecked(rt->regs[closureReg]);
            size_t const closureOffset = rt_->regsOffset() + sizeof(ORef) * closureVReg;
            as_.mov(tmpReg, x86::Mem{rtReg, int32_t(closureOffset)});
            // rt->regs[destReg] = closure->clovers()[cloverIdx];
            size_t const cloverOffset = Closure::flexOffset + sizeof(ORef) * cloverIdxVReg;
            as_.mov(tmpReg, x86::Mem{tmpReg, int32_t(cloverOffset)});
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, tmpReg);
        }; break;

        case OP_UNSPILL: {
            uint8_t const destVReg = *it++;
            uint8_t const contVReg = *it++;
            uint8_t const cloverIdxVReg = *it++;

            x86::Gp const tmpReg = x86::rax;
            // rt->pc += 4;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 4);` was
            // storing an incorrect value for some reason :(:
            as_.mov(tmpReg, 4);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            // auto const cont = HRef<Continuation>::fromUnchecked(rt->regs[contReg]);
            size_t const contOffset = rt_->regsOffset() + sizeof(ORef) * contVReg;
            as_.mov(tmpReg, x86::Mem{rtReg, int32_t(contOffset)});
            // rt->regs[destReg] = cont->saves()[cloverIdx];
            size_t const cloverOffset = Continuation::flexOffset + sizeof(ORef) * cloverIdxVReg;
            as_.mov(tmpReg, x86::Mem{tmpReg, int32_t(cloverOffset)});
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, tmpReg);
        }; break;

        case OP_CALL: {
            as_.mov(retReg, PrimopRes::INTERPRET);
            as_.ret();
            return;
        }; break;

        case OP_TAILCALL: {
            [[maybe_unused]] uint8_t const inlineCacheIdx = *it++; // TODO: Make use of
            uint8_t const regCount = *it++;

            // rt->entryRegc = regCount;
            as_.mov(x86::Mem{rtReg, int32_t(rt_->entryRegcOffset())}, regCount);

            // ORef const callee = rt->regs[calleeReg];
            x86::Gp const calleeGp = x86::rax;
            size_t const calleeOffset = rt_->regsOffset() + sizeof(ORef) * calleeReg;
            as_.mov(calleeGp, x86::Mem{rtReg, int32_t(calleeOffset)});

            // if (!isHeaped(callee)) { return PrimopRes::CallBytecode; }
            x86::Gp const tagReg = x86::r11;
            as_.movabs(tagReg, nonFlonumTag);
            as_.cmp(calleeGp, tagReg); // Actual NaN?
            auto const callBytecode = Label{};
            as_.je(callBytecode);
            as_.test(calleeGp, tagReg); // `(callee.bits & tagMask) == heapedTag`?
            auto const callHeaped = Label{};
            as_.je(callHeaped);
            as_.bind(callBytecode);
            as_.mov(retReg, PrimopRes::CALL_BYTECODE);
            as_.ret();

            as_.bind(callHeaped);
            // Object* const calleePtr = &*callee;
            as_.movabs(tagReg, payloadMask);
            as_.and_(calleeGp, tagReg);
            // HRef<Type> const type = callee->header()->type();
            x86::Gp const typeReg = x86::r11;
            as_.movabs(typeReg, heapedTag);
            as_.or_(typeReg, x86::Mem{calleeGp, int32_t(Object::typeOffset())});

            // if (eq(type, rt->types.closure)) goto callClosure;
            x86::Gp const goalTypeReg = x86::r10;
            size_t const closureTypeOffset = rt_->typeOffset(offsetof(NamedTypes, closure));
            as_.mov(goalTypeReg, x86::Mem{rtReg, int32_t(closureTypeOffset)});
            as_.cmp(typeReg, goalTypeReg);
            auto const callClosure = Label{};
            as_.je(callClosure);

            // TODO: JIT multimethod cache probe:
            as_.jmp(callBytecode);

            as_.bind(callClosure);
            // HRef<Method>::fromUnchecked(calleePtr->method)->nativeCode()(rt);
            x86::Gp const methodReg = x86::r11;
            as_.movabs(methodReg, payloadMask);
            as_.and_(methodReg, x86::Mem{calleeGp, int32_t(offsetof(Closure, method))});
            as_.movabs(calleeGp, payloadMask);
            as_.and_(calleeGp, x86::Mem{methodReg, int32_t(offsetof(Method, code))});
            as_.jmp(x86::Mem{calleeGp, 0});
        }; break;

        case OP_FFICALL: {
            as_.mov(retReg, PrimopRes::INTERPRET);
            as_.ret();
            return;
        }; break;
        }
    }
}

void X64SYSVJIT::jitMethod(Method& method) {
    using namespace asmjit;

    if (code_.logger()) {
        logger_.log("\n;; # JITed ");
        write(rt_, logger_.file(), HRef{&method});
        logger_.log(", Native Code:\n");
    }

    if (!std::ranges::all_of(method.domain(),
                             [&](ORef type) { return eq(type, rt_->types.any); })
        ) {
        // TODO: Generate (non-punting) code for these functions with nontrivial domain.
        as_.mov(x86::rax, PrimopRes::CALL_BYTECODE);
        as_.ret();
    } else {
        auto const doCheckDomain = as_.new_label();
        auto const doCheckSpeculation = as_.new_label();
        auto const argsChecked = as_.new_label();

        // HRef<Method> const method = rt->regs[calleeReg]->method;
        size_t const calleeOffset = rt_->regsOffset() + sizeof(ORef) * calleeReg;
        as_.movabs(x86::r10, payloadMask);
        as_.and_(x86::r10, x86::Mem{rtReg, int32_t(calleeOffset)});
        as_.mov(x86::r10, x86::Mem{x86::r10, offsetof(Closure, method)});

        // RT::DomainChecking const domainChecking = rt->domainChecking;
        // if (domainChecking == RT::DomainChecking::SKIP) {
        as_.mov(x86::rax, x86::Mem{rtReg, int32_t(rt_->domainCheckingOffset())});
        as_.cmp(x86::rax, RT::DomainChecking::SKIP);
        as_.jne(doCheckDomain);
        //      domainChecking = RT::DomainChecking::CHECK;
        as_.mov(x86::Mem{rtReg, int32_t(rt_->domainCheckingOffset())},
                RT::DomainChecking::CHECK);
        //      goto argsChecked;
        as_.jmp(argsChecked);
        // }

        as_.bind(doCheckDomain);
        // if (domainChecking == RT::DomainChecking::CHECK) {
        as_.cmp(x86::rax, RT::DomainChecking::CHECK);
        as_.jne(doCheckSpeculation);
        //     size_t const argc = state->entryRegc - firstArgReg;
        as_.mov(x86::rax, x86::Mem{rtReg, int32_t(rt_->entryRegcOffset())});
        as_.sub(x86::rax, firstArgReg);
        //     auto const arity = size_t(method->flexCount().val());
        as_.movabs(x86::r9, payloadMask);
        as_.and_(x86::r9, x86::r10);
        as_.and_(x86::r11, x86::Mem{x86::r9, int32_t(flexCountOffset)});
        //     if (argc != arity) {
        as_.cmp(x86::rax, x86::r11);
        as_.je(argsChecked);
        if (method.hasVarArg.val()) {
            // TODO: Generate (non-punting) code for this:
            as_.mov(retReg, PrimopRes::CALL_BYTECODE);
            as_.ret();
        }
        // Domain check failed. Fall back to interpreter to face consequences (and do redundant
        // work, but realistically a logic error like this should end the entire process):
        as_.mov(retReg, PrimopRes::CALL_BYTECODE);
        as_.ret();
        //     }
        // }

        as_.bind(doCheckSpeculation);
        // TODO: Generate (non-punting) code for this:
        as_.mov(retReg, PrimopRes::CALL_BYTECODE);
        as_.ret();

        as_.bind(argsChecked);

        if (method.hasVarArg.val()) {
            // TODO: Generate (non-punting) code for vararg reification:
            as_.mov(retReg, PrimopRes::CALL_BYTECODE);
            as_.ret();
        }

        // rt->method = method;
        as_.mov(x86::Mem{rtReg, int32_t(rt_->methodOffset())}, x86::r10);
        // rt->code = HRef<ByteArray>::fromUnchecked(method->code)->flexData();
        as_.movabs(x86::r11, payloadMask);
        as_.and_(x86::r10, x86::r11);
        as_.and_(x86::r11, x86::Mem{x86::r10, offsetof(Method, code)});
        as_.mov(x86::Mem{rtReg, int32_t(rt_->codeOffset())}, x86::r11);
        // rt->consts = HRef<ArrayMut>::fromUnchecked(method->consts)->itemsMut().data();
        as_.movabs(x86::r11, payloadMask);
        as_.and_(x86::r11, x86::Mem{x86::r10, offsetof(Method, consts)});
        size_t const constsObjOffset = rt_->constsOffset();
        as_.mov(x86::Mem{rtReg, int32_t(constsObjOffset)}, x86::r11);
        // OPTIMIZE: `SlotsMut<ORef>::slots_` seems redundant for `RT::consts`:
        size_t const constsSlotsOffset = constsObjOffset + SlotsMut<ORef>::slotsOffset;
        as_.mov(x86::Mem{rtReg, int32_t(constsSlotsOffset)}, x86::r11);
        // rt->pc = Method::entryPc();
        // `as_.mov(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, Method::entryPc());` was
        // storing an incorrect value for some reason :(:
        as_.mov(x86::r11, Method::entryPc());
        as_.mov(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, x86::r11);

        naturalize(method.code->items());
    }

    MethodCode* entryCode = reinterpret_cast<MethodCode*>(method.code->itemsMut().data());
    if (asmjit::Error const err = rt_->jit.add(entryCode, &code_); err != Error::kOk) {
        PANIC("JIT miscompilation: %s", asmjit::DebugUtils::error_as_string(err));
    }
}

void jitCompile(RT& rt, Method& method) {
    X64SYSVJIT{rt}.jitMethod(method);
}

} // namespace
