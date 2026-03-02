#include "jit.hpp"

#include <algorithm>

#include "asmjit/x86.h"

#include "bytecode.hpp"
#include "write.hpp"

namespace {

class X64SYSVJIT {
    RT* rt_;
    asmjit::FileLogger logger_;
    asmjit::CodeHolder code_;
    asmjit::x86::Assembler as_;

    void naturalize(std::span<uint8_t const> bytecode);

public:
    X64SYSVJIT(RT& rt) :
        rt_{&rt}, logger_{stdout}, code_{},
        // HACK: `asmjit::x86::Assembler` cannot be copied or even moved, but we need to
        // `code_.init()` before constructing the assembler so use the comma operator to make that
        // possible in initializer list. Oh the joys of C++ initialization:
        as_{(code_.init(rt_->jit.environment(), rt_->jit.cpu_features()), &code_)}
    {
        if (!eq(rt.debug->val().get(), False)) { code_.set_logger(&logger_); }
    }

    static constexpr asmjit::x86::Gp rtReg = asmjit::x86::rdi;
    static constexpr asmjit::x86::Gp retReg = asmjit::x86::rax;

    void jitMethod(Method& method);
};

void X64SYSVJIT::naturalize(std::span<uint8_t const> bytecode) {
    using namespace asmjit;

    auto const end = bytecode.end();
    for (
        auto it = bytecode.begin() + static_cast<decltype(end)::difference_type>(Method::entryPc());
        it != end;
    ) {
        // TODO: JIT-compile the remaining bytecodes:
        switch (static_cast<Opcode>(*it++)) {
        case OP_MOVE:
        case OP_SWAP:
        case OP_DEFINE:
        case OP_GLOBAL_SET:
        case OP_GLOBAL:
        case OP_CONST:
        case OP_SPECIALIZE:
        case OP_KNOT:
        case OP_KNOT_INIT:
        case OP_KNOT_GET:
        case OP_BRF:
        case OP_BR: {
            as_.mov(retReg, PrimopRes::INTERPRET);
            as_.ret();
            return;
        }; break;

        case OP_RET: {
            // Continuation* const ret = &*HRef<Continuation>::fromUnchecked(rt->regs[retContReg]);
            x86::Gp const retReg = x86::r11;
            size_t const retOffset = rt_->regsOffset() + sizeof(ORef) * retContReg;
            as_.mov(retReg, payloadMask);
            as_.and_(retReg, x86::Mem{rtReg, int32_t(retOffset)});

            // HRef<Method> const method = ret->method;
            x86::Gp const methodReg = x86::r10;
            as_.mov(methodReg, x86::Mem{retReg, offsetof(Continuation, method)});

            // auto retPc = size_t(ret->pc.val());
            x86::Gp const pcReg = x86::r10;
            as_.mov(pcReg, payloadMask);
            as_.mov(pcReg, x86::Mem{retReg, offsetof(Continuation, pc)});

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
            x86::Gp const constsReg = x86::r9;
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

        case OP_CLOSURE:
        case OP_CLOVER:
        case OP_CALL:
        case OP_TAILCALL:
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
        as_.mov(x86::r10, payloadMask);
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
    if (rt_->jit.add(entryCode, &code_) != Error::kOk) { PANIC("JIT miscompilation"); }
}

void jitCompile(RT& rt, Method& method) {
    X64SYSVJIT{rt}.jitMethod(method);
}

} // namespace
