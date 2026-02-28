#include "jit.hpp"

#include <algorithm>

#include "asmjit/x86.h"

#include "bytecode.hpp"

namespace {

class X64SYSVJIT {
public:
    static constexpr asmjit::x86::Gp rtReg = asmjit::x86::rdi;
    static constexpr asmjit::x86::Gp retReg = asmjit::x86::rax;
};

void jitCompile(RT& rt, Method& method) {
    using namespace asmjit;

    CodeHolder code;
    code.init(rt.jit.environment(), rt.jit.cpu_features());
    auto as = x86::Assembler(&code);

    if (!std::ranges::all_of(method.domain(), [&](ORef type) { return eq(type, rt.types.any); })) {
        // TODO: Generate (non-punting) code for these functions with nontrivial domain.
        as.mov(x86::rax, PrimopRes::CALL_BYTECODE);
        as.ret();
    } else {
        auto const doCheckDomain = as.new_label();
        auto const doCheckSpeculation = as.new_label();
        auto const argsChecked = as.new_label();

        // HRef<Method> const method = rt->regs[calleeReg]->method;
        size_t const calleeOffset = rt.regsOffset() + sizeof(ORef) * calleeReg;
        as.mov(x86::r10, payloadMask);
        as.and_(x86::r10, x86::Mem{X64SYSVJIT::rtReg, int32_t(calleeOffset)});
        as.mov(x86::r10, x86::Mem{x86::r10, offsetof(Closure, method)});

        // RT::DomainChecking const domainChecking = rt->domainChecking;
        // if (domainChecking == RT::DomainChecking::SKIP) {
        as.mov(x86::rax, x86::Mem{X64SYSVJIT::rtReg, int32_t(rt.domainCheckingOffset())});
        as.cmp(x86::rax, RT::DomainChecking::SKIP);
        as.jne(doCheckDomain);
        //      domainChecking = RT::DomainChecking::CHECK;
        as.mov(x86::Mem{X64SYSVJIT::rtReg, int32_t(rt.domainCheckingOffset())},
               RT::DomainChecking::CHECK);
        //      goto argsChecked;
        as.jmp(argsChecked);
        // }

        as.bind(doCheckDomain);
        // if (domainChecking == RT::DomainChecking::CHECK) {
        as.cmp(x86::rax, RT::DomainChecking::CHECK);
        as.jne(doCheckSpeculation);
        //     size_t const argc = state->entryRegc - firstArgReg;
        as.mov(x86::rax, x86::Mem{X64SYSVJIT::rtReg, int32_t(rt.entryRegcOffset())});
        as.sub(x86::rax, firstArgReg);
        //     auto const arity = size_t(method->flexCount().val());
        as.movabs(x86::r9, payloadMask);
        as.and_(x86::r9, x86::r10);
        as.and_(x86::r11, x86::Mem{x86::r9, int32_t(flexCountOffset)});
        //     if (argc != arity) {
        as.cmp(x86::rax, x86::r11);
        as.je(argsChecked);
        if (method.hasVarArg.val()) {
            // TODO: Generate (non-punting) code for this:
            as.mov(X64SYSVJIT::retReg, PrimopRes::CALL_BYTECODE);
            as.ret();
        }
        // Domain check failed. Fall back to interpreter to face consequences (and do redundant
        // work, but realistically a logic error like this should end the entire process):
        as.mov(X64SYSVJIT::retReg, PrimopRes::CALL_BYTECODE);
        as.ret();
        //     }
        // }

        as.bind(doCheckSpeculation);
        // TODO: Generate (non-punting) code for this:
        as.mov(X64SYSVJIT::retReg, PrimopRes::CALL_BYTECODE);
        as.ret();

        as.bind(argsChecked);

        if (method.hasVarArg.val()) {
            // TODO: Generate (non-punting) code for vararg reification:
            as.mov(X64SYSVJIT::retReg, PrimopRes::CALL_BYTECODE);
            as.ret();
        }

        // rt->method = method;
        as.mov(x86::Mem{X64SYSVJIT::rtReg, int32_t(rt.methodOffset())}, x86::r10);
        // rt->code = HRef<ByteArray>::fromUnchecked(method->code)->flexData();
        as.movabs(x86::r11, payloadMask);
        as.and_(x86::r10, x86::r11);
        as.and_(x86::r11, x86::Mem{x86::r10, offsetof(Method, code)});
        as.mov(x86::Mem{X64SYSVJIT::rtReg, int32_t(rt.codeOffset())}, x86::r11);
        // rt->consts = HRef<ArrayMut>::fromUnchecked(method->consts)->itemsMut().data();
        as.movabs(x86::r11, payloadMask);
        as.and_(x86::r11, x86::Mem{x86::r10, offsetof(Method, consts)});
        size_t const constsObjOffset = rt.constsOffset();
        as.mov(x86::Mem{X64SYSVJIT::rtReg, int32_t(constsObjOffset)}, x86::r11);
        // OPTIMIZE: `SlotsMut<ORef>::slots_` seems redundant for `RT::consts`:
        size_t const constsSlotsOffset = constsObjOffset + SlotsMut<ORef>::slotsOffset;
        as.mov(x86::Mem{X64SYSVJIT::rtReg, int32_t(constsSlotsOffset)}, x86::r11);
        // rt->pc = Method::entryPc();
        // `as.mov(x86::Mem{X64SYSVJIT::rtReg, int32_t(rt.pcOffset())}, Method::entryPc());` was
        // storing an incorrect value for some reason :(:
        as.mov(x86::r11, Method::entryPc());
        as.mov(x86::Mem{X64SYSVJIT::rtReg, int32_t(rt.pcOffset())}, x86::r11);
        // TODO: JIT actual body code:
        as.mov(X64SYSVJIT::retReg, PrimopRes::INTERPRET);
        as.ret();
    }

    MethodCode* entryCode = reinterpret_cast<MethodCode*>(method.code->itemsMut().data());
    if (rt.jit.add(entryCode, &code) != Error::kOk) { PANIC("JIT miscompilation"); }
}

} // namespace
