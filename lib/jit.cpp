#include "jit.hpp"

#include "asmjit/x86.h"

namespace {

void jitCompile(RT& rt, Method& method) {
    asmjit::CodeHolder code;
    code.init(rt.jit.environment(), rt.jit.cpu_features());
    auto cc = asmjit::x86::Compiler(&code);

    cc.add_func(asmjit::FuncSignature::build<PrimopRes, RT*>());

    asmjit::x86::Gp const virt_reg = cc.new_gp64();
    cc.mov(virt_reg, PrimopRes::CALL_BYTECODE);
    cc.ret(virt_reg);

    cc.end_func();
    cc.finalize();

    MethodCode* entryCode = reinterpret_cast<MethodCode*>(method.code->itemsMut().data());
    if (rt.jit.add(entryCode, &code) != asmjit::Error::kOk) { PANIC("JIT miscompilation"); }
}

} // namespace
