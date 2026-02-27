#include "jit.hpp"

#include "asmjit/x86.h"

namespace {

void jitCompile(RT& rt, Method& method) {
    asmjit::CodeHolder code;
    code.init(rt.jit.environment(), rt.jit.cpu_features());
    auto as = asmjit::x86::Assembler(&code);

    as.mov(asmjit::x86::rax, PrimopRes::CALL_BYTECODE);
    as.ret();

    MethodCode* entryCode = reinterpret_cast<MethodCode*>(method.code->itemsMut().data());
    if (rt.jit.add(entryCode, &code) != asmjit::Error::kOk) { PANIC("JIT miscompilation"); }
}

} // namespace
