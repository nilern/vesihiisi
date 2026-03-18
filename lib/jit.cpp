#include "jit.hpp"

#include <algorithm>

#include "asmjit/x86.h"

#include "util/smallmap.hpp"
#include "bytecode.hpp"
#include "vm.hpp"
#include "write.hpp"

namespace {

class X64SYSVJIT {
    RT* rt_;
    asmjit::FileLogger logger_;
    asmjit::CodeHolder code_;
    asmjit::x86::Assembler as_;
    SmallMap<size_t, asmjit::Label> labels_;

    void tagging(asmjit::x86::Gp const& dest, asmjit::x86::Gp const& src, uint64_t tag);
    void untagging(asmjit::x86::Gp const& dest, asmjit::x86::Gp const& src);

    void vregLoad(asmjit::x86::Gp const& dest, uint8_t vRegIdx);
    void vregStore(uint8_t vRegIdx, asmjit::x86::Gp const& src);

    void constLoad(asmjit::x86::Gp const& cReg, uint8_t constIdx);

    void heapedCheck(
        asmjit::x86::Gp const& v, asmjit::x86::Gp const& tagReg, asmjit::Label const& onImmediate);

    void checkedHeapedUntagging(
        asmjit::x86::Gp const& dest, asmjit::x86::Gp const& src, asmjit::x86::Gp const& tmp,
        size_t typeOffsetInRT, asmjit::Label const& onWrongType);

    void interpreterFallback();

    void emitCall(uint8_t inlineCacheIdx, uint8_t regCount, asmjit::Label const& interpret);

    void naturalize(Method const& method, std::span<uint8_t const> bytecode);

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

/// `dest = tag | src;`
void X64SYSVJIT::tagging(asmjit::x86::Gp const& dest, asmjit::x86::Gp const& src, uint64_t tag) {
    assert(dest != src);

    as_.movabs(dest, tag);
    as_.or_(dest, src);
}

/// `dest = src & payloadMask;`
void X64SYSVJIT::untagging(asmjit::x86::Gp const& dest, asmjit::x86::Gp const& src) {
    assert(dest != src);

    as_.movabs(dest, payloadMask);
    as_.and_(dest, src);
}

/// `ORef const dest = rt->regs[vRegIdx]`
void X64SYSVJIT::vregLoad(asmjit::x86::Gp const& dest, uint8_t vRegIdx) {
    using namespace asmjit;

    size_t const vregOffset = rt_->regsOffset() + sizeof(ORef) * vRegIdx;
    as_.mov(dest, x86::Mem{rtReg, int32_t(vregOffset)});
}

/// `rt->regs[vRegIdx] = src`
void X64SYSVJIT::vregStore(uint8_t vRegIdx, asmjit::x86::Gp const& src) {
    using namespace asmjit;

    size_t const vregOffset = rt_->regsOffset() + sizeof(ORef) * vRegIdx;
    as_.mov(x86::Mem{rtReg, int32_t(vregOffset)}, src);
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

/// if (!isa<T>(*rt, v)) goto onWrongType;
/// T* dest = &*HRef<T>::fromUnchecked(v);
void X64SYSVJIT::checkedHeapedUntagging(
    asmjit::x86::Gp const& dest, asmjit::x86::Gp const& src, asmjit::x86::Gp const& tmp,
    size_t typeOffsetInRT, asmjit::Label const& onWrongType
) {
    assert(dest != src);
    assert(src != tmp);
    assert(tmp != dest);

    using namespace asmjit;

    // if (!isHeaped(v)) goto onWrongType;
    heapedCheck(src, tmp, onWrongType);
    // Object* const obj = &*HRef<Object>::fromUnchecked(c);
    untagging(dest, src);
    // HRef<Type> const type = obj->header()->type();
    x86::Gp const typeReg = tmp;
    as_.movabs(typeReg, heapedTag);
    as_.or_(typeReg, x86::Mem{dest, int32_t(Object::typeOffset())});
    // if (!eq(type, rt->types.$type)) goto onWrongType;
    as_.cmp(typeReg, x86::Mem{rtReg, int32_t(typeOffsetInRT)});
    as_.jne(onWrongType);
    // auto dest = static_cast<T*>(obj);
}

void X64SYSVJIT::interpreterFallback() {
    using namespace asmjit;

    as_.mov(retReg, PrimopRes::INTERPRET);
    as_.ret();
}

void X64SYSVJIT::emitCall(
    uint8_t inlineCacheIdx, uint8_t regCount, asmjit::Label const& interpret
) {
    using namespace asmjit;

    // rt->entryRegc = regCount;
    as_.mov(x86::Mem{rtReg, int32_t(rt_->entryRegcOffset()), sizeof(RT::entryRegc)}, regCount);

    // ORef const callee = rt->regs[calleeReg];
    x86::Gp const calleeGp = x86::rax;
    vregLoad(calleeGp, calleeReg);

    // if (!isHeaped(callee)) goto interpret;
    x86::Gp const tagReg = x86::r11;
    heapedCheck(calleeGp, tagReg, interpret);

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
    Label const callClosure = as_.new_anonymous_label("callClosure");
    as_.je(callClosure);

    // if (!eq(type, rt->types.multimethod)) goto interpret;
    // auto const multiCalleeRef = HRef<Multimethod>::fromUnchecked(callee);
    size_t const multimethodTypeOffset = rt_->typeOffset(offsetof(NamedTypes, multimethod));
    as_.mov(goalTypeReg, x86::Mem{rtReg, int32_t(multimethodTypeOffset)});
    as_.cmp(typeReg, goalTypeReg);
    as_.jne(interpret);
    // if (!eq(state->consts[inlineCacheIdx].get(), multiCalleeRef->methods().get()))
    //     goto interpret;
    x86::Gp const goalMethodsReg = x86::r11;
    constLoad(goalMethodsReg, inlineCacheIdx);
    x86::Gp const methodsReg = x86::r10;
    as_.mov(methodsReg, x86::Mem{calleeGp, int32_t(Multimethod::methodsOffset())});
    as_.cmp(goalMethodsReg, methodsReg);
    as_.jne(interpret);
    // state->regs[calleeReg] = state->consts[inlineCacheIdx + 1].get();
    x86::Gp const cachedClosureReg = x86::r11;
    constLoad(cachedClosureReg, inlineCacheIdx + 1);
    vregStore(calleeReg, cachedClosureReg);
    // state->domainChecking = RT::DomainChecking::SPECULATE;
    as_.mov(x86::Mem{rtReg, int32_t(rt_->domainCheckingOffset()), sizeof(RT::DomainChecking)},
            RT::DomainChecking::SPECULATE);

    as_.bind(callClosure);
    // HRef<Method>::fromUnchecked(calleePtr->method)->nativeCode()(rt);
    x86::Gp const methodReg = x86::r11;
    as_.movabs(methodReg, payloadMask);
    as_.and_(methodReg, x86::Mem{calleeGp, int32_t(offsetof(Closure, method))});
    as_.movabs(calleeGp, payloadMask);
    as_.and_(calleeGp, x86::Mem{methodReg, int32_t(offsetof(Method, code))});
    as_.jmp(x86::Mem{calleeGp, 0});
}

void X64SYSVJIT::naturalize(Method const& method, std::span<uint8_t const> bytecode) {
    using namespace asmjit;

    auto const end = bytecode.end();
    for (
        auto it = bytecode.begin() + static_cast<decltype(end)::difference_type>(Method::entryPc());
        it != end;
    ) {
        auto const pc = size_t(std::distance(bytecode.begin(), it));

        if (code_.logger()) {
            FILE* const dest = logger_.file();
            disassembleInstrAt(rt_, false, dest, HRef{&method}, pc);
            putc('\n', dest);
        }

        { // If this has been a `br(f)` target, bind label here:
            std::optional<Label> const label = labels_.tryGet(pc);
            if (label) {
                as_.bind(*label);
            }
        }

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
            vregLoad(tmpReg, srcVReg);
            vregStore(destVReg, tmpReg);
        }; break;

        case OP_SWAP: {
            uint8_t const reg1 = *it++;
            uint8_t const reg2 = *it++;

            x86::Gp const tmpReg = x86::rax;
            // rt->pc += 3;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 3);` was
            // storing an incorrect value for some reason :(:
            as_.mov(tmpReg, 3);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);

            // ORef const tmp = rt->regs[reg1];
            vregLoad(tmpReg, reg1);
            // rt->regs[reg1] = rt->regs[reg2];
            x86::Gp const tmpReg2 = x86::rcx;
            vregLoad(tmpReg2, reg2);
            vregStore(reg1, tmpReg2);
            // rt->regs[reg2] = tmp;
            vregStore(reg2, tmpReg);
        }; break;

        // These differ only in the initial linkage, which we do not JIT-compile:
        case OP_DEFINE:
        case OP_GLOBAL_SET: {
            uint8_t const constIdx = *it++;
            uint8_t const srcVReg = *it++;

            // ORef const c = rt->consts[constIdx].get();
            x86::Gp const cReg = x86::rax;
            constLoad(cReg, constIdx);

            // if (!isHeaped(c)) goto interpret;
            Label const interpret = as_.new_anonymous_label("interpret");
            heapedCheck(cReg, x86::r11, interpret);
            // Object* const obj = &*HRef<Object>::fromUnchecked(c);
            x86::Gp const objReg = x86::rsi;
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

            // ORef const v = rt->regs[srcReg];
            x86::Gp const vReg = x86::r11;
            size_t const srcOffset = rt_->regsOffset() + sizeof(ORef) * srcVReg;
            as_.mov(vReg, x86::Mem{rtReg, int32_t(srcOffset)});
            // // var->val().set(*rt, v);
            // if (!Heap::writeBarrier(&rt->heap, var)) goto interpret;
            as_.push(rtReg);
            as_.push(objReg);
            as_.push(vReg);
            x86::Gp const heapReg = x86::rdi;
            as_.lea(heapReg, x86::Mem{rtReg, int32_t(rt_->heapOffset())});
            Heap::writeBarrier_t writeBarrier = &Heap::writeBarrier;
            as_.call(writeBarrier);
            as_.pop(vReg);
            as_.pop(objReg);
            as_.pop(rtReg);
            as_.test(retReg, retReg);
            as_.je(interpret);
            // var->val_ = v;
            as_.mov(x86::Mem{objReg, int32_t(Var::valOffset())}, vReg);

            // rt->pc += 3;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 3);` was
            // storing an incorrect value for some reason :(:
            x86::Gp const tmpReg = x86::rax;
            as_.mov(tmpReg, 3);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            Label const done = as_.new_anonymous_label("done");
            as_.jmp(done);

            as_.bind(interpret);
            interpreterFallback();

            as_.bind(done);
        }; break;

        case OP_GLOBAL: {
            uint8_t const destVReg = *it++;
            uint8_t const constIdx = *it++;

            // ORef const c = rt->consts[constIdx].get();
            x86::Gp const cReg = x86::rax;
            constLoad(cReg, constIdx);

            // if (!isHeaped(c)) goto interpret;
            Label const interpret = as_.new_anonymous_label("interpret");
            heapedCheck(cReg, x86::r11, interpret);
            // Object* const obj = &*HRef<Object>::fromUnchecked(c);
            x86::Gp const objReg = x86::rsi; // For consistency with OP_DEFINE & OP_GLOBAL_SET
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
            Label const done = as_.new_anonymous_label("done");
            as_.jmp(done);

            as_.bind(interpret);
            interpreterFallback();

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
            constLoad(tmpReg, constIdx);
            vregStore(destVReg, tmpReg);
        }; break;

        case OP_SPECIALIZE: {
            uint8_t const destVReg = *it++;
            uint8_t const constIdx = *it++;
            uint8_t const typeSetByteCount = *it++;
            ptrdiff_t const typeSetStartIdx = std::distance(bytecode.begin(), it);
            size_t typeCount = 0;
            for (uint8_t const byte :
                 std::span{bytecode.begin() + typeSetStartIdx, typeSetByteCount}
            ) {
                typeCount += stdc_count_ones(byte);
            }
            it += typeSetByteCount;

            // ArrayMut* const types = allocArray(rt, Fixnum{intptr_t(typeCount)});
            x86::Gp const typeCountReg = x86::rsi; // ABI arg 2
            as_.movabs(typeCountReg, Fixnum{intptr_t(typeCount)}.bits);
            as_.push(rtReg);
            as_.call(allocArray);
            x86::Gp const typesReg = retReg;
            as_.pop(rtReg);

            Label const interpret = as_.new_anonymous_label("interpret");
            {
                x86::Gp const typeReg = x86::r11;
                x86::Gp const typeObjReg = x86::r10;
                x86::Gp const tmpReg = x86::r9;
                size_t typeIdx = 0;
                size_t regIdx = 0;
                for (uint8_t const byte :
                     std::span{bytecode.begin() + typeSetStartIdx, typeSetByteCount}
                ) {
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> bitIdx) & 1) {
                            // ORef const maybeType = rt->regs[regIdx];
                            size_t const regOffset = rt_->regsOffset() + sizeof(ORef) * regIdx;
                            as_.mov(typeReg, x86::Mem{rtReg, int32_t(regOffset)});

                            // if (!isa<Type>(*rt, maybeType)) goto interpret;
                            // (Incidentally `Type* const typeObj` but not used here:)
                            checkedHeapedUntagging(typeObjReg, typeReg, tmpReg,
                                                   rt_->typeOffset(offsetof(NamedTypes, type)),
                                                   interpret);

                            // types[typeIdx] = maybeType;
                            size_t const typeOffset = sizeof(ORef) * typeIdx;
                            as_.mov(x86::Mem{typesReg, int32_t(typeOffset)}, typeReg);

                            ++typeIdx;
                        }

                        ++regIdx;
                    }
                }
            }

            // auto const typesRef = HRef{types};
            x86::Gp const typesRefReg = x86::rdx; // ABI arg 3
            tagging(typesRefReg, typesReg, heapedTag);

            // HRef<Method> const generic = HRef<Method>::fromUnchecked(rt->consts[constIdx].get());
            x86::Gp const genericReg = x86::rsi; // ABI arg 2
            constLoad(genericReg, constIdx);
            // HRef<Method> const method = specialize(rt, generic, types);
            as_.push(rtReg);
            as_.call(specialize);
            x86::Gp const methodReg = retReg;
            as_.pop(rtReg);

            // rt->regs[destReg] = method;
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, methodReg);

            // rt->pc += instrSize;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, instrSize);` was
            // storing an incorrect value for some reason :(:
            x86::Gp const tmpReg = x86::rax;
            size_t const instrSize = 4 + typeSetByteCount;
            as_.mov(tmpReg, instrSize);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            Label const done = as_.new_anonymous_label("done");
            as_.jmp(done);

            as_.bind(interpret);
            interpreterFallback();

            as_.bind(done);
        }; break;

        case OP_KNOT: {
            uint8_t const destVReg = *it++;

            x86::Gp const tmpReg = x86::rax;
            // rt->pc += 2;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 2);` was
            // storing an incorrect value for some reason :(:
            as_.mov(tmpReg, 2);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);

            // rt->regs[destReg] = allocKnot(rt);
            as_.push(rtReg);
            as_.call(allocKnot);
            as_.pop(rtReg);
            x86::Gp const knotReg = retReg;
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, knotReg);
        }; break;

        case OP_KNOT_INIT: {
            uint8_t const knotVReg = *it++;
            uint8_t const srcVReg = *it++;

            // Knot* const knot = &*HRef<Knot>::fromUnchecked(rt->regs[knotReg]);
            x86::Gp const knotReg = x86::rax;
            size_t const knotOffset = rt_->regsOffset() + sizeof(ORef) * knotVReg;
            as_.movabs(knotReg, payloadMask);
            as_.and_(knotReg, x86::Mem{rtReg, int32_t(knotOffset)});

            // ORef const v = rt->regs[srcReg];
            x86::Gp const vReg = x86::r11;
            size_t const srcOffset = rt_->regsOffset() + sizeof(ORef) * srcVReg;
            as_.mov(vReg, x86::Mem{rtReg, int32_t(srcOffset)});
            // // knot->val().set(*rt, v);
            // if (!Heap::writeBarrier(&rt->heap, var)) goto interpret;
            as_.push(rtReg);
            as_.push(knotReg);
            as_.push(vReg);
            x86::Gp const heapReg = x86::rdi;
            as_.lea(heapReg, x86::Mem{rtReg, int32_t(rt_->heapOffset())});
            Heap::writeBarrier_t writeBarrier = &Heap::writeBarrier;
            as_.call(writeBarrier);
            as_.pop(vReg);
            as_.pop(knotReg);
            as_.pop(rtReg);
            as_.test(retReg, retReg);
            Label const interpret = as_.new_anonymous_label("interpret");
            as_.je(interpret);
            // knot->val_ = v;
            as_.mov(x86::Mem{knotReg, int32_t(Knot::valOffset())}, vReg);

            // rt->pc += 3;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 3);` was
            // storing an incorrect value for some reason :(:
            x86::Gp const tmpReg = x86::rax;
            as_.mov(tmpReg, 3);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            Label const done = as_.new_anonymous_label("done");
            as_.jmp(done);

            as_.bind(interpret);
            interpreterFallback();

            as_.bind(done);
        }; break;

        case OP_KNOT_GET: {
            uint8_t const destVReg = *it++;
            uint8_t const knotVReg = *it++;

            x86::Gp const tmpReg = x86::rax;
            // rt->pc += 3;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, 3);` was
            // storing an incorrect value for some reason :(:
            as_.mov(tmpReg, 3);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);

            // auto const knot = HRef<Knot>::fromUnchecked(rt->regs[knotReg]);
            x86::Gp const knotReg = x86::rax;
            size_t const knotOffset = rt_->regsOffset() + sizeof(ORef) * knotVReg;
            as_.mov(knotReg, x86::Mem{rtReg, int32_t(knotOffset)});
            // rt->regs[destReg] = knot->val().get();
            as_.mov(knotReg, x86::Mem{knotReg, int32_t(Knot::valOffset())});
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, knotReg);
        }; break;

        case OP_BRF: {
            uint8_t const condVReg = *it++;
            uint16_t displacement = *it++;
            displacement = (uint16_t)(displacement << UINT8_WIDTH) | *it++;

            Label const dest = as_.new_anonymous_label("dest");
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
            Label const truthyLabel = as_.new_anonymous_label("truthyLabel");
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

            Label const dest = as_.new_anonymous_label("dest");
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
            uint8_t const destVReg = *it++;
            uint8_t const methodVReg = *it++;
            uint8_t const closesByteCount = *it++;
            ptrdiff_t const closesStartIdx = std::distance(bytecode.begin(), it);
            size_t cloverCount = 0;
            for (uint8_t const byte : std::span{bytecode.begin() + closesStartIdx, closesByteCount})
            {
                cloverCount += stdc_count_ones(byte);
            }
            it += closesByteCount;

            // rt->pc += instrSize;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, instrSize);` was
            // storing an incorrect value for some reason :(:
            x86::Gp const tmpReg = x86::rax;
            size_t const instrSize = 4 + closesByteCount;
            as_.mov(tmpReg, instrSize);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);

            // HRef<Method> const method = HRef<Method>::fromUnchecked(rt->regs[methodReg]);
            x86::Gp const methodReg = x86::rsi; // ABI arg 2
            size_t const methodOffset = rt_->regsOffset() + sizeof(ORef) * methodVReg;
            as_.mov(methodReg, x86::Mem{rtReg, int32_t(methodOffset)});
            // Closure* const closure = allocClosure(rt, method, Fixnum{int64_t(cloverCount)});
            x86::Gp const countReg = x86::rdx; // ABI arg 3
            as_.movabs(countReg, Fixnum{int64_t(cloverCount)}.bits);
            as_.push(rtReg);
            as_.call(allocClosure);
            x86::Gp const closureReg = retReg;
            as_.pop(rtReg);

            {
                // ORef* clovers = const_cast<ORef*>(closure->clovers().data());
                x86::Gp const cloversReg = x86::r11;
                as_.lea(cloversReg, x86::Mem{closureReg, int32_t(Closure::flexOffset)});

                x86::Gp const vReg = x86::r10;
                size_t cloverIdx = 0;
                size_t regIdx = 0;
                for (uint8_t const byte :
                     std::span{bytecode.begin() + closesStartIdx, closesByteCount}
                ) {
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> bitIdx) & 1) {
                            // clovers[cloverIdx] = rt->regs[regIdx];
                            size_t const regOffset = rt_->regsOffset() + sizeof(ORef) * regIdx;
                            as_.mov(vReg, x86::Mem{rtReg, int32_t(regOffset)});
                            size_t const cloverOffset = sizeof(ORef) * cloverIdx;
                            as_.mov(x86::Mem{cloversReg, int32_t(cloverOffset)}, vReg);

                            ++cloverIdx;
                        }

                        ++regIdx;
                    }
                }
            }

            // rt->regs[destReg] = HRef{closure};
            x86::Gp const taggedClosureReg = x86::r11;
            tagging(taggedClosureReg, closureReg, heapedTag);
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, taggedClosureReg);
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

            // Closure const* const closure = &*HRef<Closure>::fromUnchecked(rt->regs[closureReg]);
            size_t const closureOffset = rt_->regsOffset() + sizeof(ORef) * closureVReg;
            as_.movabs(tmpReg, payloadMask);
            as_.and_(tmpReg, x86::Mem{rtReg, int32_t(closureOffset)});
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

            // Continuation* const cont = &*HRef<Continuation>::fromUnchecked(rt->regs[contReg]);
            size_t const contOffset = rt_->regsOffset() + sizeof(ORef) * contVReg;
            as_.movabs(tmpReg, payloadMask);
            as_.and_(tmpReg, x86::Mem{rtReg, int32_t(contOffset)});
            // rt->regs[destReg] = cont->saves()[cloverIdx];
            size_t const cloverOffset = Continuation::flexOffset + sizeof(ORef) * cloverIdxVReg;
            as_.mov(tmpReg, x86::Mem{tmpReg, int32_t(cloverOffset)});
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, tmpReg);
        }; break;

        case OP_CALL: {
            uint8_t const inlineCacheIdx = *it++;
            uint8_t const regCount  = *it++;
            uint8_t const savesByteCount = *it++;
            ptrdiff_t const savesStartIdx = std::distance(bytecode.begin(), it);
            size_t saveCount = 0;
            for (uint8_t const byte : std::span{bytecode.begin() + savesStartIdx, savesByteCount}) {
                saveCount += stdc_count_ones(byte);
            }
            it += savesByteCount;

            // HRef<Method> const callerMethod = HRef<Method>::fromUnchecked(rt->method);
            x86::Gp const methodReg = x86::rsi; // ABI arg 2
            as_.mov(methodReg, x86::Mem{rtReg, int32_t(rt_->methodOffset())});
            // Continuation* const cont = allocContinuation(
            //    rt, callerMethod, Fixnum{int64_t(rt->pc)}, Fixnum{int64_t(saveCount)}
            // );
            x86::Gp const retPcReg = x86::rdx; // ABI arg 3
            ptrdiff_t const retPc = std::distance(bytecode.begin(), it);
            it += sizeof(MethodCode);
            as_.movabs(retPcReg, Fixnum{int64_t(retPc)}.bits);
            x86::Gp const countReg = x86::rcx; // ABI arg 4
            as_.movabs(countReg, Fixnum{int64_t(saveCount)}.bits);
            as_.push(rtReg);
            as_.call(allocContinuation);
            x86::Gp const contReg = retReg;
            as_.pop(rtReg);

            { // TODO: DRY wrt. `OP_CLOSURE`:
                // ORef* spillSlots = const_cast<ORef*>(cont->saves().data());
                x86::Gp const spillsReg = x86::r11;
                as_.lea(spillsReg, x86::Mem{contReg, int32_t(Continuation::flexOffset)});

                x86::Gp const vReg = x86::r10;
                size_t spillIdx = 0;
                size_t regIdx = 0;
                for (uint8_t const byte :
                     std::span{bytecode.begin() + savesStartIdx, savesByteCount}
                ) {
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> bitIdx) & 1) {
                            // spillSlots[spillIdx] = rt->regs[regIdx];
                            size_t const regOffset = rt_->regsOffset() + sizeof(ORef) * regIdx;
                            as_.mov(vReg, x86::Mem{rtReg, int32_t(regOffset)});
                            size_t const cloverOffset = sizeof(ORef) * spillIdx;
                            as_.mov(x86::Mem{spillsReg, int32_t(cloverOffset)}, vReg);

                            ++spillIdx;
                        }

                        ++regIdx;
                    }
                }
            }

            // rt->regs[retContReg] = HRef{cont};
            x86::Gp const taggedContReg = x86::r11;
            tagging(taggedContReg, contReg, heapedTag);
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * retContReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, taggedContReg);

            Label const interpret = as_.new_anonymous_label("interpret");
            emitCall(inlineCacheIdx, regCount, interpret);

            Label const done = as_.new_anonymous_label("done");
            as_.jmp(done);

            as_.bind(interpret);
            interpreterFallback();

            as_.bind(done);
        }; break;

        case OP_TAILCALL: {
            uint8_t const inlineCacheIdx = *it++;
            uint8_t const regCount = *it++;

            Label const interpret = as_.new_anonymous_label("interpret");
            emitCall(inlineCacheIdx, regCount, interpret);

            as_.bind(interpret);
            interpreterFallback();
        }; break;

        case OP_FFICALL: {
            uint8_t const destVReg = *it++;
            uint8_t const codomainVReg = *it++;
            uint8_t const argc = *it++;
            uint8_t const unboxingsByteCount = *it++;
            bool const boxRet = bool(*it & 0b1);
            auto const unboxingsStartIdx = size_t(std::distance(bytecode.begin(), it));
            it += unboxingsByteCount;

            // ORef const anyCodomain = rt->regs[codomainReg];
            x86::Gp codomainReg = x86::r11;
            size_t const codomainOffset = rt_->regsOffset() + sizeof(ORef) * codomainVReg;
            as_.mov(codomainReg, x86::Mem{rtReg, int32_t(codomainOffset)});
            // if (!isa<Type>(*rt, anyCodomain)) goto interpret;
            // auto const codomain = HRef<Type>::fromUnchecked(anyCodomain);
            // Type* const codomainPtr = &*codomain;
            x86::Gp const codomainPtrReg = x86::r10;
            Label const interpret = as_.new_anonymous_label("interpret");
            checkedHeapedUntagging(codomainPtrReg, codomainReg, x86::r9,
                                   rt_->typeOffset(offsetof(NamedTypes, type)), interpret);

            // bool const fRet = eq(codomain, rt->types.flonum);
            x86::Gp const fRetReg = x86::rsi; // ABI arg 2
            size_t const flonumOffset = rt_->typeOffset(offsetof(NamedTypes, flonum));
            as_.cmp(codomainReg, x86::Mem{rtReg, int32_t(flonumOffset)});
            as_.sete(x86::al);
            as_.mov(fRetReg, x86::rax);

            // if (codomainPtr->isFlex.val()) goto interpret;
            as_.test(x86::Mem{codomainPtrReg, int32_t(offsetof(Type, isFlex))}, 1);
            as_.je(interpret);

            // if (size_t(codomainPtr->minSize.val()) > sizeof(uint64_t)) goto interpret;
            x86::Gp const tmpReg = x86::rax;
            as_.movabs(tmpReg, payloadMask);
            as_.and_(tmpReg, x86::Mem{codomainPtrReg, int32_t(offsetof(Type, minSize))});
            as_.cmp(tmpReg, sizeof(uint64_t));
            as_.jbe(interpret);

            // ORef const anyF = rt->regs[codomainVReg + 1];
            x86::Gp const fReg = x86::r10;
            x86::Gp const fPtrReg = x86::rdi; // ABI arg 1
            size_t const fOffset = rt_->regsOffset() + sizeof(ORef) * (codomainVReg + 1);
            as_.mov(fReg, x86::Mem{rtReg, int32_t(fOffset)});
            // if (!isa<Pointer>(*rt, anyF)) goto interpret;
            // Pointer* const fPtr = &*HRef<Pointer>::fromUnchecked(anyF);
            checkedHeapedUntagging(fPtrReg, fReg, tmpReg,
                                   rt_->typeOffset(offsetof(NamedTypes, pointer)), interpret);
            // void* const f = fPtr->val;
            as_.mov(fPtrReg, x86::Mem{fPtrReg, int32_t(offsetof(Pointer, val))});

            // uint8_t const* unboxings = rt->code + unboxingsStartIdx;
            x86::Gp const unboxingsReg = x86::rdx; // ABI arg 3
            as_.mov(unboxingsReg, x86::Mem{rtReg, int32_t(rt_->codeOffset())});
            as_.add(unboxingsReg, unboxingsStartIdx);
            // ORef* const args = &rt->regs[codomainVReg + 2];
            x86::Gp const argsReg = x86::rcx; // ABI arg 4
            size_t const argsOffset = rt_->regsOffset() + sizeof(ORef) * (codomainVReg + 2);
            as_.lea(argsReg, x86::Mem{rtReg, int32_t(argsOffset)});
            // auto const argc = size_t($argc);
            x86::Gp const argcReg = x86::r8; // ABI arg 5
            as_.mov(argcReg, argc);

            // uint64_t const rawRes = callForeign(f, fRet, unboxings, args, argc);
            as_.push(rtReg);
            as_.push(codomainReg);
            as_.push(fRetReg);
            as_.call(callForeign);
            x86::Gp resReg = retReg;
            as_.pop(fRetReg);
            codomainReg = x86::rsi; // ABI arg 2
            as_.pop(codomainReg);
            as_.pop(rtReg);

            // ORef res;

            // if (fRet) { res = Flonum{std::bit_cast<double>(rawRes)}; goto storeRes; }
            as_.test(fRetReg, fRetReg);
            Label const storeRes = as_.new_anonymous_label("storeRes");
            as_.jne(storeRes);

            if (!boxRet) {
                // res = tag(*rt, codomain, rawRes);
                as_.mov(x86::rdx, retReg); // ABI arg 3
                as_.push(rtReg);
                as_.call(tag);
                as_.pop(rtReg);
            } else {
                // Object* const obj = RT::alloc(rt, codomain);
                as_.push(resReg);
                as_.push(rtReg);
                as_.call(static_cast<RT::alloc_t>(RT::alloc));
                x86::Gp const objReg = retReg;
                as_.pop(rtReg);
                resReg = x86::r11;
                as_.pop(resReg);
                // *reinterpret_cast<uint64_t*>(obj) = rawRes;
                as_.mov(x86::Mem{objReg, 0}, resReg);
                // res = HRef{obj};
                tagging(resReg, objReg, heapedTag);
            }

            as_.bind(storeRes);
            // rt->regs[destReg] = res;
            size_t const destOffset = rt_->regsOffset() + sizeof(ORef) * destVReg;
            as_.mov(x86::Mem{rtReg, int32_t(destOffset)}, resReg);

            // rt->pc += instrSize;
            // `as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, instrSize);` was
            // storing an incorrect value for some reason :(:
            size_t const instrSize = 5 + unboxingsByteCount;
            as_.mov(tmpReg, instrSize);
            as_.add(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, tmpReg);
            Label const done = as_.new_anonymous_label("done");
            as_.jmp(done);

            as_.bind(interpret);
            interpreterFallback();

            as_.bind(done);
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
        Label const checkArgTypes = as_.new_anonymous_label("checkArgTypes");
        Label const onDomainError = as_.new_anonymous_label("onDomainError");
        Label const domainChecked = as_.new_anonymous_label("domainChecked");

        // HRef<Method> const method = rt->regs[calleeReg]->method;
        x86::Gp const methodReg = x86::rax;
        size_t const calleeOffset = rt_->regsOffset() + sizeof(ORef) * calleeReg;
        as_.movabs(methodReg, payloadMask);
        as_.and_(methodReg, x86::Mem{rtReg, int32_t(calleeOffset)});
        as_.mov(methodReg, x86::Mem{methodReg, offsetof(Closure, method)});
        // Method* const methodPtr = &*method;
        x86::Gp const methodPtrReg = x86::rcx;
        untagging(methodPtrReg, methodReg);

        // RT::DomainChecking const checking = rt->domainChecking;
        x86::Gp const checkingReg = x86::dl;
        as_.mov(checkingReg,
                x86::Mem{rtReg, int32_t(rt_->domainCheckingOffset()), sizeof(RT::DomainChecking)});
        // rt->domainChecking = RT::DomainChecking::CHECK;
        as_.mov(x86::Mem{rtReg, int32_t(rt_->domainCheckingOffset()), sizeof(RT::DomainChecking)},
                RT::DomainChecking::CHECK);

        // rt->domainChecking = checking;
        as_.mov(x86::Mem{rtReg,
                         int32_t(rt_->domainCheckingOffset()), sizeof(RT::DomainChecking)},
                checkingReg);
        // return PrimopRes::CALL_BYTECODE;
        as_.mov(retReg, PrimopRes::CALL_BYTECODE);
        as_.ret();

        // if (checking == RT::DomainChecking::SKIP) goto domainChecked;
        as_.cmp(checkingReg, RT::DomainChecking::SKIP);
        as_.je(domainChecked);

        // size_t const argc = state->entryRegc - firstArgReg;
        x86::Gp const argcReg = x86::rsi;
        as_.movzx(argcReg, x86::Mem{rtReg, int32_t(rt_->entryRegcOffset()), sizeof(RT::entryRegc)});
        as_.sub(argcReg, firstArgReg);
        auto const arity = size_t(method.flexCount().val());
        if (!method.hasVarArg.val()) {
            // if (argc != arity) goto onDomainError;
            as_.cmp(argcReg, uint8_t(arity));
            as_.jne(onDomainError);
        } else {
            // if (argc == arity) goto checkArgTypes;
            as_.cmp(argcReg, uint8_t(arity));
            as_.je(checkArgTypes);
            // TODO: Generate (non-punting) code for this:
            // rt->domainChecking = checking;
            as_.mov(x86::Mem{rtReg,
                             int32_t(rt_->domainCheckingOffset()), sizeof(RT::DomainChecking)},
                    checkingReg);
            // return PrimopRes::CALL_BYTECODE;
            as_.mov(retReg, PrimopRes::CALL_BYTECODE);
            as_.ret();
        }

        as_.bind(checkArgTypes);
        // TODO
        as_.jmp(domainChecked);

        as_.bind(onDomainError);
        // Domain check failed. Fall back to interpreter to face consequences (and do redundant
        // work, but realistically a logic error like this should end the entire process):
        // rt->domainChecking = checking;
        as_.mov(x86::Mem{rtReg, int32_t(rt_->domainCheckingOffset()), sizeof(RT::DomainChecking)},
                checkingReg);
        // return PrimopRes::CALL_BYTECODE;
        as_.mov(retReg, PrimopRes::CALL_BYTECODE);
        as_.ret();

        as_.bind(domainChecked);

        if (method.hasVarArg.val()) {
            // TODO: Generate (non-punting) code for vararg reification:
            // rt->domainChecking = RT::DomainChecking::SKIP;
            as_.mov(x86::Mem{rtReg, int32_t(rt_->domainCheckingOffset()),
                             sizeof(RT::DomainChecking)},
                    RT::DomainChecking::SKIP);
            // return PrimopRes::CALL_BYTECODE;
            as_.mov(retReg, PrimopRes::CALL_BYTECODE);
            as_.ret();
        }

        // rt->method = method;
        as_.mov(x86::Mem{rtReg, int32_t(rt_->methodOffset())}, methodReg);
        // rt->code = HRef<ByteArray>::fromUnchecked(method->code)->flexData();
        x86::Gp const codeReg = x86::rdx;
        as_.movabs(codeReg, payloadMask);
        as_.and_(codeReg, x86::Mem{methodPtrReg, offsetof(Method, code)});
        as_.mov(x86::Mem{rtReg, int32_t(rt_->codeOffset())}, codeReg);
        // rt->consts = HRef<ArrayMut>::fromUnchecked(method->consts)->itemsMut().data();
        x86::Gp const constsReg = x86::rdx;
        as_.movabs(constsReg, payloadMask);
        as_.and_(constsReg, x86::Mem{methodPtrReg, offsetof(Method, consts)});
        size_t const constsObjOffset = rt_->constsOffset();
        as_.mov(x86::Mem{rtReg, int32_t(constsObjOffset)}, constsReg);
        // OPTIMIZE: `SlotsMut<ORef>::slots_` seems redundant for `RT::consts`:
        size_t const constsSlotsOffset = constsObjOffset + SlotsMut<ORef>::slotsOffset;
        as_.mov(x86::Mem{rtReg, int32_t(constsSlotsOffset)}, constsReg);
        // rt->pc = Method::entryPc();
        // `as_.mov(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, Method::entryPc());` was
        // storing an incorrect value for some reason :(:
        x86::Gp const pcReg = x86::rdx;
        as_.mov(pcReg, Method::entryPc());
        as_.mov(x86::Mem{rtReg, int32_t(rt_->pcOffset())}, pcReg);

        naturalize(method, method.code->items());

        // FIXME: Patch `call` code `MethodCode`s
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
