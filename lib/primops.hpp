#pragma once

#include "value.hpp"
#include "rt.hpp"
#include "bytecode.hpp"

namespace {

ORef getErrorHandler(RT const* state);

[[nodiscard]]
PrimopRes primopError(RT* state, ORef err);
[[nodiscard]]
PrimopRes primopArityError(RT* state, HRef<Closure> callee, size_t argc);
[[nodiscard]]
PrimopRes primopTypeError(RT* state, HRef<Type> type, ORef v);
template<typename T>
[[nodiscard]]
PrimopRes primopTypeError(RT* state, ORef v) {
    return primopTypeError(state, state->reify<T>(), v);
}

// TODO: Move to dispatch.hpp:
template<bool hasVararg, typename... Domain>
[[nodiscard]]
PrimopRes checkDomain(RT* state) {
    if (state->domainChecking == RT::DomainChecking::SKIP) {
        state->domainChecking = RT::DomainChecking::CHECK;
        return PrimopRes::CONTINUE; // HACK
    }

    switch (state->domainChecking) {
    case RT::DomainChecking::CHECK: {
        size_t const argc = state->entryRegc - firstArgReg;
        static constexpr size_t arity = sizeof...(Domain);
        if (argc != arity) {
            if (!(hasVararg && argc >= arity - 1)) {
                assert(isa<Closure>(*state, state->regs[calleeReg]));
                auto const callee = HRef<Closure>::fromUnchecked(state->regs[calleeReg]);
                return primopArityError(state, callee, argc);
            }
        }

        ORef const* const args = state->regs + firstArgReg;
        HRef<Type> const domain[] = {state->reify<Domain>()...}; // OPTIMIZE: Get rid of this
        if constexpr (!hasVararg) {
            // TODO: `template for` (requires C++26):
            size_t i = 0;
            ((isa<Domain>(*state, args[i++])
                  ? void()
                  : ({ return primopTypeError(state, domain[i - 1], args[i - 1]); }))
             , ...);
        } else {
            static constexpr size_t minArity = arity - 1;

            { // TODO: `template for` (requires C++26):
                size_t i = 0;
                // HACK: Short-circuit `i` to avoid duplicate work and even buffer overflow:
                ((i >= minArity || isa<Domain>(*state, args[i++])
                      ? void()
                      : ({ return primopTypeError(state, domain[i - 1], args[i - 1]); }))
                 , ...);
            }

            using VarargType = // `Domain...[minArity]`:
                std::remove_reference_t<
                    decltype(std::get<minArity>(*(std::tuple<Domain...>*)(nullptr)))>;
            for (size_t i = minArity; i < argc; ++i) {
                ORef const v = args[i];
                if (!isa<VarargType>(*state, v)) {
                    return primopTypeError<VarargType>(state, v);
                }
            }
        }
    }; break;

    case RT::DomainChecking::SPECULATE: {
        state->domainChecking = RT::DomainChecking::CHECK;

        size_t const argc = state->entryRegc - firstArgReg;
        static constexpr size_t arity = sizeof...(Domain);
        if (argc != arity) {
            if (!(hasVararg && argc >= arity - 1)) {
                return PrimopRes::MISSPECULATION;
            }
        }

        ORef const* const args = state->regs + firstArgReg;
        if constexpr (!hasVararg) {
            // TODO: `template for` (requires C++26):
            size_t i = 0;
            ((isa<Domain>(*state, args[i++])
                  ? void()
                  : ({ return PrimopRes::MISSPECULATION; }))
             , ...);
        } else {
            static constexpr size_t minArity = arity - 1;

            { // TODO: `template for` (requires C++26):
                size_t i = 0;
                // HACK: Short-circuit `i` to avoid duplicate work and even buffer overflow:
                ((i >= minArity || isa<Domain>(*state, args[i++])
                      ? void()
                      : ({ return PrimopRes::MISSPECULATION; }))
                 , ...);
            }

            using VarargType = // `Domain...[minArity]`:
                std::remove_reference_t<
                    decltype(std::get<minArity>(*(std::tuple<Domain...>*)(nullptr)))>;
            for (size_t i = minArity; i < argc; ++i) {
                ORef const v = args[i];
                if (!isa<VarargType>(*state, v)) {
                    return PrimopRes::MISSPECULATION;
                }
            }
        }
    }; break;

    case RT::DomainChecking::SKIP: PANIC("Unreachable code reached.");
    }

    return PrimopRes::CONTINUE; // HACK
}

// Primops
// =================================================================================================

template<typename CRTPSub, typename... Domain>
struct Primop {
    static void install(RT& state) {
        installPrimop(&state, Str{CRTPSub::name, sizeof CRTPSub::name - 1},
                      static_cast<MethodCode>(invoke), CRTPSub::hasVararg,
                      Fixnum{int64_t(sizeof...(Domain))}, state.reify<Domain>()...);
    }

    static PrimopRes invoke(RT* state) {
        auto const checkRes = checkDomain<CRTPSub::hasVararg, Domain...>(state);
        switch (checkRes) {
        case PrimopRes::CONTINUE: { state->originalCallee = Default; }; break;

        case PrimopRes::MISSPECULATION: case PrimopRes::ERROR: return checkRes;

        case PrimopRes::INTERPRET: case PrimopRes::CALL_BYTECODE: case PrimopRes::TAILCALL:
        case PrimopRes::TAILAPPLY: case PrimopRes::ABORT: case PrimopRes::EXIT_VM:
            PANIC("Unreachable code reached.");
        }

        return CRTPSub::uncheckedInvoke(state);
    }
};

template<typename CRTPSub, typename... Domain>
struct FixedArityPrimop : public Primop<CRTPSub, Domain...> {
    static constexpr bool hasVararg = false;
};

template<typename CRTPSub, typename... Domain>
struct VarargsPrimop : public Primop<CRTPSub, Domain...> {
    static constexpr bool hasVararg = true;
};

// Pseudo-Operations
PrimopRes callBytecode(RT* rt);
PrimopRes interpret(RT* rt);
PrimopRes exitVMOnReturn(RT* rt);

// Control Flow
PrimopRes primopAbort(RT* state);
struct PrimopApplyArray : public FixedArityPrimop<PrimopApplyArray, ORef, Array> {
    static constexpr uint8_t name[] = "apply-array";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopApplyArrayMut : public FixedArityPrimop<PrimopApplyArrayMut, ORef, ArrayMut> {
    static constexpr uint8_t name[] = "apply-array!"; // TODO: `array!` -> `array-mut` (everywhere)
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopApplyList : public FixedArityPrimop<PrimopApplyList, ORef, ORef> {
    static constexpr uint8_t name[] = "apply-list";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopCallCC : public FixedArityPrimop<PrimopCallCC, Closure> {
    static constexpr uint8_t name[] = "call-with-current-continuation";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopContinue : public FixedArityPrimop<PrimopContinue, Continuation, ORef> {
    static constexpr uint8_t name[] = "continue";
    static PrimopRes uncheckedInvoke(RT* state);
};

// For Any Value
struct PrimopIdentical : public FixedArityPrimop<PrimopIdentical, ORef, ORef> {
    static constexpr uint8_t name[] = "identical?";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopTypeOf : public FixedArityPrimop<PrimopTypeOf, ORef> {
    static constexpr uint8_t name[] = "type-of";
    static PrimopRes uncheckedInvoke(RT* state);
};

// Heap Objects
struct PrimopMakeSlotsType : public FixedArityPrimop<PrimopMakeSlotsType, Symbol, Fixnum, Bool> {
    static constexpr uint8_t name[] = "make-slots-type";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopMake : public VarargsPrimop<PrimopMake, Type, ORef> {
    static constexpr uint8_t name[] = "make";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopSlotGet : public FixedArityPrimop<PrimopSlotGet, ORef, Fixnum> {
    static constexpr uint8_t name[] = "slot-get";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopSlotSet : public FixedArityPrimop<PrimopSlotSet, ORef, Fixnum, ORef> {
    static constexpr uint8_t name[] = "slot-set!";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopMakeFlex : public FixedArityPrimop<PrimopMakeFlex, Type, Fixnum> {
    static constexpr uint8_t name[] = "make-flex";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlexCount : public FixedArityPrimop<PrimopFlexCount, ORef> {
    static constexpr uint8_t name[] = "flex-count";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlexGet : public FixedArityPrimop<PrimopFlexGet, ORef, Fixnum> {
    static constexpr uint8_t name[] = "flex-get";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlexU8Get : public FixedArityPrimop<PrimopFlexU8Get, ORef, Fixnum> {
    static constexpr uint8_t name[] = "flex-u8-get";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlexSet : public FixedArityPrimop<PrimopFlexSet, ORef, Fixnum, ORef> {
    static constexpr uint8_t name[] = "flex-set!";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlexU8Set : public FixedArityPrimop<PrimopFlexU8Set, ORef, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "flex-u8-set!";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlexCopy
    : public FixedArityPrimop<PrimopFlexCopy, ORef, Fixnum, ORef, Fixnum, Fixnum>
{
    static constexpr uint8_t name[] = "flex-copy!";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlexClone : public FixedArityPrimop<PrimopFlexClone, ORef, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "flex-copy";
    static PrimopRes uncheckedInvoke(RT* state);
};

// Fixnums
struct PrimopFxAdd : public FixedArityPrimop<PrimopFxAdd, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx+";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxSub : public FixedArityPrimop<PrimopFxSub, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx-";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxMul : public FixedArityPrimop<PrimopFxMul, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx*";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxQuot : public FixedArityPrimop<PrimopFxQuot, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx-quot";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxRem : public FixedArityPrimop<PrimopFxRem, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx-rem";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxLt : public FixedArityPrimop<PrimopFxLt, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx<";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxAbs : public FixedArityPrimop<PrimopFxAbs, Fixnum> {
    static constexpr uint8_t name[] = "fx-abs";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxShl : public FixedArityPrimop<PrimopFxShl, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx<<";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxShr : public FixedArityPrimop<PrimopFxShr, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx>>";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxLshr : public FixedArityPrimop<PrimopFxLshr, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx>>>";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFxNlz : public FixedArityPrimop<PrimopFxNlz, Fixnum> {
    static constexpr uint8_t name[] = "fx-nlz";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFixnumToFlonum : public FixedArityPrimop<PrimopFixnumToFlonum, Fixnum> {
    static constexpr uint8_t name[] = "fixnum->flonum";
    static PrimopRes uncheckedInvoke(RT* state);
};

// Flonums
struct PrimopFlAdd : public FixedArityPrimop<PrimopFlAdd, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl+";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlSub : public FixedArityPrimop<PrimopFlSub, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl-";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlMul : public FixedArityPrimop<PrimopFlMul, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl*";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlDiv : public FixedArityPrimop<PrimopFlDiv, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl/";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlLt : public FixedArityPrimop<PrimopFlLt, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl<";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlGt : public FixedArityPrimop<PrimopFlGt, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl>";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlLeq : public FixedArityPrimop<PrimopFlLeq, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl<=";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlGeq : public FixedArityPrimop<PrimopFlGeq, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl>=";
    static PrimopRes uncheckedInvoke(RT* state);
};

// Characters
struct PrimopCharLt : public FixedArityPrimop<PrimopCharLt, Char, Char> {
    static constexpr uint8_t name[] = "char<";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopCharToInteger : public FixedArityPrimop<PrimopCharToInteger, Char> {
    static constexpr uint8_t name[] = "char->integer";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopCharIsAlphabetic : public FixedArityPrimop<PrimopCharIsAlphabetic, Char> {
    static constexpr uint8_t name[] = "char-alphabetic?";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopCharIsNumeric : public FixedArityPrimop<PrimopCharIsNumeric, Char> {
    static constexpr uint8_t name[] = "char-numeric?";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopCharIsWhitespace : public FixedArityPrimop<PrimopCharIsWhitespace, Char> {
    static constexpr uint8_t name[] = "char-whitespace?";
    static PrimopRes uncheckedInvoke(RT* state);
};

// Strings
struct PrimopArrayMutToString : public FixedArityPrimop<PrimopArrayMutToString, ArrayMut> {
    static constexpr uint8_t name[] = "array!->string";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopStringIteratorPeek
    : public FixedArityPrimop<PrimopStringIteratorPeek, StringIterator>
{
    static constexpr uint8_t name[] = "string-iterator-peek";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopStringIteratorNext
    : public FixedArityPrimop<PrimopStringIteratorNext, StringIterator>
{
    static constexpr uint8_t name[] = "string-iterator-next!";
    static PrimopRes uncheckedInvoke(RT* state);
};

// Symbols
struct PrimopStringToSymbol : public FixedArityPrimop<PrimopStringToSymbol, String> {
    static constexpr uint8_t name[] = "string->symbol";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopGensym : public FixedArityPrimop<PrimopGensym> {
    static constexpr uint8_t name[] = "gensym";
    static PrimopRes uncheckedInvoke(RT* rt);
};

// I/O
struct PrimopFileExists : public FixedArityPrimop<PrimopFileExists, String> {
    static constexpr uint8_t name[] = "file-exists?";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopOpenInputFile : public FixedArityPrimop<PrimopOpenInputFile, String> {
    static constexpr uint8_t name[] = "open-input-file";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopClosePort : public FixedArityPrimop<PrimopClosePort, InputFile> {
    static constexpr uint8_t name[] = "close-port";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopPeekChar : public FixedArityPrimop<PrimopPeekChar, InputFile> {
    static constexpr uint8_t name[] = "peek-char";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopReadChar : public FixedArityPrimop<PrimopReadChar, InputFile> {
    static constexpr uint8_t name[] = "read-char";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopWrite : public FixedArityPrimop<PrimopWrite, ORef> {
    static constexpr uint8_t name[] = "write";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopWriteChar : public FixedArityPrimop<PrimopWriteChar, Char> {
    static constexpr uint8_t name[] = "write-char";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopWriteString : public FixedArityPrimop<PrimopWriteString, String> {
    static constexpr uint8_t name[] = "write-string";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopFlushOutputPort : public FixedArityPrimop<PrimopFlushOutputPort> {
    static constexpr uint8_t name[] = "flush-output-port";
    static PrimopRes uncheckedInvoke(RT* state);
};

// Time
struct PrimopCurrentSecond : public FixedArityPrimop<PrimopCurrentSecond> {
    static constexpr uint8_t name[] = "current-second";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopCurrentJiffy : public FixedArityPrimop<PrimopCurrentJiffy> {
    static constexpr uint8_t name[] = "current-jiffy";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopJiffiesPerSecond : public FixedArityPrimop<PrimopJiffiesPerSecond> {
    static constexpr uint8_t name[] = "jiffies-per-second";
    static PrimopRes uncheckedInvoke(RT* state);
};

// Reflection
struct PrimopResolve : public FixedArityPrimop<PrimopResolve, Symbol> {
    static constexpr uint8_t name[] = "resolve";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopEval : public FixedArityPrimop<PrimopEval, ORef, Loc, Bool> {
    static constexpr uint8_t name[] = "eval";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopJITCompile : public FixedArityPrimop<PrimopJITCompile, Closure> {
    static constexpr uint8_t name[] = "jit-compile!";
    static PrimopRes uncheckedInvoke(RT* state);
};
struct PrimopContinuationCallLoc
    : public FixedArityPrimop<PrimopContinuationCallLoc, Continuation>
{
    static constexpr uint8_t name[] = "continuation-call-loc";
    static PrimopRes uncheckedInvoke(RT* state);
};

// System Interface
struct PrimopExit : public FixedArityPrimop<PrimopExit, ORef> {
    static constexpr uint8_t name[] = "exit";
    static PrimopRes uncheckedInvoke(RT* state);
};

// TODO: A proper dylib type instead of `Pointer`
struct PrimopOpenForeignLibrary : public FixedArityPrimop<PrimopOpenForeignLibrary, ORef> {
    static constexpr uint8_t name[] = "open-foreign-library";
    static PrimopRes uncheckedInvoke(RT* state);
};

// TODO: A proper dylib type instead of `Pointer`
struct PrimopCloseForeignLibrary : public FixedArityPrimop<PrimopCloseForeignLibrary, Pointer> {
    static constexpr uint8_t name[] = "close-foreign-library";
    static PrimopRes uncheckedInvoke(RT* state);
};

// TODO: A proper dylib type instead of `Pointer`
struct PrimopGetForeign : public FixedArityPrimop<PrimopGetForeign, Pointer, String> {
    static constexpr uint8_t name[] = "get-foreign";
    static PrimopRes uncheckedInvoke(RT* state);
};

} // namespace
