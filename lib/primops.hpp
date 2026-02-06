#pragma once

#include "object.hpp"
#include "state.hpp"
#include "bytecode.hpp"

namespace {

ORef getErrorHandler(State const* state);

[[nodiscard]]
PrimopRes primopError(State* state, ORef err);
[[nodiscard]]
PrimopRes primopArityError(State* state, HRef<Closure> callee, size_t argc);
[[nodiscard]]
PrimopRes primopTypeError(State* state, HRef<Type> type, ORef v);
template<typename T>
[[nodiscard]]
PrimopRes primopTypeError(State* state, ORef v) {
    return primopTypeError(state, state->reify<T>(), v);
}

// TODO: Move to dispatch.hpp:
template<bool hasVararg, typename... Domain>
[[nodiscard]]
PrimopRes checkDomain(State* state) {
    if (state->domainChecking == State::DomainChecking::SKIP) {
        state->domainChecking = State::DomainChecking::CHECK;
        return PrimopRes::CONTINUE; // HACK
    }

    switch (state->domainChecking) {
    case State::DomainChecking::CHECK: {
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

    case State::DomainChecking::SPECULATE: {
        state->domainChecking = State::DomainChecking::CHECK;

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

    case State::DomainChecking::SKIP: PANIC("Unreachable code reached.");
    }

    return PrimopRes::CONTINUE; // HACK
}

// Primops
// =================================================================================================

template<typename CRTPSub, typename... Domain>
struct Primop {
    static void install(State& state) {
        installPrimop(&state, Str{CRTPSub::name, sizeof CRTPSub::name - 1},
                      static_cast<MethodCode>(invoke), CRTPSub::hasVararg,
                      Fixnum{int64_t(sizeof...(Domain))}, state.reify<Domain>()...);
    }

    static PrimopRes invoke(State* state) {
        auto const checkRes = checkDomain<CRTPSub::hasVararg, Domain...>(state);
        switch (checkRes) {
        case PrimopRes::CONTINUE: break;
        case PrimopRes::MISSPECULATION: case PrimopRes::ERROR: return checkRes;
        case PrimopRes::TAILCALL: case PrimopRes::TAILAPPLY: case PrimopRes::ABORT:
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

// Pseudo-Operation
PrimopRes callBytecode(State* state);

// Control Flow
PrimopRes primopAbort(State* state);
struct PrimopApplyArray : public FixedArityPrimop<PrimopApplyArray, ORef, Array> {
    static constexpr uint8_t name[] = "apply-array";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopApplyArrayMut : public FixedArityPrimop<PrimopApplyArrayMut, ORef, ArrayMut> {
    static constexpr uint8_t name[] = "apply-array!"; // TODO: `array!` -> `array-mut` (everywhere)
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopApplyList : public FixedArityPrimop<PrimopApplyList, ORef, ORef> {
    static constexpr uint8_t name[] = "apply-list";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopCallCC : public FixedArityPrimop<PrimopCallCC, Closure> {
    static constexpr uint8_t name[] = "call-with-current-continuation";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopContinue : public FixedArityPrimop<PrimopContinue, Continuation, ORef> {
    static constexpr uint8_t name[] = "continue";
    static PrimopRes uncheckedInvoke(State* state);
};

// For Any Value
struct PrimopIdentical : public FixedArityPrimop<PrimopIdentical, ORef, ORef> {
    static constexpr uint8_t name[] = "identical?";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopTypeOf : public FixedArityPrimop<PrimopTypeOf, ORef> {
    static constexpr uint8_t name[] = "type-of";
    static PrimopRes uncheckedInvoke(State* state);
};

// Heap Objects
struct PrimopMakeSlotsType : public FixedArityPrimop<PrimopMakeSlotsType, Symbol, Fixnum, Bool> {
    static constexpr uint8_t name[] = "make-slots-type";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopMake : public VarargsPrimop<PrimopMake, Type, ORef> {
    static constexpr uint8_t name[] = "make";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopSlotGet : public FixedArityPrimop<PrimopSlotGet, ORef, Fixnum> {
    static constexpr uint8_t name[] = "slot-get";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopSlotSet : public FixedArityPrimop<PrimopSlotSet, ORef, Fixnum, ORef> {
    static constexpr uint8_t name[] = "slot-set!";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopMakeFlex : public FixedArityPrimop<PrimopMakeFlex, Type, Fixnum> {
    static constexpr uint8_t name[] = "make-flex";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlexCount : public FixedArityPrimop<PrimopFlexCount, ORef> {
    static constexpr uint8_t name[] = "flex-count";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlexGet : public FixedArityPrimop<PrimopFlexGet, ORef, Fixnum> {
    static constexpr uint8_t name[] = "flex-get";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlexSet : public FixedArityPrimop<PrimopFlexSet, ORef, Fixnum, ORef> {
    static constexpr uint8_t name[] = "flex-set!";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlexCopy
    : public FixedArityPrimop<PrimopFlexCopy, ORef, Fixnum, ORef, Fixnum, Fixnum>
{
    static constexpr uint8_t name[] = "flex-copy!";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlexClone : public FixedArityPrimop<PrimopFlexClone, ORef, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "flex-copy";
    static PrimopRes uncheckedInvoke(State* state);
};

// Fixnums
struct PrimopFxAdd : public FixedArityPrimop<PrimopFxAdd, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx+";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFxSub : public FixedArityPrimop<PrimopFxSub, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx-";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFxMul : public FixedArityPrimop<PrimopFxMul, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx*";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFxQuot : public FixedArityPrimop<PrimopFxQuot, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx-quot";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFxLt : public FixedArityPrimop<PrimopFxLt, Fixnum, Fixnum> {
    static constexpr uint8_t name[] = "fx<";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFixnumToFlonum : public FixedArityPrimop<PrimopFixnumToFlonum, Fixnum> {
    static constexpr uint8_t name[] = "fixnum->flonum";
    static PrimopRes uncheckedInvoke(State* state);
};

// Flonums
struct PrimopFlAdd : public FixedArityPrimop<PrimopFlAdd, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl+";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlSub : public FixedArityPrimop<PrimopFlSub, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl-";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlMul : public FixedArityPrimop<PrimopFlMul, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl*";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlDiv : public FixedArityPrimop<PrimopFlDiv, Flonum, Flonum> {
    static constexpr uint8_t name[] = "fl/";
    static PrimopRes uncheckedInvoke(State* state);
};

// Characters
struct PrimopCharLt : public FixedArityPrimop<PrimopCharLt, Char, Char> {
    static constexpr uint8_t name[] = "char<";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopCharToInteger : public FixedArityPrimop<PrimopCharToInteger, Char> {
    static constexpr uint8_t name[] = "char->integer";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopCharIsAlphabetic : public FixedArityPrimop<PrimopCharIsAlphabetic, Char> {
    static constexpr uint8_t name[] = "char-alphabetic?";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopCharIsNumeric : public FixedArityPrimop<PrimopCharIsNumeric, Char> {
    static constexpr uint8_t name[] = "char-numeric?";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopCharIsWhitespace : public FixedArityPrimop<PrimopCharIsWhitespace, Char> {
    static constexpr uint8_t name[] = "char-whitespace?";
    static PrimopRes uncheckedInvoke(State* state);
};

// Strings
struct PrimopArrayMutToString : public FixedArityPrimop<PrimopArrayMutToString, ArrayMut> {
    static constexpr uint8_t name[] = "array!->string";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopStringIteratorPeek
    : public FixedArityPrimop<PrimopStringIteratorPeek, StringIterator>
{
    static constexpr uint8_t name[] = "string-iterator-peek";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopStringIteratorNext
    : public FixedArityPrimop<PrimopStringIteratorNext, StringIterator>
{
    static constexpr uint8_t name[] = "string-iterator-next!";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopStringToSymbol : public FixedArityPrimop<PrimopStringToSymbol, String> {
    static constexpr uint8_t name[] = "string->symbol";
    static PrimopRes uncheckedInvoke(State* state);
};

// I/O
struct PrimopFileExists : public FixedArityPrimop<PrimopFileExists, String> {
    static constexpr uint8_t name[] = "file-exists?";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopOpenInputFile : public FixedArityPrimop<PrimopOpenInputFile, String> {
    static constexpr uint8_t name[] = "open-input-file";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopClosePort : public FixedArityPrimop<PrimopClosePort, InputFile> {
    static constexpr uint8_t name[] = "close-port";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopPeekChar : public FixedArityPrimop<PrimopPeekChar, InputFile> {
    static constexpr uint8_t name[] = "peek-char";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopReadChar : public FixedArityPrimop<PrimopReadChar, InputFile> {
    static constexpr uint8_t name[] = "read-char";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopWrite : public FixedArityPrimop<PrimopWrite, ORef> {
    static constexpr uint8_t name[] = "write";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopWriteChar : public FixedArityPrimop<PrimopWriteChar, Char> {
    static constexpr uint8_t name[] = "write-char";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopWriteString : public FixedArityPrimop<PrimopWriteString, String> {
    static constexpr uint8_t name[] = "write-string";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopFlushOutputPort : public FixedArityPrimop<PrimopFlushOutputPort> {
    static constexpr uint8_t name[] = "flush-output-port";
    static PrimopRes uncheckedInvoke(State* state);
};

// Time
struct PrimopCurrentSecond : public FixedArityPrimop<PrimopCurrentSecond> {
    static constexpr uint8_t name[] = "current-second";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopCurrentJiffy : public FixedArityPrimop<PrimopCurrentJiffy> {
    static constexpr uint8_t name[] = "current-jiffy";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopJiffiesPerSecond : public FixedArityPrimop<PrimopJiffiesPerSecond> {
    static constexpr uint8_t name[] = "jiffies-per-second";
    static PrimopRes uncheckedInvoke(State* state);
};

// Reflection
struct PrimopResolve : public FixedArityPrimop<PrimopResolve, Symbol> {
    static constexpr uint8_t name[] = "resolve";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopEval : public FixedArityPrimop<PrimopEval, ORef, Loc, Bool> {
    static constexpr uint8_t name[] = "eval";
    static PrimopRes uncheckedInvoke(State* state);
};
struct PrimopContinuationCallLoc
    : public FixedArityPrimop<PrimopContinuationCallLoc, Continuation>
{
    static constexpr uint8_t name[] = "continuation-call-loc";
    static PrimopRes uncheckedInvoke(State* state);
};

// System Interface
struct PrimopExit : public FixedArityPrimop<PrimopExit, ORef> {
    static constexpr uint8_t name[] = "exit";
    static PrimopRes uncheckedInvoke(State* state);
};

} // namespace
