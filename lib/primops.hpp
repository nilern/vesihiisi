#pragma once

#include "object.hpp"
#include "state.hpp"

namespace {

ORef getErrorHandler(State const* state);

// Primops
// =================================================================================================

// Pseudo-Operation
PrimopRes callBytecode(State* state);

// Control Flow
PrimopRes primopAbort(State* state);
PrimopRes primopApplyArray(State* state);
PrimopRes primopApplyList(State* state);
PrimopRes primopCallCC(State* state);
PrimopRes primopContinue(State* state);

// For Any Value
PrimopRes primopIdentical(State* state);
PrimopRes primopTypeOf(State* state);

// Heap Objects
PrimopRes primopMakeSlotsType(State* state);
PrimopRes primopMake(State* state);
PrimopRes primopSlotGet(State* state);
PrimopRes primopSlotSet(State* state);
PrimopRes primopMakeFlex(State* state);
PrimopRes primopFlexCount(State* state);
PrimopRes primopFlexGet(State* state);
PrimopRes primopFlexSet(State* state);
PrimopRes primopFlexCopy(State* state);
PrimopRes primopFlexClone(State* state);

// Fixnums
PrimopRes primopFxAdd(State* state);
PrimopRes primopFxSub(State* state);
PrimopRes primopFxMul(State* state);
PrimopRes primopFxQuot(State* state);
PrimopRes primopFxLt(State* state);
PrimopRes primopFixnumToFlonum(State* state);

// Flonums
PrimopRes primopFlAdd(State* state);
PrimopRes primopFlSub(State* state);
PrimopRes primopFlMul(State* state);
PrimopRes primopFlDiv(State* state);

// Characters
PrimopRes primopCharLt(State* state);
PrimopRes primopCharToInteger(State* state);
PrimopRes primopCharIsAlphabetic(State* state);
PrimopRes primopCharIsNumeric(State* state);
PrimopRes primopCharIsWhitespace(State* state);

// Strings
PrimopRes primopArrayMutToString(State* state);
PrimopRes primopStringIteratorPeek(State* state);
PrimopRes primopStringIteratorNext(State* state);
PrimopRes primopStringToSymbol(State* state);

// I/O
PrimopRes primopOpenInputFile(State* state);
PrimopRes primopClosePort(State* state);
PrimopRes primopPeekChar(State* state);
PrimopRes primopReadChar(State* state);
PrimopRes primopWrite(State* state);
PrimopRes primopWriteChar(State* state);
PrimopRes primopWriteString(State* state);
PrimopRes primopFlushOutputPort(State* state);

// Time
PrimopRes primopCurrentSecond(State* state);
PrimopRes primopCurrentJiffy(State* state);
PrimopRes primopJiffiesPerSecond(State* state);

// Eval
PrimopRes primopEval(State* state);

// System Interface
PrimopRes primopExit(State* state);

} // namespace
