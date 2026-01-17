#pragma once

#include "object.hpp"
#include "state.hpp"

namespace {

ORef getErrorHandler(State const* state);

PrimopRes callBytecode(State* state);
PrimopRes primopAbort(State* state);
PrimopRes primopApplyArray(State* state);
PrimopRes primopApplyList(State* state);
PrimopRes primopCallCC(State* state);
PrimopRes primopContinue(State* state);
PrimopRes primopIdentical(State* state);
PrimopRes primopTypeOf(State* state);
PrimopRes primopMakeSlotsType(State* state);
PrimopRes primopMake(State* state);
PrimopRes primopSlotGet(State* state);
PrimopRes primopSlotSet(State* state);
PrimopRes primopMakeFlex(State* state);
PrimopRes primopFlexCount(State* state);
PrimopRes primopFlexGet(State* state);
PrimopRes primopFlexCopy(State* state);
PrimopRes primopFxAdd(State* state);
PrimopRes primopFxSub(State* state);
PrimopRes primopFxMul(State* state);
PrimopRes primopFxQuot(State* state);
PrimopRes primopFxLt(State* state);
PrimopRes primopFixnumToFlonum(State* state);
PrimopRes primopFlAdd(State* state);
PrimopRes primopFlSub(State* state);
PrimopRes primopFlMul(State* state);
PrimopRes primopFlDiv(State* state);
PrimopRes primopCharLt(State* state);
PrimopRes primopCharToInteger(State* state);
PrimopRes primopCharIsAlphabetic(State* state);
PrimopRes primopCharIsWhitespace(State* state);
PrimopRes primopStringIteratorNext(State* state);
PrimopRes primopWrite(State* state);
PrimopRes primopWriteChar(State* state);
PrimopRes primopWriteString(State* state);
PrimopRes primopCurrentSecond(State* state);
PrimopRes primopCurrentJiffy(State* state);
PrimopRes primopJiffiesPerSecond(State* state);
PrimopRes primopExit(State* state);

} // namespace
