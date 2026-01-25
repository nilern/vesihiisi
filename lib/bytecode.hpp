#pragma once

#include <stdint.h>
#include <stdio.h>

#include "state.hpp"

namespace {

constexpr uint8_t bytecodeVarIntPayloadWidth = UINT8_WIDTH - 1;
constexpr uint8_t bytecodeVarIntPayloadMask = (1 << bytecodeVarIntPayloadWidth) - 1;
constexpr uint8_t bytecodeVarIntTerminalBit = 1 << bytecodeVarIntPayloadWidth;

// TODO: Bigger (only when necessary?) displacements for BR(F)?

// OPTIMIZE: Save cache (and memory in general) with:
// * 4-bit opcodes and arguments whenever possible
// * Encode bitmaps as (lastByte? bit{7})+ instead of count byte{count}
// * `rep` prefix (for e.g. mov, swap, clover)

// OPTIMIZE: `(tail)call-method` to allow direct calls to (lambda-lifted) local functions

typedef enum Opcode : uint8_t {
    OP_MOVE,        // rx = mov ry
    OP_SWAP,        // swap rx ry
    OP_DEF,         // def c rx
    OP_GLOBAL_SET,  // set! c rx
    OP_GLOBAL,      // rx = global c
    OP_CONST,       // rx = const c ; TODO: Encode as specialize with 0 types
    OP_SPECIALIZE,  // rx = specialize c #bi*
    OP_KNOT,        // rx = knot ()
    OP_KNOT_INIT,   // knot-init! rx ry
    OP_KNOT_GET,    // rx = knot-get ry
    OP_BRF,         // brf rx d
    OP_BR,          // br d
    OP_RET,         // ret
    OP_CLOSURE,     // rx = closure ry #bi* ; OPTIMIZE: fuse `specialize`(/`const`) into this
    OP_CLOVER,      // rx = clover rx i
    // TODO: OP_CONT_CLOVER / OP_RESTORE
    OP_CALL,        // call n #bi*
    OP_TAILCALL     // tailcall n TODO: Encode as call with 0 closes (nontail call always saves at
                    // least ret cont)?
} Opcode;

// Calling convention:
constexpr uint8_t calleeReg = 1;
constexpr uint8_t retContReg = 0;
constexpr uint8_t firstArgReg = 2;
constexpr uint8_t retReg = firstArgReg;

struct ZLoc {
    ORef maybeFilename;
    size_t srcByteIdx;

    void print(State const& state, FILE* dest) const;
};

void disassemble(State const* state, FILE* dest, HRef<Method> methodRef);

[[maybe_unused]]
void disassembleInstrAt(State const* state, FILE* dest, HRef<Method> methodRef, size_t pc);

Maybe<ZLoc> locatePc(HRef<Method> methodRef, size_t pc);

} // namespace
