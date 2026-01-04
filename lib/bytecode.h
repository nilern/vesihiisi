#pragma once

#include <stdint.h>
#include <stdio.h>

#include "state.h"

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
    // TODO: OP_GLOBAL_SET
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
static const uint8_t calleeReg;
static const uint8_t retContReg;
static const uint8_t firstArgReg;
static const uint8_t retReg;

static void disassemble(State const* state, FILE* dest, MethodRef methodRef);
