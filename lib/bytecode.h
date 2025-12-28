#pragma once

#include <stdint.h>

// TODO: Bigger (only when necessary?) displacements for BR(F)?

// OPTIMIZE: 4-bit opcode with one 4-bit argument when possible?

typedef enum Opcode : uint8_t {
    OP_MOVE,
    OP_SWAP,
    OP_DEF,
    // TODO: OP_GLOBAL_SET
    OP_GLOBAL,
    OP_CONST,
    OP_SPECIALIZE,
    OP_KNOT,        // rx = knot ()
    OP_KNOT_INIT,   // knot-init! rx ry
    OP_KNOT_GET,    // rx = knot-get ry
    OP_BRF,
    OP_BR,
    OP_RET,
    OP_CLOSURE,
    OP_CLOVER,
    // TODO: OP_CONT_CLOVER / OP_RESTORE
    OP_CALL,
    OP_TAILCALL // TODO: Encode as call with 0 closes (nontail call always saves at least ret cont)?
} Opcode;

// Calling convention:
static const uint8_t calleeReg;
static const uint8_t retContReg;
static const uint8_t firstArgReg;
static const uint8_t retReg;
