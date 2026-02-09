#pragma once

#include <stdint.h>

#include "ir.hpp"

namespace {

struct Reg {
    uint8_t index;

    bool operator==(Reg that) const { return index == that.index; }
};

void regAllocFn(Compiler& compiler, IRFn& fn);

} // namespace
