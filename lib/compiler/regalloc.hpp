#pragma once

#include <stdint.h>

#include "ir.hpp"

namespace {

typedef struct { uint8_t index; } Reg;

void regAllocFn(Compiler* compiler, IRFn* fn);

} // namespace
