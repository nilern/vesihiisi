#pragma once

#include <stdint.h>

#include "compiler.h"

typedef struct { uint8_t index; } Reg;

static void regAllocFn(Compiler* compiler, IRFn* fn);
