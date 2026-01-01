#pragma once

#include "compiler.h"

/// Compute `IRFn` liveness, backwards (a post-order) through the blocks.
static void enlivenFn(Compiler* compiler, IRFn* fn);
