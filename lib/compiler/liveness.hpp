#pragma once

#include "ir.hpp"

namespace {

/// Compute `IRFn` liveness, backwards (a post-order) through the blocks.
void enlivenFn(Compiler& compiler, IRFn& fn);

} // namespace
