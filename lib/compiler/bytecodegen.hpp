#pragma once

#include "compiler.hpp"

namespace {

HRef<Method> emitToplevelMethod(State* state, Compiler* compiler, IRFn* fn);

} // namespace
