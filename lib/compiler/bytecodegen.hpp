#pragma once

#include "compiler.hpp"

namespace {

[[nodiscard]]
bool markMethodBuilder(State* state, struct MethodBuilder* builder);
[[maybe_unused]]
void assertMethodBuilderInTospace(State const* state, struct MethodBuilder const* builder);

HRef<Method> emitToplevelMethod(State* state, Compiler* compiler, IRFn* fn);

} // namespace
