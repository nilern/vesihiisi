#pragma once

#include "compiler.hpp"

namespace {

[[nodiscard]]
bool markMethodBuilder(RT* state, struct MethodBuilder* builder);
[[maybe_unused]]
void assertMethodBuilderInTospace(RT const* state, struct MethodBuilder const* builder);

HRef<Method> emitToplevelMethod(RT* state, Compiler* compiler, IRFn* fn);

} // namespace
