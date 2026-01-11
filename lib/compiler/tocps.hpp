#pragma once

#include "compiler.hpp"

namespace {

using ToIRRes = Res<SyntaxErrors, IRFn>;

ToIRRes topLevelExprToIR(State const* state, Compiler* compiler, ORef expr, HRef<Loc> loc);

}
