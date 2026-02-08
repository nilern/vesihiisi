#pragma once

#include "compiler.hpp"

namespace {

using ToIRRes = Res<SyntaxErrors, IRFn>;

ToIRRes topLevelExprToIR(RT const* state, Compiler* compiler, ORef expr, HRef<Loc> loc);

}
