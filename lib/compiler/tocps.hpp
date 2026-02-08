#pragma once

#include "compiler.hpp"

namespace {

using ToIRRes = Res<Vshs_SyntaxErrors, IRFn>;

ToIRRes topLevelExprToIR(RT const* state, Compiler* compiler, ORef expr, HRef<Loc> loc);

}
