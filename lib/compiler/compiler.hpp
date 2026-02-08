#pragma once

#include "../rt.hpp"

namespace {

// TODO: Compiler linter a la GHC (in addition to bytecode verifier!)

using CompilationRes = Res<Vshs_SyntaxErrors, HRef<Method>>;

CompilationRes compile(RT* state, ORef expr, HRef<Loc> loc, bool debug);

} // namespace
