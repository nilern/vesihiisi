#include "tocps.hpp"

#include <string.h>

#include "../rt.hpp"
#include "../util/asmallmap.hpp"

namespace {

// Env
// =================================================================================================

class ToCpsEnv {
public:
    struct Def {
        union {
            IRName name;
            IRName knotName;
        };
        enum {
            NAME,
            KNOT
        } type;
    };

private:
    ASmallMap<HRef<Symbol>, Def> bindings_;
    ToCpsEnv const* parent_;

public:
    ToCpsEnv(Arena* arena, ToCpsEnv const* parent) : bindings_{arena}, parent_{parent} {}

    std::optional<Def> useSymbolDef(HRef<Symbol> sym) const {
        for (ToCpsEnv const* env = this; env; env = env->parent_) {
            auto const optDef = env->bindings_.tryGet(sym);
            if (optDef) {
                return optDef;
            }
        }

        return std::nullopt;
    }

    enum BindingsType { BINDINGS_PAR, BINDINGS_SEQ };

    void setSymbolDef(HRef<Symbol> sym, Def def, BindingsType type) {
        // FIXME: Proper error (duplicate defs):
        if (type == BINDINGS_PAR) {
            assert(!bindings_.tryGet(sym));
        }

        bindings_.set(sym, def);
    }
};

// Static Continuations
// =================================================================================================

struct ToCpsCont {
    struct Return {
        IRName cont;
    };

    struct Def {
        IRName name;
        HRef<Symbol> sym;
    };

    union {
        Return ret;
        Def def;
    };
    enum {
        EFF,
        VAL,
        BIND,
        DEF,
        SET,
        JOIN,
        RETURN
    } type;

    IRName destName(Compiler& compiler) const {
        switch (type) {
        case ToCpsCont::BIND: // fallthrough
        case ToCpsCont::DEF: case ToCpsCont::SET: return def.name;

        case ToCpsCont::EFF: // fallthrough
        case ToCpsCont::VAL: // fallthrough
        case ToCpsCont::JOIN: // fallthrough
        case ToCpsCont::RETURN: return freshName(&compiler);
        }

        return invalidIRName; // Unreachable
    }

    ORef destSymbol() const {
        switch (type) {
        case ToCpsCont::BIND: // fallthrough
        case ToCpsCont::DEF: case ToCpsCont::SET: return def.sym;

        case ToCpsCont::EFF: // fallthrough
        case ToCpsCont::VAL: // fallthrough
        case ToCpsCont::JOIN: // fallthrough
        case ToCpsCont::RETURN: return Default;
        }

        return Default; // Unreachable
    }
};

// Conversion from S-expressions to CPS IR
// =================================================================================================

struct CPSConv {
    RT const* state;
    Compiler* compiler;
private:
    AVec<Vshs_SyntaxError> errs;

public:
    CPSConv(RT const& t_state, Compiler& t_compiler) :
        state{&t_state}, compiler{&t_compiler}, errs{&compiler->arena} {}

    void error(Vshs_SyntaxError err) { errs.push(err); }

    Slice<Vshs_SyntaxError const> errors() const { return errs.slice(); }
};

IRName constToCPS(CPSConv& pass, IRBlock& block, ORef expr, ORef maybeLoc, ToCpsCont k) {
    IRName const name = k.destName(*pass.compiler);
    block.stmts.push(IRStmt{ConstDef{name, expr}, maybeLoc});

    if (k.type == ToCpsCont::RETURN) {
        block.createReturn(k.ret.cont, name, maybeLoc);
    }

    return name;
}

IRName globalToCPS(CPSConv& pass, IRBlock& block, HRef<Symbol> sym, ORef maybeLoc, ToCpsCont k) {
    IRName const name = k.destName(*pass.compiler);
    block.stmts.push(IRStmt{IRGlobal{name, sym}, maybeLoc});

    if (k.type == ToCpsCont::RETURN) {
        block.createReturn(k.ret.cont, name, maybeLoc);
    }

    return name;
}

IRName exprToIR(CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef expr,
                ORef maybeLoc, ToCpsCont k);

IRName bodyToCPS(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef body, ToCpsCont k
) {
    if (!isa<Pair>(*pass.state, body)) {
        assert(false); // TODO: Proper empty/improper body error
    }
    auto argsPair = HRef<Pair>::fromUnchecked(body);

    for (;/*ever*/;) {
        ORef const stmt = argsPair->car().get();
        body = argsPair->cdr().get();
        ORef const maybeLoc = argsPair->maybeLoc().get();

        if (isEmptyList(pass.state, body)) {
            IRName const bodyName = exprToIR(pass, fn, env, block, stmt, maybeLoc, k);

            return bodyName;
        } else if (isa<Pair>(*pass.state, body)){
            exprToIR(pass, fn, env, block, stmt, maybeLoc, ToCpsCont{{}, ToCpsCont::EFF});

            argsPair = HRef<Pair>::fromUnchecked(body);
        } else {
            assert(false); // TODO: Proper improper args error
        }
    }
}

[[nodiscard]]
bool paramToCPS(
    CPSConv& pass, IRFn& outerFn, ToCpsEnv const& outerEnv, IRBlock*& outerBlock, IRFn& fn,
    ToCpsEnv& fnEnv, IRBlock& entryBlock, size_t idx, ORef param
    ) {
    if (isa<Symbol>(*pass.state, param)) {
        HRef<Symbol> const paramSym = HRef<Symbol>::fromUnchecked(param);

        IRName const paramName = renameSymbol(pass.compiler, paramSym);
        entryBlock.params.push(paramName);
        fnEnv.setSymbolDef(paramSym, ToCpsEnv::Def{.name = paramName, .type = ToCpsEnv::Def::NAME},
                           ToCpsEnv::BINDINGS_PAR);

        return true;
    } else if (isa<Pair>(*pass.state, param)) {
        auto const paramPair = HRef<Pair>::fromUnchecked(param);

        ORef const op = paramPair->car().get();
        if (!eq(op, pass.state->singletons.ofType)) { return false; }

        ORef anyArgs = paramPair->cdr().get();
        if (!isa<Pair>(*pass.state, anyArgs)) { return false; }
        auto args = HRef<Pair>::fromUnchecked(anyArgs);

        ORef const maybeSym = args->car().get();
        if (!isa<Symbol>(*pass.state, maybeSym)) { return false; }
        HRef<Symbol> const sym = HRef<Symbol>::fromUnchecked(maybeSym);

        anyArgs = args->cdr().get();
        if (!isa<Pair>(*pass.state, anyArgs)) { return false; }
        args = HRef<Pair>::fromUnchecked(anyArgs);

        ORef const type = args->car().get();

        if (!isEmptyList(pass.state, args->cdr().get())) { return false; }

        IRName const typeName = exprToIR(pass, outerFn, outerEnv, outerBlock, type,
                                         args->maybeLoc().get(), ToCpsCont{{}, ToCpsCont::VAL});

        // TODO: DRY with symbol branch above:
        IRName const paramName = renameSymbol(pass.compiler, sym);
        entryBlock.params.push(paramName);
        fnEnv.setSymbolDef(sym, ToCpsEnv::Def{.name = paramName, .type = ToCpsEnv::Def::NAME},
                     ToCpsEnv::BINDINGS_PAR);
        setParamType(pass.compiler, &fn.domain, idx, typeName);

        return true;
    } else {
        return false;
    }
}

IRName fnToCPSimpl(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, IRName maybeSelf, ORef params,
    ORef body, ORef maybeLoc, ToCpsCont k
) {
    ORef const maybeName = k.destSymbol();
    auto innerFn = IRFn{&pass.compiler->arena, maybeName};

    IRBlock* entryBlock = innerFn.createBlock(0);

    auto fnEnv = ToCpsEnv{&pass.compiler->arena, &env};
    IRName const self = maybeSelf.isValid() ? maybeSelf : freshName(pass.compiler);
    entryBlock->params.push(self);
    IRName const ret = freshName(pass.compiler);
    entryBlock->params.push(ret);

    size_t arity = 0;

    while (!isEmptyList(pass.state, params)) {
        // TODO: Is this just bad syntax design?:
        // Has to be first because `(x y . (: zs <t>))` = `(x y : zs <t>)`:
        if (paramToCPS(pass, fn, env, block, innerFn, fnEnv, *entryBlock, arity, params)) {
            innerFn.hasVarArg = true;

            ++arity;
            break;
        }

        if (isa<Pair>(*pass.state, params)) {
            auto const paramsPair = HRef<Pair>::fromUnchecked(params);

            if (!paramToCPS(pass, fn, env, block, innerFn, fnEnv, *entryBlock, arity,
                            paramsPair->car().get())
            ) {
                pass.error({paramsPair->maybeLoc().get(), INVALID_PARAM});
            }
            params = paramsPair->cdr().get();

            ++arity;
            continue;
        }

        assert(false); // TODO: Proper invalid vararg error
    }

    completeIRDomain(pass.compiler, &innerFn.domain, arity);

    ToCpsCont const retK = {{.ret = {.cont = ret}}, ToCpsCont::RETURN};
    // Body is in tail position so discard the returned `IRName`:
    bodyToCPS(pass, innerFn, fnEnv, entryBlock, body, retK);

    IRName const name = k.destName(*pass.compiler);
    auto const closes =
        static_cast<AVec<IRName>*>(amalloc(&pass.compiler->arena, sizeof(AVec<IRName>)));
    new (closes) AVec<IRName>{&pass.compiler->arena};
    block->stmts.push(IRStmt{MethodDef{name, std::move(innerFn), closes}, maybeLoc});

    if (k.type == ToCpsCont::RETURN) {
        block->createReturn(k.ret.cont, name, maybeLoc);
    }

    return name;
}

IRName fnToCPS(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef args, ORef maybeLoc,
    ToCpsCont k
) {
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO: Proper args error (`(fn)`)
    }
    auto const argsPair = HRef<Pair>::fromUnchecked(args);

    ORef const params = argsPair->car().get();
    ORef const body = argsPair->cdr().get();
    return fnToCPSimpl(pass, fn, env, block, invalidIRName, params, body, maybeLoc, k);
}

IRName ifToCPS(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef args, ORef maybeLoc,
    ToCpsCont k
) {
    // OPTIMIZE: Avoid creating `goto`s to `goto`s:

    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO
    }
    auto argsPair = HRef<Pair>::fromUnchecked(args);

    ORef const cond = argsPair->car().get();
    ORef const condLoc = argsPair->maybeLoc().get();
    args = argsPair->cdr().get();
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args);

    ORef const conseq = argsPair->car().get();
    ORef const conseqLoc = argsPair->maybeLoc().get();
    args = argsPair->cdr().get();
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args);

    ORef const alt = argsPair->car().get();
    ORef const altLoc = argsPair->maybeLoc().get();
    if (!isEmptyList(pass.state, argsPair->cdr().get())) {
        assert(false); // TODO
    }

    ToCpsCont const splitK = ToCpsCont{{}, ToCpsCont::VAL};
    IRName const condName = exprToIR(pass, fn, env, block, cond, condLoc, splitK);
        // Will patch targets shortly:
    IRIf* ifTransfer = block->createIf(condName, IRLabel{}, IRLabel{}, maybeLoc);
    IRLabel const ifLabel = block->label;
    ToCpsCont const joinK = k.type != ToCpsCont::RETURN
        ? ToCpsCont{{}, ToCpsCont::JOIN}
        : k;

    IRBlock* conseqBlock = fn.createBlock(1);
    conseqBlock->callers.push(ifLabel);
    ifTransfer->conseq = conseqBlock->label;
    IRName const conseqName = exprToIR(pass, fn, env, conseqBlock, conseq, conseqLoc, joinK);

    IRBlock* altBlock = fn.createBlock(1);
    altBlock->callers.push(ifLabel);
    ifTransfer->alt = altBlock->label;
    IRName const altName = exprToIR(pass, fn, env, altBlock, alt, altLoc, joinK);

    if (k.type != ToCpsCont::RETURN) {
        // FIXME: If we avoid `goto`s to `goto`s, 2 might not suffice:
        IRBlock* const joinBlock = fn.createBlock(2);
        IRName const phi = k.destName(*pass.compiler);
        joinBlock->params.push(phi);

        conseqBlock->createGoto(&pass.compiler->arena, joinBlock->label, conseqName, conseqLoc);
        joinBlock->callers.push(conseqBlock->label);

        altBlock->createGoto(&pass.compiler->arena, joinBlock->label, altName, altLoc);
        joinBlock->callers.push(altBlock->label);

        block = joinBlock;
        return phi;
    } else {
        return condName; // Arbitrary value, will not be used by callee
    }
}

IRName quoteToCPS(CPSConv& pass, IRBlock*& block, ORef args, ToCpsCont k) {
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO
    }
    auto const argsPair = HRef<Pair>::fromUnchecked(args);

    if (!isEmptyList(pass.state, argsPair->cdr().get())) {
        assert(false); // TODO
    }

    return constToCPS(pass, *block, argsPair->car().get(), argsPair->maybeLoc().get(), k);
}

IRName defToCPS(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef args, ORef maybeLoc,
    ToCpsCont k
) {
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO
    }
    auto argsPair = HRef<Pair>::fromUnchecked(args);

    ORef const pat = argsPair->car().get();
    if (!isa<Symbol>(*pass.state, pat)) {
        pass.error({argsPair->maybeLoc().get(), INVALID_DEFINIEND});
    }
    HRef<Symbol> const name = HRef<Symbol>::fromUnchecked(pat);
    args = argsPair->cdr().get();
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args);

    ORef const val = argsPair->car().get();
    ORef const valLoc = argsPair->maybeLoc().get();
    if (!isEmptyList(pass.state, argsPair->cdr().get())) {
        assert(false); // TODO
    }

    IRName const nameHint = renameSymbol(pass.compiler, name);
    ToCpsCont const defK =
        ToCpsCont{.def = {.name = nameHint, .sym = name}, .type = ToCpsCont::DEF};
    IRName const valName = exprToIR(pass, fn, env, block, val, valLoc, defK);
    block->stmts.push(IRStmt{Define{name, valName}, maybeLoc});
    // FIXME: Return e.g. nil/undefined/unspecified instead of new val:
    IRName const resName = valName;
    if (k.type == ToCpsCont::RETURN) {
        block->createReturn(k.ret.cont, resName, maybeLoc);
    }

    return resName;
}

// FIXME: Complain if target is locally bound:
IRName setToCPS(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef args, ORef maybeLoc,
    ToCpsCont k
) {
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO
    }
    auto argsPair = HRef<Pair>::fromUnchecked(args);

    ORef const pat = argsPair->car().get();
    if (!isa<Symbol>(*pass.state, pat)) {
        pass.error({argsPair->maybeLoc().get(), INVALID_DEFINIEND});
    }
    HRef<Symbol> const name = HRef<Symbol>::fromUnchecked(pat);
    args = argsPair->cdr().get();
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args);

    ORef const val = argsPair->car().get();
    ORef const valLoc = argsPair->maybeLoc().get();
    if (!isEmptyList(pass.state, argsPair->cdr().get())) {
        assert(false); // TODO
    }

    IRName const nameHint = renameSymbol(pass.compiler, name);
    ToCpsCont const setK =
        ToCpsCont{.def = {.name = nameHint, .sym = name}, .type = ToCpsCont::SET};
    IRName const valName = exprToIR(pass, fn, env, block, val, valLoc, setK);
    block->stmts.push(IRStmt{GlobalSet{name, valName}, maybeLoc});
    // FIXME: Return e.g. nil/undefined/unspecified instead of new val:
    IRName const resName = valName;
    if (k.type == ToCpsCont::RETURN) {
        block->createReturn(k.ret.cont, resName, maybeLoc);
    }

    return resName;
}

IRName letToCPS(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef args, ToCpsCont k
) {
    auto letEnv = ToCpsEnv{&pass.compiler->arena, &env};

    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    auto const argsPair = HRef<Pair>::fromUnchecked(args);

    for (ORef bindings = argsPair->car().get();;) {
        if (isa<Pair>(*pass.state, bindings)) {
            auto const bindingsPair = HRef<Pair>::fromUnchecked(bindings);

            ORef const binding = bindingsPair->car().get();
            if (!isa<Pair>(*pass.state, binding)) {
                pass.error({bindingsPair->maybeLoc().get(), INVALID_BINDING});
            }
            auto const bindingPair = HRef<Pair>::fromUnchecked(binding);

            ORef const pat = bindingPair->car().get();
            if (!isa<Symbol>(*pass.state, pat)) {
                assert(false); // TODO: Proper invalid binder error
            }
            HRef<Symbol> const binder = HRef<Symbol>::fromUnchecked(pat);

            ORef const bindingArgs = bindingPair->cdr().get();
            if (!isa<Pair>(*pass.state, bindingArgs)) {
                pass.error({bindingPair->maybeLoc().get(), INVALID_BINDER});
            }
            auto const bindingArgsPair = HRef<Pair>::fromUnchecked(bindingArgs);

            ORef const val = bindingArgsPair->car().get();
            ORef const valLoc = bindingArgsPair->maybeLoc().get();

            if (!isEmptyList(pass.state, bindingArgsPair->cdr().get())) {
                pass.error({bindingsPair->maybeLoc().get(), OVERLONG_BINDING});
            }

            IRName const binderName = renameSymbol(pass.compiler, binder);
            ToCpsCont const valK =
                ToCpsCont{.def = {.name = binderName, .sym = binder}, .type = ToCpsCont::BIND};
            IRName const finalName = exprToIR(pass, fn, letEnv, block, val, valLoc, valK);
            // If `finalName != binderName` we have a local copy e.g.
            // `(let ((x 5) (y x)) ...)` and `useToCPS` emitted nothing. Putting
            // `finalName` to env implements the rest of copy propagation:
            letEnv.setSymbolDef(binder,
                                ToCpsEnv::Def{.name = finalName, .type = ToCpsEnv::Def::NAME},
                                ToCpsEnv::BINDINGS_SEQ);

            bindings = bindingsPair->cdr().get();
        } else if (isEmptyList(pass.state, bindings)) {
            break;
        } else {
            assert(false); // TODO: Proper invalid bindings error
        }
    }

    return bodyToCPS(pass, fn, letEnv, block, argsPair->cdr().get(), k);
}

void knotCreation(CPSConv& pass, IRBlock& block, ToCpsEnv& letfnEnv, ORef binding, ORef maybeLoc) {
    if (!isa<Pair>(*pass.state, binding)) {
        pass.error({maybeLoc, INVALID_BINDING});
    }
    auto const bindingPair = HRef<Pair>::fromUnchecked(binding); // `((f x) ...)`

    ORef const binder = bindingPair->car().get();
    if (!isa<Pair>(*pass.state, binder)) {
        assert(false); // TODO: Proper invalid binder error
    }
    auto const binderPair = HRef<Pair>::fromUnchecked(binder); // `(f x)`

    ORef const pat = binderPair->car().get();
    if (!isa<Symbol>(*pass.state, pat)) {
        pass.error({binderPair->maybeLoc().get(), INVALID_BINDER});
    }
    HRef<Symbol> const fSym = HRef<Symbol>::fromUnchecked(pat); // `f`

    IRName const knotName = renameSymbol(pass.compiler, fSym);
    block.stmts.push(IRStmt{KnotStmt{knotName}, maybeLoc});
    letfnEnv.setSymbolDef(fSym, ToCpsEnv::Def{.knotName = knotName, .type = ToCpsEnv::Def::KNOT},
                          ToCpsEnv::BINDINGS_PAR);
}

ToCpsEnv knotCreations(CPSConv& pass, IRBlock& block, ToCpsEnv const& env, ORef bindings) {
    auto innerEnv = ToCpsEnv{&pass.compiler->arena, &env};

    for (;/*ever*/;) {
        if (isa<Pair>(*pass.state, bindings)) {
            auto const bindingsPair = HRef<Pair>::fromUnchecked(bindings);

            knotCreation(pass, block, innerEnv, bindingsPair->car().get(),
                         bindingsPair->maybeLoc().get());

            bindings = bindingsPair->cdr().get();
        } else if (isEmptyList(pass.state, bindings)) {
            return innerEnv;
        } else {
            assert(false); // TODO: Proper invalid bindings error
        }
    }
}

void knotInit(CPSConv& pass, IRFn& fn, ToCpsEnv& env, IRBlock*& block, ORef binding) {
    if (!isa<Pair>(*pass.state, binding)) {
    }
    auto const bindingPair = HRef<Pair>::fromUnchecked(binding); // `((f x) ...)`

    ORef const binder = bindingPair->car().get();
    if (!isa<Pair>(*pass.state, binder)) {
        assert(false); // TODO: Proper invalid binder error (actually unreachable tho)
    }
    auto const binderPair = HRef<Pair>::fromUnchecked(binder); // `(f x)`

    ORef const pat = binderPair->car().get();
    if (!isa<Symbol>(*pass.state, pat)) {
        assert(false); // TODO: Proper invalid fn name error (actually unreachable tho)
    }
    HRef<Symbol> const fSym = HRef<Symbol>::fromUnchecked(pat); // `f`

    auto const optKnotDef = env.useSymbolDef(fSym);
    assert(optKnotDef && optKnotDef->type == ToCpsEnv::Def::KNOT);
    IRName const knotName = optKnotDef->knotName;

    IRName const self = renameSymbol(pass.compiler, fSym);
    env.setSymbolDef(fSym, ToCpsEnv::Def{.name = self, .type = ToCpsEnv::Def::NAME},
                     ToCpsEnv::BINDINGS_SEQ);
    IRName const fName = renameSymbol(pass.compiler, fSym);
    ToCpsCont const bindK =
        ToCpsCont{.def = {.name = fName, .sym = fSym}, .type = ToCpsCont::BIND};
    // Will just return `fName`, can discard that:
    fnToCPSimpl(pass, fn, env, block, self, binderPair->cdr().get(), bindingPair->cdr().get(),
                bindingPair->maybeLoc().get(), bindK);
    env.setSymbolDef(fSym, ToCpsEnv::Def{.name = fName, .type = ToCpsEnv::Def::NAME},
                     ToCpsEnv::BINDINGS_SEQ);

    block->stmts.push(IRStmt{KnotInitStmt{knotName, fName}, bindingPair->maybeLoc().get()});
}

void knotInits(CPSConv& pass, IRFn& fn, ToCpsEnv& env, IRBlock*& block, ORef bindings) {
    for (;/*ever*/;) {
        if (isa<Pair>(*pass.state, bindings)) {
            auto const bindingsPair = HRef<Pair>::fromUnchecked(bindings);

            knotInit(pass, fn, env, block, bindingsPair->car().get());

            bindings = bindingsPair->cdr().get();
        } else if (isEmptyList(pass.state, bindings)) {
            return;
        } else {
            assert(false); // TODO: Proper invalid bindings error (actually unreachable tho)
        }
    }
}

IRName letfnToCPS(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef args, ToCpsCont k
) {
    if (!isa<Pair>(*pass.state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    auto const argsPair = HRef<Pair>::fromUnchecked(args);
    ORef const bindings = argsPair->car().get();
    ORef const body = argsPair->cdr().get();

    ToCpsEnv letfnEnv = knotCreations(pass, *block, env, bindings);

    knotInits(pass, fn, letfnEnv, block, bindings);

    return bodyToCPS(pass, fn, letfnEnv, block, body, k);;
}

IRName callToCPS(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef callee, ORef calleeLoc,
    ORef args, ORef maybeLoc, ToCpsCont k
) {
    IRName const calleeName =
        exprToIR(pass, fn, env, block, callee, calleeLoc, ToCpsCont{{}, ToCpsCont::VAL});
    auto cpsArgs = AVec<IRName>{&pass.compiler->arena};
    for (;/*ever*/;) {
        if (isa<Pair>(*pass.state, args)) {
            auto const argsPair = HRef<Pair>::fromUnchecked(args);

            ORef const arg = argsPair->car().get();
            ORef const argLoc = argsPair->maybeLoc().get();
            IRName const argName =
                exprToIR(pass, fn, env, block, arg, argLoc, ToCpsCont{{}, ToCpsCont::VAL});
            cpsArgs.push(argName);

            args = argsPair->cdr().get();
        } else if (isEmptyList(pass.state, args)) {
            break;
        } else {
            assert(false); // TODO: proper improper args error
        }
    }

    IRName const retValName = k.destName(*pass.compiler);

    if (k.type != ToCpsCont::RETURN) {
        IRBlock* const retBlock = fn.createBlock(0);
        IRName const frame = freshName(pass.compiler);
        retBlock->params.push(frame);
        retBlock->params.push(retValName);

        block->createCall(calleeName, retBlock->label, AVec<IRName>{&pass.compiler->arena},
                          std::move(cpsArgs), maybeLoc);

        block = retBlock;
    } else {
        block->createTailcall(calleeName, k.ret.cont, std::move(cpsArgs), maybeLoc);
    }

    return retValName;
}

IRName useToCPS(
    CPSConv& pass, ToCpsEnv const& env, IRBlock*& block, HRef<Symbol> sym, ORef maybeLoc,
    ToCpsCont k
) {
    auto const optDef = env.useSymbolDef(sym);
    if (optDef) {
        auto const def = *optDef;

        switch (def.type) {
        case ToCpsEnv::Def::NAME: {
            IRName const name = def.name;

            if (k.type == ToCpsCont::RETURN) {
                block->createReturn(k.ret.cont, name, maybeLoc);
            }

            return name;
        }; break;

        case ToCpsEnv::Def::KNOT: {
            IRName const knotName = def.knotName;

            IRName const name = renameIRName(pass.compiler, knotName);
            block->stmts.push(IRStmt{KnotGetStmt{name, knotName}, maybeLoc});

            if (k.type == ToCpsCont::RETURN) {
                block->createReturn(k.ret.cont, name, maybeLoc);
            }

            return name;
        }; break;
        }
    } else {
        return globalToCPS(pass, *block, sym, maybeLoc, k);
    }

    assert(false); // Unreachable
    return invalidIRName;
}

IRName exprToIR(
    CPSConv& pass, IRFn& fn, ToCpsEnv const& env, IRBlock*& block, ORef expr, ORef maybeLoc,
    ToCpsCont k
) {
    if (isHeaped(expr)) {
        if (isa<Pair>(*pass.state, expr)) {
            auto const callPair = HRef<Pair>::fromUnchecked(expr);
            ORef const callee = callPair->car().get();
            ORef const args = callPair->cdr().get();

            if (isa<Symbol>(*pass.state, callee)) {
                HRef<Symbol> const calleeSym = HRef<Symbol>::fromUnchecked(callee);

                // OPTIMIZE: Symbol comparisons instead of `strEq`:
                if (strEq(calleeSym->name(), strLit("fn"))) {
                    return fnToCPS(pass, fn, env, block, args, maybeLoc, k);
                } else if (strEq(calleeSym->name(), strLit("if"))) {
                    return ifToCPS(pass, fn, env, block, args, maybeLoc, k);
                } else if (strEq(calleeSym->name(), strLit("quote"))) {
                    return quoteToCPS(pass, block, args, k);
                } else if (strEq(calleeSym->name(), strLit("define"))) {
                    return defToCPS(pass, fn, env, block, args, maybeLoc, k);
                } else if (strEq(calleeSym->name(), strLit("set!"))) {
                    return setToCPS(pass, fn, env, block, args, maybeLoc, k);
                } else if (strEq(calleeSym->name(), strLit("let"))) {
                    return letToCPS(pass, fn, env, block, args, k);
                } else if (strEq(calleeSym->name(), strLit("letfn"))) {
                    return letfnToCPS(pass, fn, env, block, args, k);
                }
            }

            return callToCPS(pass, fn, env, block, callee, callPair->maybeLoc().get(), args,
                             maybeLoc, k);
        } else if (isa<Symbol>(*pass.state, expr)) {
            HRef<Symbol> const sym = HRef<Symbol>::fromUnchecked(expr);

            return useToCPS(pass, env, block, sym, maybeLoc, k);
        }
    }

    // Else a constant:
    return constToCPS(pass, *block, expr, maybeLoc, k);
}

// Pass API
// =================================================================================================

ToIRRes topLevelExprToIR(RT const& state, Compiler& compiler, ORef expr, HRef<Loc> loc) {
    CPSConv pass{state, compiler};

    auto fn = IRFn{&pass.compiler->arena, Default};

    {
        IRBlock* entryBlock = fn.createBlock(0);

        ToCpsEnv env = ToCpsEnv{&compiler.arena, nullptr};
        IRName const self = freshName(pass.compiler);
        entryBlock->params.push(self);
        IRName const ret = freshName(pass.compiler);
        entryBlock->params.push(ret);

        ToCpsCont const retK = {{.ret = {.cont = ret}}, ToCpsCont::RETURN};
        exprToIR(pass, fn, env, entryBlock, expr, loc, retK);
    }

    Slice<Vshs_SyntaxError const> const errSlice = pass.errors();
    if (errSlice.count == 0) {
        return ToIRRes{std::move(fn)};
    } else {
        Vshs_SyntaxError* errVals = (Vshs_SyntaxError*)malloc(errSlice.count * sizeof *errVals);
        memcpy(errVals, errSlice.data, errSlice.count * sizeof *errVals);
        Vshs_SyntaxErrors const errs = {errVals, errSlice.count};
        return ToIRRes{errs};
    }
}

} // namespace
