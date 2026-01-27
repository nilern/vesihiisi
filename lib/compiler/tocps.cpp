#include "tocps.hpp"

#include <string.h>

#include "../state.hpp"
#include "../util/avec.hpp"

namespace {

// Env
// =================================================================================================

// Frame
// -------------------------------------------------------------------------------------------------

typedef struct ToCpsFrameDef {
    union {
        IRName name;
        IRName knotName;
    };
    enum {
        NAME,
        KNOT
    } type;
} ToCpsFrameDef;

typedef struct ToCpsFrame {
    ORef* keys;
    ToCpsFrameDef* vals;
    size_t count;
    size_t cap;
} ToCpsFrame;

inline void freeToCpsFrame(ToCpsFrame* frame) { free(frame->keys); }

ToCpsFrame createToCpsFrame(void) {
    size_t const cap = 2;
    void* const kvs = malloc(cap * (sizeof *ToCpsFrame{}.keys + sizeof *ToCpsFrame{}.vals));
    ORef* const keys = (ORef*)kvs;
    memset(keys, 0, cap * sizeof *keys);
    ToCpsFrameDef* const vals = (ToCpsFrameDef*)(keys + cap);

    return ToCpsFrame{
        .keys = keys,
        .vals = vals,
        .count = 0,
        .cap = cap
    };
}

void rehashToCpsFrame(ToCpsFrame* frame) {
    size_t const oldCap = frame->cap;
    ORef* const oldKeys = frame->keys;
    ToCpsFrameDef* const oldVals = frame->vals;

    size_t const newCap = oldCap << 1;
    void* const newKvs =
        malloc(newCap * (sizeof *ToCpsFrame{}.keys + sizeof *ToCpsFrame{}.vals));
    ORef* const newKeys = (ORef*)newKvs;
    memset(newKeys, 0, newCap * sizeof *newKeys);
    ToCpsFrameDef* const newVals = (ToCpsFrameDef*)(newKeys + newCap);

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const k = oldKeys[i];

        if (!eq(k, Default)) {
            size_t const h = (uintptr_t)HRef<Symbol>::fromUnchecked(k).ptr()->hash.val();

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const maybeK = newKeys + j;

                if (eq(*maybeK, Default)) {
                    *maybeK = k;
                    newVals[j] = oldVals[i];
                    break;
                }
            }
        }
    }

    frame->keys = newKeys;
    frame->vals = newVals;
    frame->cap = newCap;

    free(oldKeys);
}

BucketIdx toCpsFrameFindIdx(ToCpsFrame const* frame, HRef<Symbol> sym) {
    size_t const h = (uintptr_t)sym.ptr()->hash.val();

    size_t const maxIdx = frame->cap - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        ORef const k = frame->keys[i];

        if (eq(k, sym.oref())) {
            return BucketIdx{.idx = i, .occupied = true};
        } else if (eq(k, Default)) {
            return BucketIdx{.idx = i, .occupied = false};
        }
    }
}

ToCpsFrameDef toCpsFrameFind(ToCpsFrame const* frame, HRef<Symbol> sym) {
    BucketIdx const bucketIdx = toCpsFrameFindIdx(frame, sym);
    if (!bucketIdx.occupied) {
        return ToCpsFrameDef{.name = invalidIRName, .type = ToCpsFrameDef::NAME};
    }

    return frame->vals[bucketIdx.idx];
}

void toCpsFrameSet(ToCpsFrame* frame, HRef<Symbol> sym, ToCpsFrameDef def) {
    BucketIdx bucketIdx = toCpsFrameFindIdx(frame, sym);
    if (bucketIdx.occupied) {
        frame->vals[bucketIdx.idx] = def;
    } else {
        size_t idx = bucketIdx.idx;

        size_t const newCount = frame->count + 1;
        if (newCount > (frame->cap >> 1)) {
            rehashToCpsFrame(frame);
            bucketIdx = toCpsFrameFindIdx(frame, sym);
            assert(!bucketIdx.occupied);
            idx = bucketIdx.idx;
        }

        frame->keys[idx] = sym.oref();
        frame->vals[idx] = def;
        frame->count = newCount;
    }
}

// Env as a Stack of Frames
// -------------------------------------------------------------------------------------------------

typedef struct ToCpsEnv {
    ToCpsFrame frame;
    struct ToCpsEnv const* parent;
} ToCpsEnv;

inline void freeToCpsEnv(ToCpsEnv* env) { freeToCpsFrame(&env->frame); }

ToCpsEnv createToCpsEnv(ToCpsEnv const* parent) {
    return ToCpsEnv{
        .frame = createToCpsFrame(),
        .parent = parent
    };
}

// Defs and Uses
// -------------------------------------------------------------------------------------------------

ToCpsFrameDef useSymbolDef(ToCpsEnv const* env, HRef<Symbol> sym) {
    for (; env; env = env->parent) {
        ToCpsFrameDef const def = toCpsFrameFind(&env->frame, sym);
        switch (def.type) {
        case ToCpsFrameDef::NAME: {
            if (irNameIsValid(def.name)) { return def; }
        }; break;

        case ToCpsFrameDef::KNOT: return def;
        }
    }

    return ToCpsFrameDef{.name = invalidIRName, .type = ToCpsFrameDef::NAME};
}

typedef enum BindingsType {
    BINDINGS_PAR,
    BINDINGS_SEQ
} BindingsType;

void setSymbolDef(ToCpsEnv* env, HRef<Symbol> sym, ToCpsFrameDef def, BindingsType type) {
     // FIXME: Proper error (duplicate defs):
    if (type == BINDINGS_PAR) {
        ToCpsFrameDef const oldDef /*HACK:*/ [[maybe_unused]] = toCpsFrameFind(&env->frame, sym);
        assert(oldDef.type != ToCpsFrameDef::NAME || !irNameIsValid(oldDef.name));
    }

    toCpsFrameSet(&env->frame, sym, def);
}

// Static Continuations
// =================================================================================================

typedef struct ToCpsContReturn {
    IRName cont;
} ToCpsContReturn;

typedef struct ToCpsCont {
    struct Def {
        IRName name;
        HRef<Symbol> sym;
    };

    union {
        ToCpsContReturn ret;
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
} ToCpsCont;

IRName toCpsContDestName(Compiler* compiler, ToCpsCont k) {
    switch (k.type) {
    case ToCpsCont::BIND: // fallthrough
    case ToCpsCont::DEF: case ToCpsCont::SET: return k.def.name;

    case ToCpsCont::EFF: // fallthrough
    case ToCpsCont::VAL: // fallthrough
    case ToCpsCont::JOIN: // fallthrough
    case ToCpsCont::RETURN: return freshName(compiler);
    }

    return invalidIRName; // Unreachable
}

ORef toCpsContDestSymbol(ToCpsCont k) {
    switch (k.type) {
    case ToCpsCont::BIND: // fallthrough
    case ToCpsCont::DEF: case ToCpsCont::SET: return k.def.sym.oref();

    case ToCpsCont::EFF: // fallthrough
    case ToCpsCont::VAL: // fallthrough
    case ToCpsCont::JOIN: // fallthrough
    case ToCpsCont::RETURN: return Default;
    }

    return Default; // Unreachable
}

// Conversion from S-expressions to CPS IR
// =================================================================================================

struct CPSConv {
    CPSConv(State const* t_state, Compiler* t_compiler) :
        state{t_state}, compiler{t_compiler}, errs{&compiler->arena} {}

    void error(SyntaxError err) { errs.push(err); }

    Slice<SyntaxError const> errors() const { return errs.slice(); }

public:
    State const* state;
    Compiler* compiler;
private:
    AVec<SyntaxError> errs;
};

IRName constToCPS(CPSConv& pass, IRBlock* block, ORef expr, ORef maybeLoc, ToCpsCont k) {
    IRName const name = toCpsContDestName(pass.compiler, k);
    pushIRStmt(pass.compiler, &block->stmts, constDefToStmt(ConstDef{name, expr}, maybeLoc));

    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(block, k.ret.cont, name, maybeLoc);
    }

    return name;
}

IRName globalToCPS(CPSConv& pass, IRBlock* block, HRef<Symbol> sym, ORef maybeLoc, ToCpsCont k) {
    IRName const name = toCpsContDestName(pass.compiler, k);
    pushIRStmt(pass.compiler, &block->stmts, globalToStmt(IRGlobal{name, sym}, maybeLoc));

    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(block, k.ret.cont, name, maybeLoc);
    }

    return name;
}

IRName exprToIR(CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef expr,
                ORef maybeLoc, ToCpsCont k);

IRName bodyToCPS(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef body, ToCpsCont k
) {
    if (!isPair(pass.state, body)) {
        assert(false); // TODO: Proper empty/improper body error
    }
    Pair const* argsPair = HRef<Pair>::fromUnchecked(body).ptr();

    for (;/*ever*/;) {
        ORef const stmt = argsPair->car;
        body = argsPair->cdr;
        ORef const maybeLoc = argsPair->maybeLoc;

        if (isEmptyList(pass.state, body)) {
            IRName const bodyName = exprToIR(pass, fn, env, block, stmt, maybeLoc, k);

            return bodyName;
        } else if (isPair(pass.state, body)){
            exprToIR(pass, fn, env, block, stmt, maybeLoc, ToCpsCont{{}, ToCpsCont::EFF});

            argsPair = HRef<Pair>::fromUnchecked(body).ptr();
        } else {
            assert(false); // TODO: Proper improper args error
        }
    }
}

[[nodiscard]]
bool paramToCPS(
    CPSConv& pass, IRFn* outerFn, ToCpsEnv const* outerEnv, IRBlock** outerBlock, IRFn* fn,
    ToCpsEnv* fnEnv, IRBlock* entryBlock, size_t idx, ORef param
) {
    if (isSymbol(pass.state, param)) {
        HRef<Symbol> const paramSym = HRef<Symbol>::fromUnchecked(param);

        IRName const paramName = renameSymbol(pass.compiler, paramSym);
        pushIRParam(pass.compiler, entryBlock, paramName);
        setSymbolDef(fnEnv, paramSym, ToCpsFrameDef{.name = paramName, .type = ToCpsFrameDef::NAME},
                     BINDINGS_PAR);

        return true;
    } else if (isPair(pass.state, param)) {
        Pair const* const paramPair = HRef<Pair>::fromUnchecked(param).ptr();

        ORef const op = paramPair->car;
        if (!eq(op, pass.state->singletons.ofType.oref())) { return false; }

        ORef anyArgs = paramPair->cdr;
        if (!isPair(pass.state, anyArgs)) { return false; }
        Pair const* args = HRef<Pair>::fromUnchecked(anyArgs).ptr();

        ORef const maybeSym = args->car;
        if (!isSymbol(pass.state, maybeSym)) { return false; }
        HRef<Symbol> const sym = HRef<Symbol>::fromUnchecked(maybeSym);

        anyArgs = args->cdr;
        if (!isPair(pass.state, anyArgs)) { return false; }
        args = HRef<Pair>::fromUnchecked(anyArgs).ptr();

        ORef const type = args->car;

        if (!isEmptyList(pass.state, args->cdr)) { return false; }

        IRName const typeName = exprToIR(pass, outerFn, outerEnv, outerBlock, type, args->maybeLoc,
                                         ToCpsCont{{}, ToCpsCont::VAL});

        // TODO: DRY with symbol branch above:
        IRName const paramName = renameSymbol(pass.compiler, sym);
        pushIRParam(pass.compiler, entryBlock, paramName);
        setSymbolDef(fnEnv, sym, ToCpsFrameDef{.name = paramName, .type = ToCpsFrameDef::NAME},
                     BINDINGS_PAR);
        setParamType(pass.compiler, &fn->domain, idx, typeName);

        return true;
    } else {
        return false;
    }
}

IRName fnToCPSimpl(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, IRName maybeSelf, ORef params,
    ORef body, ORef maybeLoc, ToCpsCont k
) {
    ORef const maybeName = toCpsContDestSymbol(k);
    IRFn innerFn = createIRFn(pass.compiler, maybeName);

    IRBlock* entryBlock = createIRBlock(pass.compiler, &innerFn, 0);

    ToCpsEnv fnEnv = createToCpsEnv(env);
    IRName const self = irNameIsValid(maybeSelf) ? maybeSelf : freshName(pass.compiler);
    pushIRParam(pass.compiler, entryBlock, self);
    IRName const ret = freshName(pass.compiler);
    pushIRParam(pass.compiler, entryBlock, ret);

    size_t arity = 0;

    while (!isEmptyList(pass.state, params)) {
        // TODO: Is this just bad syntax design?:
        // Has to be first because `(x y . (: zs <t>))` = `(x y : zs <t>)`:
        if (paramToCPS(pass, fn, env, block,
                       &innerFn, &fnEnv, entryBlock, arity, params)
            ) {
            innerFn.hasVarArg = true;

            ++arity;
            break;
        }

        if (isPair(pass.state, params)) {
            Pair const* const paramsPair = HRef<Pair>::fromUnchecked(params).ptr();

            if (!paramToCPS(pass, fn, env, block,
                            &innerFn, &fnEnv, entryBlock, arity, paramsPair->car)
                ) {
                pass.error({paramsPair->maybeLoc, INVALID_PARAM});
            }
            params = paramsPair->cdr;

            ++arity;
            continue;
        }

        assert(false); // TODO: Proper invalid vararg error
    }

    completeIRDomain(pass.compiler, &innerFn.domain, arity);

    ToCpsCont const retK = {{.ret = {.cont = ret}}, ToCpsCont::RETURN};
    // Body is in tail position so discard the returned `IRName`:
    bodyToCPS(pass, &innerFn, &fnEnv, &entryBlock, body, retK);
    freeToCpsEnv(&fnEnv);

    IRName const name = toCpsContDestName(pass.compiler, k);
    Args* const closes = (Args*)amalloc(&pass.compiler->arena, sizeof *closes);
    *closes = createArgs(pass.compiler);
    pushIRStmt(pass.compiler, &(*block)->stmts,
               IRStmt{maybeLoc, {.methodDef = {name, innerFn, closes}}, IRStmt::METHOD_DEF});

    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(*block, k.ret.cont, name, maybeLoc);
    }

    return name;
}

IRName fnToCPS(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef args, ORef maybeLoc,
    ToCpsCont k
) {
    if (!isPair(pass.state, args)) {
        assert(false); // TODO: Proper args error (`(fn)`)
    }
    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const params = argsPair->car;
    ORef const body = argsPair->cdr;
    return fnToCPSimpl(pass, fn, env, block, invalidIRName, params, body, maybeLoc, k);
}

IRName ifToCPS(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef args, ORef maybeLoc,
    ToCpsCont k
) {
    // OPTIMIZE: Avoid creating `goto`s to `goto`s:

    if (!isPair(pass.state, args)) {
        assert(false); // TODO
    }
    Pair const* argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const cond = argsPair->car;
    ORef const condLoc = argsPair->maybeLoc;
    args = argsPair->cdr;
    if (!isPair(pass.state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const conseq = argsPair->car;
    ORef const conseqLoc = argsPair->maybeLoc;
    args = argsPair->cdr;
    if (!isPair(pass.state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const alt = argsPair->car;
    ORef const altLoc = argsPair->maybeLoc;
    if (!isEmptyList(pass.state, argsPair->cdr)) {
        assert(false); // TODO
    }

    ToCpsCont const splitK = ToCpsCont{{}, ToCpsCont::VAL};
    IRName const condName = exprToIR(pass, fn, env, block, cond, condLoc, splitK);
        // Will patch targets shortly:
    IRIf* ifTransfer = createIRIf(*block, condName, IRLabel{}, IRLabel{}, maybeLoc);
    IRLabel const ifLabel = (*block)->label;
    ToCpsCont const joinK = k.type != ToCpsCont::RETURN
        ? ToCpsCont{{}, ToCpsCont::JOIN}
        : k;

    IRBlock* conseqBlock = createIRBlock(pass.compiler, fn, 1);
    pushCaller(conseqBlock, ifLabel);
    ifTransfer->conseq = conseqBlock->label;
    IRName const conseqName = exprToIR(pass, fn, env, &conseqBlock, conseq, conseqLoc, joinK);

    IRBlock* altBlock = createIRBlock(pass.compiler, fn, 1);
    pushCaller(altBlock, ifLabel);
    ifTransfer->alt = altBlock->label;
    IRName const altName = exprToIR(pass, fn, env, &altBlock, alt, altLoc, joinK);

    if (k.type != ToCpsCont::RETURN) {
        // FIXME: If we avoid `goto`s to `goto`s, 2 might not suffice:
        IRBlock* const joinBlock = createIRBlock(pass.compiler, fn, 2);
        IRName const phi = toCpsContDestName(pass.compiler, k);
        pushIRParam(pass.compiler, joinBlock, phi);

        createIRGoto(pass.compiler, conseqBlock, joinBlock->label, conseqName, conseqLoc);
        pushCaller(joinBlock, conseqBlock->label);

        createIRGoto(pass.compiler, altBlock, joinBlock->label, altName, altLoc);
        pushCaller(joinBlock, altBlock->label);

        *block = joinBlock;
        return phi;
    } else {
        return condName; // Arbitrary value, will not be used by callee
    }
}

IRName quoteToCPS(CPSConv& pass, IRBlock** block, ORef args, ToCpsCont k) {
    if (!isPair(pass.state, args)) {
        assert(false); // TODO
    }
    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    if (!isEmptyList(pass.state, argsPair->cdr)) {
        assert(false); // TODO
    }

    return constToCPS(pass, *block, argsPair->car, argsPair->maybeLoc, k);
}

IRName defToCPS(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef args, ORef maybeLoc,
    ToCpsCont k
) {
    if (!isPair(pass.state, args)) {
        assert(false); // TODO
    }
    Pair const* argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const pat = argsPair->car;
    if (!isSymbol(pass.state, pat)) {
        pass.error({argsPair->maybeLoc, INVALID_DEFINIEND});
    }
    HRef<Symbol> const name = HRef<Symbol>::fromUnchecked(pat);
    args = argsPair->cdr;
    if (!isPair(pass.state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const val = argsPair->car;
    ORef const valLoc = argsPair->maybeLoc;
    if (!isEmptyList(pass.state, argsPair->cdr)) {
        assert(false); // TODO
    }

    IRName const nameHint = renameSymbol(pass.compiler, name);
    ToCpsCont const defK =
        ToCpsCont{.def = {.name = nameHint, .sym = name}, .type = ToCpsCont::DEF};
    IRName const valName = exprToIR(pass, fn, env, block, val, valLoc, defK);
    pushIRStmt(pass.compiler, &(*block)->stmts,
               defineToStmt(Define{name, valName}, maybeLoc));
    // FIXME: Return e.g. nil/undefined/unspecified instead of new val:
    IRName const resName = valName;
    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(*block, k.ret.cont, resName, maybeLoc);
    }

    return resName;
}

// FIXME: Complain if target is locally bound:
IRName setToCPS(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef args, ORef maybeLoc,
    ToCpsCont k
) {
    if (!isPair(pass.state, args)) {
        assert(false); // TODO
    }
    Pair const* argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const pat = argsPair->car;
    if (!isSymbol(pass.state, pat)) {
        pass.error({argsPair->maybeLoc, INVALID_DEFINIEND});
    }
    HRef<Symbol> const name = HRef<Symbol>::fromUnchecked(pat);
    args = argsPair->cdr;
    if (!isPair(pass.state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const val = argsPair->car;
    ORef const valLoc = argsPair->maybeLoc;
    if (!isEmptyList(pass.state, argsPair->cdr)) {
        assert(false); // TODO
    }

    IRName const nameHint = renameSymbol(pass.compiler, name);
    ToCpsCont const setK =
        ToCpsCont{.def = {.name = nameHint, .sym = name}, .type = ToCpsCont::SET};
    IRName const valName = exprToIR(pass, fn, env, block, val, valLoc, setK);
    pushIRStmt(pass.compiler, &(*block)->stmts,
               globalSetToStmt(GlobalSet{name, valName}, maybeLoc));
    // FIXME: Return e.g. nil/undefined/unspecified instead of new val:
    IRName const resName = valName;
    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(*block, k.ret.cont, resName, maybeLoc);
    }

    return resName;
}

IRName letToCPS(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef args, ToCpsCont k
) {
    ToCpsEnv letEnv = createToCpsEnv(env);

    if (!isPair(pass.state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    for (ORef bindings = argsPair->car;;) {
        if (isPair(pass.state, bindings)) {
            Pair const* const bindingsPair = HRef<Pair>::fromUnchecked(bindings).ptr();

            ORef const binding = bindingsPair->car;
            if (!isPair(pass.state, binding)) {
                pass.error({bindingsPair->maybeLoc, INVALID_BINDING});
            }
            Pair const* const bindingPair = HRef<Pair>::fromUnchecked(binding).ptr();

            ORef const pat = bindingPair->car;
            if (!isSymbol(pass.state, pat)) {
                assert(false); // TODO: Proper invalid binder error
            }
            HRef<Symbol> const binder = HRef<Symbol>::fromUnchecked(pat);

            ORef const bindingArgs = bindingPair->cdr;
            if (!isPair(pass.state, bindingArgs)) {
                pass.error({bindingPair->maybeLoc, INVALID_BINDER});
            }
            Pair const* const bindingArgsPair = HRef<Pair>::fromUnchecked(bindingArgs).ptr();

            ORef const val = bindingArgsPair->car;
            ORef const valLoc = bindingArgsPair->maybeLoc;

            if (!isEmptyList(pass.state, bindingArgsPair->cdr)) {
                pass.error({bindingsPair->maybeLoc, OVERLONG_BINDING});
            }

            IRName const binderName = renameSymbol(pass.compiler, binder);
            ToCpsCont const valK =
                ToCpsCont{.def = {.name = binderName, .sym = binder}, .type = ToCpsCont::BIND};
            IRName const finalName =
                exprToIR(pass, fn, &letEnv, block, val, valLoc, valK);
            // If `finalName != binderName` we have a local copy e.g.
            // `(let ((x 5) (y x)) ...)` and `useToCPS` emitted nothing. Putting
            // `finalName` to env implements the rest of copy propagation:
            setSymbolDef(&letEnv, binder,
                         ToCpsFrameDef{.name = finalName, .type = ToCpsFrameDef::NAME},
                         BINDINGS_SEQ);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(pass.state, bindings)) {
            break;
        } else {
            assert(false); // TODO: Proper invalid bindings error
        }
    }

    IRName const bodyName = bodyToCPS(pass, fn, &letEnv, block, argsPair->cdr, k);
    freeToCpsEnv(&letEnv);
    return bodyName;
}

void knotCreation(CPSConv& pass, IRBlock* block, ToCpsEnv* letfnEnv, ORef binding, ORef maybeLoc) {
    if (!isPair(pass.state, binding)) {
        pass.error({maybeLoc, INVALID_BINDING});
    }
    Pair const* const bindingPair = HRef<Pair>::fromUnchecked(binding).ptr(); // `((f x) ...)`

    ORef const binder = bindingPair->car;
    if (!isPair(pass.state, binder)) {
        assert(false); // TODO: Proper invalid binder error
    }
    Pair const* const binderPair = HRef<Pair>::fromUnchecked(binder).ptr(); // `(f x)`

    ORef const pat = binderPair->car;
    if (!isSymbol(pass.state, pat)) {
        pass.error({binderPair->maybeLoc, INVALID_BINDER});
    }
    HRef<Symbol> const fSym = HRef<Symbol>::fromUnchecked(pat); // `f`

    IRName const knotName = renameSymbol(pass.compiler, fSym);
    pushIRStmt(pass.compiler, &block->stmts,
               IRStmt{maybeLoc, {.knot = {.name = knotName}}, IRStmt::KNOT});
    setSymbolDef(letfnEnv, fSym, ToCpsFrameDef{.knotName = knotName, .type = ToCpsFrameDef::KNOT},
                 BINDINGS_PAR);
}

ToCpsEnv knotCreations(CPSConv& pass, IRBlock* block, ToCpsEnv const* env, ORef bindings) {
    ToCpsEnv innerEnv = createToCpsEnv(env);

    for (;/*ever*/;) {
        if (isPair(pass.state, bindings)) {
            Pair const* const bindingsPair = HRef<Pair>::fromUnchecked(bindings).ptr();

            knotCreation(pass, block, &innerEnv, bindingsPair->car, bindingsPair->maybeLoc);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(pass.state, bindings)) {
            return innerEnv;
        } else {
            assert(false); // TODO: Proper invalid bindings error
        }
    }
}

void knotInit(CPSConv& pass, IRFn* fn, ToCpsEnv* env, IRBlock** block, ORef binding) {
    if (!isPair(pass.state, binding)) {
    }
    Pair const* const bindingPair = HRef<Pair>::fromUnchecked(binding).ptr(); // `((f x) ...)`

    ORef const binder = bindingPair->car;
    if (!isPair(pass.state, binder)) {
        assert(false); // TODO: Proper invalid binder error (actually unreachable tho)
    }
    Pair const* const binderPair = HRef<Pair>::fromUnchecked(binder).ptr(); // `(f x)`

    ORef const pat = binderPair->car;
    if (!isSymbol(pass.state, pat)) {
        assert(false); // TODO: Proper invalid fn name error (actually unreachable tho)
    }
    HRef<Symbol> const fSym = HRef<Symbol>::fromUnchecked(pat); // `f`

    ToCpsFrameDef const knotDef = useSymbolDef(env, fSym);
    assert(knotDef.type == ToCpsFrameDef::KNOT);
    IRName const knotName = knotDef.knotName;

    IRName const self = renameSymbol(pass.compiler, fSym);
    setSymbolDef(env, fSym, ToCpsFrameDef{.name = self, .type = ToCpsFrameDef::NAME}, BINDINGS_SEQ);
    IRName const fName = renameSymbol(pass.compiler, fSym);
    ToCpsCont const bindK =
        ToCpsCont{.def = {.name = fName, .sym = fSym}, .type = ToCpsCont::BIND};
    // Will just return `fName`, can discard that:
    fnToCPSimpl(pass, fn, env, block, self, binderPair->cdr, bindingPair->cdr,
                bindingPair->maybeLoc, bindK);
    setSymbolDef(env, fSym,
                 ToCpsFrameDef{.name = fName, .type = ToCpsFrameDef::NAME}, BINDINGS_SEQ);

    pushIRStmt(pass.compiler, &(*block)->stmts,
               IRStmt{bindingPair->maybeLoc, {.knotInit = {.knot = knotName, .v = fName}},
                      IRStmt::KNOT_INIT});
}

void knotInits(CPSConv& pass, IRFn* fn, ToCpsEnv* env, IRBlock** block, ORef bindings) {
    for (;/*ever*/;) {
        if (isPair(pass.state, bindings)) {
            Pair const* const bindingsPair = HRef<Pair>::fromUnchecked(bindings).ptr();

            knotInit(pass, fn, env, block, bindingsPair->car);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(pass.state, bindings)) {
            return;
        } else {
            assert(false); // TODO: Proper invalid bindings error (actually unreachable tho)
        }
    }
}

IRName letfnToCPS(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef args, ToCpsCont k
) {
    if (!isPair(pass.state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();
    ORef const bindings = argsPair->car;
    ORef const body = argsPair->cdr;

    ToCpsEnv letfnEnv = knotCreations(pass, *block, env, bindings);

    knotInits(pass, fn, &letfnEnv, block, bindings);

    IRName const bodyName = bodyToCPS(pass, fn, &letfnEnv, block, body, k);
    freeToCpsEnv(&letfnEnv);
    return bodyName;
}

IRName callToCPS(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef callee, ORef calleeLoc,
    ORef args, ORef maybeLoc, ToCpsCont k
) {
    IRName const calleeName =
        exprToIR(pass, fn, env, block, callee, calleeLoc, ToCpsCont{{}, ToCpsCont::VAL});
    Args cpsArgs = createArgs(pass.compiler);
    for (;/*ever*/;) {
        if (isPair(pass.state, args)) {
            Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

            ORef const arg = argsPair->car;
            ORef const argLoc = argsPair->maybeLoc;
            IRName const argName =
                exprToIR(pass, fn, env, block, arg, argLoc, ToCpsCont{{}, ToCpsCont::VAL});
            pushArg(pass.compiler, &cpsArgs, argName);

            args = argsPair->cdr;
        } else if (isEmptyList(pass.state, args)) {
            break;
        } else {
            assert(false); // TODO: proper improper args error
        }
    }

    IRName const retValName = toCpsContDestName(pass.compiler, k);

    if (k.type != ToCpsCont::RETURN) {
        IRBlock* const retBlock = createIRBlock(pass.compiler, fn, 0);
        IRName const frame = freshName(pass.compiler);
        pushIRParam(pass.compiler, retBlock, frame);
        pushIRParam(pass.compiler, retBlock, retValName);

        createCall(*block, calleeName, retBlock->label, createArgs(pass.compiler), cpsArgs,
                   maybeLoc);

        *block = retBlock;
    } else {
        createTailcall(*block, calleeName, k.ret.cont, cpsArgs, maybeLoc);
    }

    return retValName;
}

IRName useToCPS(
    CPSConv& pass, ToCpsEnv const* env, IRBlock** block, HRef<Symbol> sym, ORef maybeLoc,
    ToCpsCont k
) {
    ToCpsFrameDef const def = useSymbolDef(env, sym);
    switch (def.type) {
    case ToCpsFrameDef::NAME: {
        IRName const name = def.name;

        if (irNameIsValid(name)) {
            if (k.type == ToCpsCont::RETURN) {
                createIRReturn(*block, k.ret.cont, name, maybeLoc);
            }

            return name;
        } else {
            return globalToCPS(pass, *block, sym, maybeLoc, k);
        }
    }; break;

    case ToCpsFrameDef::KNOT: {
        IRName const knotName = def.knotName;

        IRName const name = renameIRName(pass.compiler, knotName);
        pushIRStmt(pass.compiler, &(*block)->stmts,
                   IRStmt{maybeLoc, {.knotGet = {.name = name, .knot = knotName}},
                          IRStmt::KNOT_GET});

        if (k.type == ToCpsCont::RETURN) {
            createIRReturn(*block, k.ret.cont, name, maybeLoc);
        }

        return name;
    }; break;
    }

    assert(false); // Unreachable
    return invalidIRName;
}

IRName exprToIR(
    CPSConv& pass, IRFn* fn, ToCpsEnv const* env, IRBlock** block, ORef expr, ORef maybeLoc,
    ToCpsCont k
) {
    if (isHeaped(expr)) {
        if (isPair(pass.state, expr)) {
            Pair const* const callPair = HRef<Pair>::fromUnchecked(expr).ptr();
            ORef const callee = callPair->car;
            ORef const args = callPair->cdr;

            if (isSymbol(pass.state, callee)) {
                HRef<Symbol> const calleeSym = HRef<Symbol>::fromUnchecked(callee);

                // OPTIMIZE: Symbol comparisons instead of `strEq`:
                if (strEq(calleeSym.ptr()->name(), strLit("fn"))) {
                    return fnToCPS(pass, fn, env, block, args, maybeLoc, k);
                } else if (strEq(calleeSym.ptr()->name(), strLit("if"))) {
                    return ifToCPS(pass, fn, env, block, args, maybeLoc, k);
                } else if (strEq(calleeSym.ptr()->name(), strLit("quote"))) {
                    return quoteToCPS(pass, block, args, k);
                } else if (strEq(calleeSym.ptr()->name(), strLit("define"))) {
                    return defToCPS(pass, fn, env, block, args, maybeLoc, k);
                } else if (strEq(calleeSym.ptr()->name(), strLit("set!"))) {
                    return setToCPS(pass, fn, env, block, args, maybeLoc, k);
                } else if (strEq(calleeSym.ptr()->name(), strLit("let"))) {
                    return letToCPS(pass, fn, env, block, args, k);
                } else if (strEq(calleeSym.ptr()->name(), strLit("letfn"))) {
                    return letfnToCPS(pass, fn, env, block, args, k);
                }
            }

            return callToCPS(pass, fn, env, block, callee, callPair->maybeLoc, args, maybeLoc, k);
        } else if (isSymbol(pass.state, expr)) {
            HRef<Symbol> const sym = HRef<Symbol>::fromUnchecked(expr);

            return useToCPS(pass, env, block, sym, maybeLoc, k);
        }
    }

    // Else a constant:
    return constToCPS(pass, *block, expr, maybeLoc, k);
}

// Pass API
// =================================================================================================

ToIRRes topLevelExprToIR(State const* state, Compiler* compiler, ORef expr, HRef<Loc> loc) {
    CPSConv pass{state, compiler};

    IRFn fn = createIRFn(pass.compiler, Default);

    {
        IRBlock* entryBlock = createIRBlock(pass.compiler, &fn, 0);

        ToCpsEnv env = createToCpsEnv(nullptr);
        IRName const self = freshName(pass.compiler);
        pushIRParam(pass.compiler, entryBlock, self);
        IRName const ret = freshName(pass.compiler);
        pushIRParam(pass.compiler, entryBlock, ret);

        ToCpsCont const retK = {{.ret = {.cont = ret}}, ToCpsCont::RETURN};
        exprToIR(pass, &fn, &env, &entryBlock, expr, loc, retK);

        freeToCpsEnv(&env);
    }

    Slice<SyntaxError const> const errSlice = pass.errors();
    if (errSlice.count == 0) {
        return ToIRRes{fn};
    } else {
        SyntaxError* errVals = (SyntaxError*)malloc(errSlice.count * sizeof *errVals);
        memcpy(errVals, errSlice.data, errSlice.count * sizeof *errVals);
        SyntaxErrors const errs = {errVals, errSlice.count};
        return ToIRRes{errs};
    }
}

} // namespace
