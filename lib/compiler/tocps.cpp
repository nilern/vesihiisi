#include "tocps.hpp"

#include <string.h>

#include "../state.hpp"

namespace {

typedef enum BindingsType {
    BINDINGS_PAR,
    BINDINGS_SEQ
} BindingsType;

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

void setSymbolDef(ToCpsEnv* env, HRef<Symbol> sym, ToCpsFrameDef def, BindingsType type) {
     // FIXME: Proper error (duplicate defs):
    if (type == BINDINGS_PAR) {
        ToCpsFrameDef const oldDef /*HACK:*/ [[maybe_unused]] = toCpsFrameFind(&env->frame, sym);
        assert(oldDef.type != ToCpsFrameDef::NAME || !irNameIsValid(oldDef.name));
    }

    toCpsFrameSet(&env->frame, sym, def);
}

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
        JOIN,
        RETURN
    } type;
} ToCpsCont;

IRName toCpsContDestName(Compiler* compiler, ToCpsCont k) {
    switch (k.type) {
    case ToCpsCont::BIND: // fallthrough
    case ToCpsCont::DEF: return k.def.name;

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
    case ToCpsCont::DEF: return k.def.sym.oref();

    case ToCpsCont::EFF: // fallthrough
    case ToCpsCont::VAL: // fallthrough
    case ToCpsCont::JOIN: // fallthrough
    case ToCpsCont::RETURN: return Default;
    }

    return Default; // Unreachable
}

IRName constToCPS(Compiler* compiler, IRBlock* block, ORef expr, ToCpsCont k) {
    IRName const name = toCpsContDestName(compiler, k);
    pushIRStmt(compiler, &block->stmts, constDefToStmt(ConstDef{name, expr}));

    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(block, k.ret.cont, name);
    }

    return name;
}

IRName globalToCPS(Compiler* compiler, IRBlock* block, HRef<Symbol> sym, ToCpsCont k) {
    IRName const name = toCpsContDestName(compiler, k);
    pushIRStmt(compiler, &block->stmts, globalToStmt(IRGlobal{name, sym}));

    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(block, k.ret.cont, name);
    }

    return name;
}

IRName exprToIR(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef expr, ToCpsCont k);

IRName bodyToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef body, ToCpsCont k
) {
    if (!isPair(state, body)) {
        assert(false); // TODO: Proper empty/improper body error
    }
    Pair const* argsPair = HRef<Pair>::fromUnchecked(body).ptr();

    for (;/*ever*/;) {
        ORef const stmt = argsPair->car;
        body = argsPair->cdr;

        if (isEmptyList(state, body)) {
            IRName const bodyName = exprToIR(state, compiler, fn, env, block, stmt, k);

            return bodyName;
        } else if (isPair(state, body)){
            exprToIR(state, compiler, fn, env, block, stmt,
                     ToCpsCont{{}, ToCpsCont::EFF});

            argsPair = HRef<Pair>::fromUnchecked(body).ptr();
        } else {
            assert(false); // TODO: Proper improper args error
        }
    }
}

[[nodiscard]]
bool paramToCPS(
    State const* state, Compiler* compiler,
    IRFn* outerFn, ToCpsEnv const* outerEnv, IRBlock** outerBlock,
    IRFn* fn, ToCpsEnv* fnEnv, IRBlock* entryBlock,
    size_t idx, ORef param
) {
    if (isSymbol(state, param)) {
        HRef<Symbol> const paramSym = HRef<Symbol>::fromUnchecked(param);

        IRName const paramName = renameSymbol(compiler, paramSym);
        pushIRParam(compiler, entryBlock, paramName);
        setSymbolDef(fnEnv, paramSym, ToCpsFrameDef{.name = paramName, .type = ToCpsFrameDef::NAME},
                     BINDINGS_PAR);

        return true;
    } else if (isPair(state, param)) {
        Pair const* const paramPair = HRef<Pair>::fromUnchecked(param).ptr();

        ORef const op = paramPair->car;
        if (!eq(op, state->singletons.ofType.oref())) { return false; }

        ORef anyArgs = paramPair->cdr;
        if (!isPair(state, anyArgs)) { return false; }
        Pair const* args = HRef<Pair>::fromUnchecked(anyArgs).ptr();

        ORef const maybeSym = args->car;
        if (!isSymbol(state, maybeSym)) { return false; }
        HRef<Symbol> const sym = HRef<Symbol>::fromUnchecked(maybeSym);

        anyArgs = args->cdr;
        if (!isPair(state, anyArgs)) { return false; }
        args = HRef<Pair>::fromUnchecked(anyArgs).ptr();

        ORef const type = args->car;

        if (!isEmptyList(state, args->cdr)) { return false; }

        IRName const typeName = exprToIR(state, compiler, outerFn, outerEnv, outerBlock, type,
                                         ToCpsCont{{}, ToCpsCont::VAL});

        // TODO: DRY with symbol branch above:
        IRName const paramName = renameSymbol(compiler, sym);
        pushIRParam(compiler, entryBlock, paramName);
        setSymbolDef(fnEnv, sym, ToCpsFrameDef{.name = paramName, .type = ToCpsFrameDef::NAME},
                     BINDINGS_PAR);
        setParamType(compiler, &fn->domain, idx, typeName);

        return true;
    } else {
        return false;
    }
}

IRName fnToCPSimpl(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    IRName maybeSelf, ORef params, ORef body, ToCpsCont k
) {
    ORef const maybeName = toCpsContDestSymbol(k);
    IRFn innerFn = createIRFn(compiler, maybeName);

    IRBlock* entryBlock = createIRBlock(compiler, &innerFn, 0);

    ToCpsEnv fnEnv = createToCpsEnv(env);
    IRName const self = irNameIsValid(maybeSelf) ? maybeSelf : freshName(compiler);
    pushIRParam(compiler, entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(compiler, entryBlock, ret);

    size_t arity = 0;

    for (; !isEmptyList(state, params); ++arity) {
        // TODO: Is this just bad syntax design?:
        // Has to be first because `(x y . (: zs <t>))` = `(x y : zs <t>)`:
        if (paramToCPS(state, compiler, fn, env, block,
                       &innerFn, &fnEnv, entryBlock, arity, params)
            ) {
            innerFn.hasVarArg = true;
            break;
        }

        if (isPair(state, params)) {
            Pair const* const paramsPair = HRef<Pair>::fromUnchecked(params).ptr();

            if (!paramToCPS(state, compiler, fn, env, block,
                            &innerFn, &fnEnv, entryBlock, arity, paramsPair->car)
                ) {
                assert(false); // TODO: Proper param error
            }
            params = paramsPair->cdr;
            continue;
        }

        assert(false); // TODO: Proper improper params error
    }

    completeIRDomain(compiler, &innerFn.domain, arity);

    ToCpsCont const retK = {{.ret = {.cont = ret}}, ToCpsCont::RETURN};
    // Body is in tail position so discard the returned `IRName`:
    bodyToCPS(state, compiler, &innerFn, &fnEnv, &entryBlock, body, retK);
    freeToCpsEnv(&fnEnv);

    IRName const name = toCpsContDestName(compiler, k);
    Args* const closes = (Args*)amalloc(&compiler->arena, sizeof *closes);
    *closes = createArgs(compiler);
    pushIRStmt(compiler, &(*block)->stmts,
               IRStmt{.methodDef = {name, innerFn, closes}, .type = IRStmt::METHOD_DEF});

    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(*block, k.ret.cont, name);
    }

    return name;
}

IRName fnToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    if (!isPair(state, args)) {
        assert(false); // TODO: Proper args error (`(fn)`)
    }
    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const params = argsPair->car;
    ORef const body = argsPair->cdr;
    return fnToCPSimpl(state, compiler, fn, env, block, invalidIRName, params, body, k);
}

IRName ifToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    // OPTIMIZE: Avoid creating `goto`s to `goto`s:

    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    Pair const* argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const cond = argsPair->car;
    args = argsPair->cdr;
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const conseq = argsPair->car;
    args = argsPair->cdr;
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const alt = argsPair->car;
    if (!isEmptyList(state, argsPair->cdr)) {
        assert(false); // TODO
    }

    ToCpsCont const splitK = ToCpsCont{{}, ToCpsCont::VAL};
    IRName const condName = exprToIR(state, compiler, fn, env, block, cond, splitK);
        // Will patch targets shortly:
    IRIf* ifTransfer = createIRIf(*block, condName, IRLabel{}, IRLabel{});
    IRLabel const ifLabel = (*block)->label;
    ToCpsCont const joinK = k.type != ToCpsCont::RETURN
        ? ToCpsCont{{}, ToCpsCont::JOIN}
        : k;

    IRBlock* conseqBlock = createIRBlock(compiler, fn, 1);
    pushCaller(conseqBlock, ifLabel);
    ifTransfer->conseq = conseqBlock->label;
    IRName const conseqName =
        exprToIR(state, compiler, fn, env, &conseqBlock, conseq, joinK);

    IRBlock* altBlock = createIRBlock(compiler, fn, 1);
    pushCaller(altBlock, ifLabel);
    ifTransfer->alt = altBlock->label;
    IRName const altName = exprToIR(state, compiler, fn, env, &altBlock, alt, joinK);

    if (k.type != ToCpsCont::RETURN) {
        // FIXME: If we avoid `goto`s to `goto`s, 2 might not suffice:
        IRBlock* const joinBlock = createIRBlock(compiler, fn, 2);
        IRName const phi = toCpsContDestName(compiler, k);
        pushIRParam(compiler, joinBlock, phi);

        createIRGoto(compiler, conseqBlock, joinBlock->label, conseqName);
        pushCaller(joinBlock, conseqBlock->label);

        createIRGoto(compiler, altBlock, joinBlock->label, altName);
        pushCaller(joinBlock, altBlock->label);

        *block = joinBlock;
        return phi;
    } else {
        return condName; // Arbitrary value, will not be used by callee
    }
}

IRName quoteToCPS(
    State const* state, Compiler* compiler, IRBlock** block, ORef args, ToCpsCont k
) {
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    if (!isEmptyList(state, argsPair->cdr)) {
        assert(false); // TODO
    }

    return constToCPS(compiler, *block, argsPair->car, k);
}

IRName defToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    Pair const* argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const pat = argsPair->car;
    if (!isSymbol(state, pat)) {
        assert(false); // TODO
    }
    HRef<Symbol> const name = HRef<Symbol>::fromUnchecked(pat);
    args = argsPair->cdr;
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    ORef const val = argsPair->car;
    if (!isEmptyList(state, argsPair->cdr)) {
        assert(false); // TODO
    }

    IRName const nameHint = renameSymbol(compiler, name);
    ToCpsCont const defK =
        ToCpsCont{.def = {.name = nameHint, .sym = name}, .type = ToCpsCont::DEF};
    IRName const valName = exprToIR(state, compiler, fn, env, block, val, defK);
    pushIRStmt(compiler, &(*block)->stmts, globalDefToStmt(GlobalDef{name, valName}));
    // FIXME: Return e.g. nil/undefined/unspecified instead of new val:
    IRName const resName = valName;
    if (k.type == ToCpsCont::RETURN) {
        createIRReturn(*block, k.ret.cont, resName);
    }

    return resName;
}

IRName letToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    ToCpsEnv letEnv = createToCpsEnv(env);

    if (!isPair(state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

    for (ORef bindings = argsPair->car;;) {
        if (isPair(state, bindings)) {
            Pair const* const bindingsPair = HRef<Pair>::fromUnchecked(bindings).ptr();

            ORef const binding = bindingsPair->car;
            if (!isPair(state, binding)) {
                assert(false); // TODO: Proper invalid binding error
            }
            Pair const* const bindingPair = HRef<Pair>::fromUnchecked(binding).ptr();

            ORef const pat = bindingPair->car;
            if (!isSymbol(state, pat)) {
                assert(false); // TODO: Proper invalid binder error
            }
            HRef<Symbol> const binder = HRef<Symbol>::fromUnchecked(pat);

            ORef const bindingArgs = bindingPair->cdr;
            if (!isPair(state, bindingArgs)) {
                assert(false); // TODO: Proper invalid binding error
            }
            Pair const* const bindingArgsPair = HRef<Pair>::fromUnchecked(bindingArgs).ptr();

            ORef const val = bindingArgsPair->car;

            if (!isEmptyList(state, bindingArgsPair->cdr)) {
                assert(false); // TODO: Proper invalid binding error
            }

            IRName const binderName = renameSymbol(compiler, binder);
            ToCpsCont const valK =
                ToCpsCont{.def = {.name = binderName, .sym = binder}, .type = ToCpsCont::BIND};
            IRName const finalName =
                exprToIR(state, compiler, fn, &letEnv, block, val, valK);
            // If `finalName != binderName` we have a local copy e.g.
            // `(let ((x 5) (y x)) ...)` and `useToCPS` emitted nothing. Putting
            // `finalName` to env implements the rest of copy propagation:
            setSymbolDef(&letEnv, binder,
                         ToCpsFrameDef{.name = finalName, .type = ToCpsFrameDef::NAME},
                         BINDINGS_SEQ);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(state, bindings)) {
            break;
        } else {
            assert(false); // TODO: Proper invalid bindings error
        }
    }

    IRName const bodyName = bodyToCPS(state, compiler, fn, &letEnv, block, argsPair->cdr, k);
    freeToCpsEnv(&letEnv);
    return bodyName;
}

void knotCreation(
    State const* state, Compiler* compiler, IRBlock* block, ToCpsEnv* letfnEnv, ORef binding
) {
    if (!isPair(state, binding)) {
        assert(false); // TODO: Proper invalid binding error
    }
    Pair const* const bindingPair = HRef<Pair>::fromUnchecked(binding).ptr(); // `((f x) ...)`

    ORef const binder = bindingPair->car;
    if (!isPair(state, binder)) {
        assert(false); // TODO: Proper invalid binder error
    }
    Pair const* const binderPair = HRef<Pair>::fromUnchecked(binder).ptr(); // `(f x)`

    ORef const pat = binderPair->car;
    if (!isSymbol(state, pat)) {
        assert(false); // TODO: Proper invalid fn name error
    }
    HRef<Symbol> const fSym = HRef<Symbol>::fromUnchecked(pat); // `f`

    IRName const knotName = renameSymbol(compiler, fSym);
    pushIRStmt(compiler, &block->stmts, IRStmt{.knot = {.name = knotName}, .type = IRStmt::KNOT});
    setSymbolDef(letfnEnv, fSym, ToCpsFrameDef{.knotName = knotName, .type = ToCpsFrameDef::KNOT},
                 BINDINGS_PAR);
}

ToCpsEnv knotCreations(
    State const* state, Compiler* compiler, IRBlock* block, ToCpsEnv const* env, ORef bindings
) {
    ToCpsEnv innerEnv = createToCpsEnv(env);

    for (;/*ever*/;) {
        if (isPair(state, bindings)) {
            Pair const* const bindingsPair = HRef<Pair>::fromUnchecked(bindings).ptr();

            knotCreation(state, compiler, block, &innerEnv, bindingsPair->car);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(state, bindings)) {
            return innerEnv;
        } else {
            assert(false); // TODO: Proper invalid bindings error
        }
    }
}

void knotInit(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv* env, IRBlock** block, ORef binding
) {
    if (!isPair(state, binding)) {
        assert(false); // TODO: Proper invalid binding error
    }
    Pair const* const bindingPair = HRef<Pair>::fromUnchecked(binding).ptr(); // `((f x) ...)`

    ORef const binder = bindingPair->car;
    if (!isPair(state, binder)) {
        assert(false); // TODO: Proper invalid binder error
    }
    Pair const* const binderPair = HRef<Pair>::fromUnchecked(binder).ptr(); // `(f x)`

    ORef const pat = binderPair->car;
    if (!isSymbol(state, pat)) {
        assert(false); // TODO: Proper invalid fn name error
    }
    HRef<Symbol> const fSym = HRef<Symbol>::fromUnchecked(pat); // `f`

    ToCpsFrameDef const knotDef = useSymbolDef(env, fSym);
    assert(knotDef.type == ToCpsFrameDef::KNOT);
    IRName const knotName = knotDef.knotName;

    IRName const self = renameSymbol(compiler, fSym);
    setSymbolDef(env, fSym, ToCpsFrameDef{.name = self, .type = ToCpsFrameDef::NAME}, BINDINGS_SEQ);
    IRName const fName = renameSymbol(compiler, fSym);
    ToCpsCont const bindK =
        ToCpsCont{.def = {.name = fName, .sym = fSym}, .type = ToCpsCont::BIND};
    // Will just return `fName`, can discard that:
    fnToCPSimpl(state, compiler, fn, env, block, self, binderPair->cdr, bindingPair->cdr, bindK);
    setSymbolDef(env, fSym,
                 ToCpsFrameDef{.name = fName, .type = ToCpsFrameDef::NAME}, BINDINGS_SEQ);

    pushIRStmt(compiler, &(*block)->stmts,
               IRStmt{.knotInit = {.knot = knotName, .v = fName}, .type = IRStmt::KNOT_INIT});
}

void knotInits(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv* env, IRBlock** block, ORef bindings
) {
    for (;/*ever*/;) {
        if (isPair(state, bindings)) {
            Pair const* const bindingsPair = HRef<Pair>::fromUnchecked(bindings).ptr();

            knotInit(state, compiler, fn, env, block, bindingsPair->car);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(state, bindings)) {
            return;
        } else {
            assert(false); // TODO: Proper invalid bindings error (actually unreachable tho)
        }
    }
}

IRName letfnToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    if (!isPair(state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();
    ORef const bindings = argsPair->car;
    ORef const body = argsPair->cdr;

    ToCpsEnv letfnEnv = knotCreations(state, compiler, *block, env, bindings);

    knotInits(state, compiler, fn, &letfnEnv, block, bindings);

    IRName const bodyName = bodyToCPS(state, compiler, fn, &letfnEnv, block, body, k);
    freeToCpsEnv(&letfnEnv);
    return bodyName;
}

IRName callToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef callee, ORef args, ToCpsCont k
) {
    IRName const calleeName =
        exprToIR(state, compiler, fn, env, block, callee, ToCpsCont{{}, ToCpsCont::VAL});
    Args cpsArgs = createArgs(compiler);
    for (;/*ever*/;) {
        if (isPair(state, args)) {
            Pair const* const argsPair = HRef<Pair>::fromUnchecked(args).ptr();

            ORef const arg = argsPair->car;
            IRName const argName =
                exprToIR(state, compiler, fn, env, block, arg, ToCpsCont{{}, ToCpsCont::VAL});
            pushArg(compiler, &cpsArgs, argName);

            args = argsPair->cdr;
        } else if (isEmptyList(state, args)) {
            break;
        } else {
            assert(false); // TODO: proper improper args error
        }
    }

    IRName const retValName = toCpsContDestName(compiler, k);

    if (k.type != ToCpsCont::RETURN) {
        IRBlock* const retBlock = createIRBlock(compiler, fn, 0);
        IRName const frame = freshName(compiler);
        pushIRParam(compiler, retBlock, frame);
        pushIRParam(compiler, retBlock, retValName);

        createCall(*block, calleeName, retBlock->label, createArgs(compiler), cpsArgs);

        *block = retBlock;
    } else {
        createTailcall(*block, calleeName, k.ret.cont, cpsArgs);
    }

    return retValName;
}

IRName useToCPS(
    Compiler* compiler, ToCpsEnv const* env, IRBlock** block, HRef<Symbol> sym, ToCpsCont k
) {
    ToCpsFrameDef const def = useSymbolDef(env, sym);
    switch (def.type) {
    case ToCpsFrameDef::NAME: {
        IRName const name = def.name;

        if (irNameIsValid(name)) {
            if (k.type == ToCpsCont::RETURN) {
                createIRReturn(*block, k.ret.cont, name);
            }

            return name;
        } else {
            return globalToCPS(compiler, *block, sym, k);
        }
    }; break;

    case ToCpsFrameDef::KNOT: {
        IRName const knotName = def.knotName;

        IRName const name = renameIRName(compiler, knotName);
        pushIRStmt(compiler, &(*block)->stmts,
                   IRStmt{.knotGet = {.name = name, .knot = knotName}, .type = IRStmt::KNOT_GET});

        if (k.type == ToCpsCont::RETURN) {
            createIRReturn(*block, k.ret.cont, name);
        }

        return name;
    }; break;
    }

    assert(false); // Unreachable
    return invalidIRName;
}

IRName exprToIR(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef expr, ToCpsCont k
) {
    if (isHeaped(expr)) {
        if (isPair(state, expr)) {
            Pair const* const pair = HRef<Pair>::fromUnchecked(expr).ptr();
            ORef const callee = pair->car;
            ORef const args = pair->cdr;

            if (isSymbol(state, callee)) {
                HRef<Symbol> const calleeSym = HRef<Symbol>::fromUnchecked(callee);

                // OPTIMIZE: Symbol comparisons instead of `strEq`:
                if (strEq(calleeSym.ptr()->name(), Str{"fn", /*HACK:*/2})) {
                    return fnToCPS(state, compiler, fn, env, block, args, k);
                } else if (strEq(calleeSym.ptr()->name(), Str{"if", /*HACK:*/2})) {
                    return ifToCPS(state, compiler, fn, env, block, args, k);
                } else if (strEq(calleeSym.ptr()->name(), Str{"quote", /*HACK:*/5})) {
                    return quoteToCPS(state, compiler, block, args, k);
                } else if (strEq(calleeSym.ptr()->name(), Str{"def", /*HACK:*/3})) {
                    return defToCPS(state, compiler, fn, env, block, args, k);
                } else if (strEq(calleeSym.ptr()->name(), Str{"let", /*HACK:*/3})) {
                    return letToCPS(state, compiler, fn, env, block, args, k);
                } else if (strEq(calleeSym.ptr()->name(), strLit("letfn"))) {
                    return letfnToCPS(state, compiler, fn, env, block, args, k);
                }
            }

            return callToCPS(state, compiler, fn, env, block, callee, args, k);
        } else if (isSymbol(state, expr)) {
            HRef<Symbol> const sym = HRef<Symbol>::fromUnchecked(expr);

            return useToCPS(compiler, env, block, sym, k);
        }
    }

    // Else a constant:
    return constToCPS(compiler, *block, expr, k);
}

IRFn topLevelExprToIR(State const* state, Compiler* compiler, ORef expr) {
    IRFn fn = createIRFn(compiler, Default);

    IRBlock* entryBlock = createIRBlock(compiler, &fn, 0);

    ToCpsEnv env = createToCpsEnv(nullptr);
    IRName const self = freshName(compiler);
    pushIRParam(compiler, entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(compiler, entryBlock, ret);

    ToCpsCont const retK = {{.ret = {.cont = ret}}, ToCpsCont::RETURN};
    exprToIR(state, compiler, &fn, &env, &entryBlock, expr, retK);

    freeToCpsEnv(&env);
    return fn;
}

} // namespace
