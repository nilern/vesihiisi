#include "tocps.h"

#include <string.h>

#include "../state.h"

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
        FRAME_DEF_NAME,
        FRAME_DEF_KNOT
    } type;
} ToCpsFrameDef;

typedef struct ToCpsFrame {
    ORef* keys;
    ToCpsFrameDef* vals;
    size_t count;
    size_t cap;
} ToCpsFrame;

inline static void freeToCpsFrame(ToCpsFrame* frame) { free(frame->keys); }

static ToCpsFrame createToCpsFrame(void) {
    size_t const cap = 2;
    void* const kvs = malloc(cap * (sizeof *(ToCpsFrame){}.keys + sizeof *(ToCpsFrame){}.vals));
    ORef* const keys = (ORef*)kvs;
    memset(keys, 0, cap * sizeof *keys);
    ToCpsFrameDef* const vals = (ToCpsFrameDef*)(keys + cap);

    return (ToCpsFrame){
        .keys = keys,
        .vals = vals,
        .count = 0,
        .cap = cap
    };
}

static void rehashToCpsFrame(ToCpsFrame* frame) {
    size_t const oldCap = frame->cap;
    ORef* const oldKeys = frame->keys;
    ToCpsFrameDef* const oldVals = frame->vals;

    size_t const newCap = oldCap << 1;
    void* const newKvs =
        malloc(newCap * (sizeof *(ToCpsFrame){}.keys + sizeof *(ToCpsFrame){}.vals));
    ORef* const newKeys = (ORef*)newKvs;
    memset(newKeys, 0, newCap * sizeof *newKeys);
    ToCpsFrameDef* const newVals = (ToCpsFrameDef*)(newKeys + newCap);

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const k = oldKeys[i];

        if (!eq(k, toORef(Zero))) {
            size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(uncheckedORefToSymbol(k))->hash);

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const maybeK = newKeys + j;

                if (eq(*maybeK, toORef(Zero))) {
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

static BucketIdx toCpsFrameFindIdx(ToCpsFrame const* frame, SymbolRef sym) {
    size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(sym)->hash);

    size_t const maxIdx = frame->cap - 1;
    for (size_t collisions = 0, i = h & maxIdx;; ++collisions, i = (i + collisions) & maxIdx) {
        ORef const k = frame->keys[i];

        if (eq(k, toORef(sym))) {
            return (BucketIdx){.idx = i, .occupied = true};
        } else if (eq(k, toORef(Zero))) {
            return (BucketIdx){.idx = i, .occupied = false};
        }
    }
}

static ToCpsFrameDef toCpsFrameFind(ToCpsFrame const* frame, SymbolRef sym) {
    BucketIdx const bucketIdx = toCpsFrameFindIdx(frame, sym);
    if (!bucketIdx.occupied) { return (ToCpsFrameDef){.name = invalidIRName, FRAME_DEF_NAME}; }

    return frame->vals[bucketIdx.idx];
}

static void toCpsFrameSet(ToCpsFrame* frame, SymbolRef sym, ToCpsFrameDef def) {
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

        frame->keys[idx] = toORef(sym);
        frame->vals[idx] = def;
        frame->count = newCount;
    }
}

typedef struct ToCpsEnv {
    ToCpsFrame frame;
    struct ToCpsEnv const* parent;
} ToCpsEnv;

inline static void freeToCpsEnv(ToCpsEnv* env) { freeToCpsFrame(&env->frame); }

static ToCpsEnv createToCpsEnv(ToCpsEnv const* parent) {
    return (ToCpsEnv){
        .frame = createToCpsFrame(),
        .parent = parent
    };
}

static ToCpsFrameDef useSymbolDef(ToCpsEnv const* env, SymbolRef sym) {
    for (; env; env = env->parent) {
        ToCpsFrameDef const def = toCpsFrameFind(&env->frame, sym);
        switch (def.type) {
        case FRAME_DEF_NAME: {
            if (irNameIsValid(def.name)) { return def; }
        }; break;

        case FRAME_DEF_KNOT: return def;
        }
    }

    return (ToCpsFrameDef){.name = invalidIRName, .type = FRAME_DEF_NAME};
}

static void setSymbolDef(ToCpsEnv* env, SymbolRef sym, ToCpsFrameDef def, BindingsType type) {
     // FIXME: Proper error (duplicate defs):
    if (type == BINDINGS_PAR) {
        ToCpsFrameDef const oldDef /*HACK:*/ [[maybe_unused]] = toCpsFrameFind(&env->frame, sym);
        assert(oldDef.type != FRAME_DEF_NAME || !irNameIsValid(oldDef.name));
    }

    toCpsFrameSet(&env->frame, sym, def);
}

typedef struct ToCpsContReturn {
    IRName cont;
} ToCpsContReturn;

typedef struct ToCpsCont {
    union {
        ToCpsContReturn ret;
        struct {
            IRName name;
            SymbolRef sym;
        };
    };
    enum {
        TO_CPS_CONT_EFF,
        TO_CPS_CONT_VAL,
        TO_CPS_CONT_BIND,
        TO_CPS_CONT_DEF,
        TO_CPS_CONT_JOIN,
        TO_CPS_CONT_RETURN
    } type;
} ToCpsCont;

static IRName toCpsContDestName(Compiler* compiler, ToCpsCont k) {
    switch (k.type) {
    case TO_CPS_CONT_BIND: // fallthrough
    case TO_CPS_CONT_DEF: return k.name;

    case TO_CPS_CONT_EFF: // fallthrough
    case TO_CPS_CONT_VAL: // fallthrough
    case TO_CPS_CONT_JOIN: // fallthrough
    case TO_CPS_CONT_RETURN: return freshName(compiler);
    }

    return invalidIRName; // Unreachable
}

static ORef toCpsContDestSymbol(ToCpsCont k) {
    switch (k.type) {
    case TO_CPS_CONT_BIND: // fallthrough
    case TO_CPS_CONT_DEF: return toORef(k.sym);

    case TO_CPS_CONT_EFF: // fallthrough
    case TO_CPS_CONT_VAL: // fallthrough
    case TO_CPS_CONT_JOIN: // fallthrough
    case TO_CPS_CONT_RETURN: return toORef(Zero);
    }

    return toORef(Zero); // Unreachable
}

static IRName constToCPS(Compiler* compiler, IRBlock* block, ORef expr, ToCpsCont k) {
    IRName const name = toCpsContDestName(compiler, k);
    pushIRStmt(compiler, &block->stmts, constDefToStmt((ConstDef){name, expr}));

    if (k.type == TO_CPS_CONT_RETURN) {
        createIRReturn(block, k.ret.cont, name);
    }

    return name;
}

static IRName globalToCPS(Compiler* compiler, IRBlock* block, SymbolRef sym, ToCpsCont k) {
    IRName const name = toCpsContDestName(compiler, k);
    pushIRStmt(compiler, &block->stmts, globalToStmt((IRGlobal){name, sym}));

    if (k.type == TO_CPS_CONT_RETURN) {
        createIRReturn(block, k.ret.cont, name);
    }

    return name;
}

static IRName exprToIR(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef expr, ToCpsCont k);

static IRName bodyToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef body, ToCpsCont k
) {
    if (!isPair(state, body)) {
        assert(false); // TODO: Proper empty/improper body error
    }
    Pair const* argsPair = toPtr(uncheckedORefToPair(body));

    for (;/*ever*/;) {
        ORef const stmt = argsPair->car;
        body = argsPair->cdr;

        if (isEmptyList(state, body)) {
            IRName const bodyName = exprToIR(state, compiler, fn, env, block, stmt, k);

            return bodyName;
        } else if (isPair(state, body)){
            exprToIR(state, compiler, fn, env, block, stmt,
                     (ToCpsCont){.type = TO_CPS_CONT_EFF});

            argsPair = toPtr(uncheckedORefToPair(body));
        } else {
            assert(false); // TODO: Proper improper args error
        }
    }
}

[[nodiscard]]
static bool paramToCPS(
    State const* state, Compiler* compiler,
    IRFn* outerFn, ToCpsEnv const* outerEnv, IRBlock** outerBlock,
    IRFn* fn, ToCpsEnv* fnEnv, IRBlock* entryBlock,
    size_t idx, ORef param
) {
    if (isSymbol(state, param)) {
        SymbolRef const paramSym = uncheckedORefToSymbol(param);

        IRName const paramName = renameSymbol(compiler, paramSym);
        pushIRParam(compiler, entryBlock, paramName);
        setSymbolDef(fnEnv, paramSym, (ToCpsFrameDef){.name = paramName, FRAME_DEF_NAME},
                     BINDINGS_PAR);

        return true;
    } else if (isPair(state, param)) {
        Pair const* const paramPair = toPtr(uncheckedORefToPair(param));

        ORef const op = paramPair->car;
        if (!eq(op, toORef(state->ofType))) { return false; }

        ORef anyArgs = paramPair->cdr;
        if (!isPair(state, anyArgs)) { return false; }
        Pair const* args = toPtr(uncheckedORefToPair(anyArgs));

        ORef const maybeSym = args->car;
        if (!isSymbol(state, maybeSym)) { return false; }
        SymbolRef const sym = uncheckedORefToSymbol(maybeSym);

        anyArgs = args->cdr;
        if (!isPair(state, anyArgs)) { return false; }
        args = toPtr(uncheckedORefToPair(anyArgs));

        ORef const type = args->car;

        if (!isEmptyList(state, args->cdr)) { return false; }

        IRName const typeName = exprToIR(state, compiler, outerFn, outerEnv, outerBlock, type,
            (ToCpsCont){.type = TO_CPS_CONT_VAL});

        // TODO: DRY with symbol branch above:
        IRName const paramName = renameSymbol(compiler, sym);
        pushIRParam(compiler, entryBlock, paramName);
        setSymbolDef(fnEnv, sym, (ToCpsFrameDef){.name = paramName, FRAME_DEF_NAME}, BINDINGS_PAR);
        setParamType(compiler, &fn->domain, idx, typeName);

        return true;
    } else {
        return false;
    }
}

static IRName fnToCPSimpl(
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
            Pair const* const paramsPair = pairToPtr(uncheckedORefToPair(params));

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

    ToCpsCont const retK = {{.ret = {.cont = ret}}, TO_CPS_CONT_RETURN};
    // Body is in tail position so discard the returned `IRName`:
    bodyToCPS(state, compiler, &innerFn, &fnEnv, &entryBlock, body, retK);
    freeToCpsEnv(&fnEnv);

    IRName const name = toCpsContDestName(compiler, k);
    Args* const closes = amalloc(&compiler->arena, sizeof *closes);
    *closes = createArgs(compiler);
    pushIRStmt(compiler, &(*block)->stmts,
               (IRStmt){.methodDef = {name, innerFn, closes}, STMT_METHOD_DEF});

    if (k.type == TO_CPS_CONT_RETURN) {
        createIRReturn(*block, k.ret.cont, name);
    }

    return name;
}

static IRName fnToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    if (!isPair(state, args)) {
        assert(false); // TODO: Proper args error (`(fn)`)
    }
    Pair const* const argsPair = pairToPtr(uncheckedORefToPair(args));

    ORef const params = argsPair->car;
    ORef const body = argsPair->cdr;
    return fnToCPSimpl(state, compiler, fn, env, block, invalidIRName, params, body, k);
}

static IRName ifToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    // OPTIMIZE: Avoid creating `goto`s to `goto`s:

    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    Pair const* argsPair = pairToPtr(uncheckedORefToPair(args));

    ORef const cond = argsPair->car;
    args = argsPair->cdr;
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    argsPair = pairToPtr(uncheckedORefToPair(args));

    ORef const conseq = argsPair->car;
    args = argsPair->cdr;
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    argsPair = pairToPtr(uncheckedORefToPair(args));

    ORef const alt = argsPair->car;
    if (!isEmptyList(state, argsPair->cdr)) {
        assert(false); // TODO
    }

    ToCpsCont const splitK = (ToCpsCont){{}, TO_CPS_CONT_VAL};
    IRName const condName = exprToIR(state, compiler, fn, env, block, cond, splitK);
        // Will patch targets shortly:
    IRIf* ifTransfer = createIRIf(*block, condName, (IRLabel){}, (IRLabel){});
    IRLabel const ifLabel = (*block)->label;
    ToCpsCont const joinK = k.type != TO_CPS_CONT_RETURN
        ? (ToCpsCont){{}, TO_CPS_CONT_JOIN}
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

    if (k.type != TO_CPS_CONT_RETURN) {
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

static IRName quoteToCPS(
    State const* state, Compiler* compiler, IRBlock** block, ORef args, ToCpsCont k
) {
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    Pair const* const argsPair = pairToPtr(uncheckedORefToPair(args));

    if (!isEmptyList(state, argsPair->cdr)) {
        assert(false); // TODO
    }

    return constToCPS(compiler, *block, argsPair->car, k);
}

static IRName defToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    Pair const* argsPair = pairToPtr(uncheckedORefToPair(args));

    ORef const pat = argsPair->car;
    if (!isSymbol(state, pat)) {
        assert(false); // TODO
    }
    SymbolRef const name = uncheckedORefToSymbol(pat);
    args = argsPair->cdr;
    if (!isPair(state, args)) {
        assert(false); // TODO
    }
    argsPair = pairToPtr(uncheckedORefToPair(args));

    ORef const val = argsPair->car;
    if (!isEmptyList(state, argsPair->cdr)) {
        assert(false); // TODO
    }

    IRName const nameHint = renameSymbol(compiler, name);
    ToCpsCont const defK = (ToCpsCont){{.name = nameHint, .sym = name}, TO_CPS_CONT_DEF};
    IRName const valName = exprToIR(state, compiler, fn, env, block, val, defK);
    pushIRStmt(compiler, &(*block)->stmts, globalDefToStmt((GlobalDef){name, valName}));
    // FIXME: Return e.g. nil/undefined/unspecified instead of new val:
    IRName const resName = valName;
    if (k.type == TO_CPS_CONT_RETURN) {
        createIRReturn(*block, k.ret.cont, resName);
    }

    return resName;
}

static IRName letToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    ToCpsEnv letEnv = createToCpsEnv(env);

    if (!isPair(state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    Pair const* const argsPair = pairToPtr(uncheckedORefToPair(args));

    for (ORef bindings = argsPair->car;;) {
        if (isPair(state, bindings)) {
            Pair const* const bindingsPair = pairToPtr(uncheckedORefToPair(bindings));

            ORef const binding = bindingsPair->car;
            if (!isPair(state, binding)) {
                assert(false); // TODO: Proper invalid binding error
            }
            Pair const* const bindingPair =
                pairToPtr(uncheckedORefToPair(binding));

            ORef const pat = bindingPair->car;
            if (!isSymbol(state, pat)) {
                assert(false); // TODO: Proper invalid binder error
            }
            SymbolRef const binder = uncheckedORefToSymbol(pat);

            ORef const bindingArgs = bindingPair->cdr;
            if (!isPair(state, bindingArgs)) {
                assert(false); // TODO: Proper invalid binding error
            }
            Pair const* const bindingArgsPair =
                pairToPtr(uncheckedORefToPair(bindingArgs));

            ORef const val = bindingArgsPair->car;

            if (!isEmptyList(state, bindingArgsPair->cdr)) {
                assert(false); // TODO: Proper invali [[maybe_unused]]d binding error
            }

            IRName const binderName = renameSymbol(compiler, binder);
            ToCpsCont const valK =
                (ToCpsCont){{.name = binderName, .sym = binder}, .type = TO_CPS_CONT_BIND};
            IRName const finalName =
                exprToIR(state, compiler, fn, &letEnv, block, val, valK);
            // If `finalName != binderName` we have a local copy e.g.
            // `(let ((x 5) (y x)) ...)` and `useToCPS` emitted nothing. Putting
            // `finalName` to env implements the rest of copy propagation:
            setSymbolDef(&letEnv, binder, (ToCpsFrameDef){.name = finalName, FRAME_DEF_NAME},
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

static void knotCreation(
    State const* state, Compiler* compiler, IRBlock* block, ToCpsEnv* letfnEnv, ORef binding
) {
    if (!isPair(state, binding)) {
        assert(false); // TODO: Proper invalid binding error
    }
    Pair const* const bindingPair = toPtr(uncheckedORefToPair(binding)); // `((f x) ...)`

    ORef const binder = bindingPair->car;
    if (!isPair(state, binder)) {
        assert(false); // TODO: Proper invalid binder error
    }
    Pair const* const binderPair = toPtr(uncheckedORefToPair(binder)); // `(f x)`

    ORef const pat = binderPair->car;
    if (!isSymbol(state, pat)) {
        assert(false); // TODO: Proper invalid fn name error
    }
    SymbolRef const fSym = uncheckedORefToSymbol(pat); // `f`

    IRName const knotName = renameSymbol(compiler, fSym);
    pushIRStmt(compiler, &block->stmts, (IRStmt){.knot = {.name = knotName}, STMT_KNOT});
    setSymbolDef(letfnEnv, fSym, (ToCpsFrameDef){.knotName = knotName, FRAME_DEF_KNOT},
                 BINDINGS_PAR);
}

static ToCpsEnv knotCreations(
    State const* state, Compiler* compiler, IRBlock* block, ToCpsEnv const* env, ORef bindings
) {
    ToCpsEnv innerEnv = createToCpsEnv(env);

    for (;/*ever*/;) {
        if (isPair(state, bindings)) {
            Pair const* const bindingsPair = toPtr(uncheckedORefToPair(bindings));

            knotCreation(state, compiler, block, &innerEnv, bindingsPair->car);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(state, bindings)) {
            return innerEnv;
        } else {
            assert(false); // TODO: Proper invalid bindings error
        }
    }
}

static void knotInit(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv* env, IRBlock** block, ORef binding
) {
    if (!isPair(state, binding)) {
        assert(false); // TODO: Proper invalid binding error
    }
    Pair const* const bindingPair = toPtr(uncheckedORefToPair(binding)); // `((f x) ...)`

    ORef const binder = bindingPair->car;
    if (!isPair(state, binder)) {
        assert(false); // TODO: Proper invalid binder error
    }
    Pair const* const binderPair = toPtr(uncheckedORefToPair(binder)); // `(f x)`

    ORef const pat = binderPair->car;
    if (!isSymbol(state, pat)) {
        assert(false); // TODO: Proper invalid fn name error
    }
    SymbolRef const fSym = uncheckedORefToSymbol(pat); // `f`

    ToCpsFrameDef const knotDef = useSymbolDef(env, fSym);
    assert(knotDef.type == FRAME_DEF_KNOT);
    IRName const knotName = knotDef.knotName;

    IRName const self = renameSymbol(compiler, fSym);
    setSymbolDef(env, fSym, (ToCpsFrameDef){.name = self, FRAME_DEF_NAME}, BINDINGS_SEQ);
    IRName const fName = renameSymbol(compiler, fSym);
    ToCpsCont const bindK = (ToCpsCont){{.name = fName, .sym = fSym}, .type = TO_CPS_CONT_BIND};
    // Will just return `fName`, can discard that:
    fnToCPSimpl(state, compiler, fn, env, block, self, binderPair->cdr, bindingPair->cdr, bindK);
    setSymbolDef(env, fSym, (ToCpsFrameDef){.name = fName, FRAME_DEF_NAME}, BINDINGS_SEQ);

    pushIRStmt(compiler, &(*block)->stmts,
               (IRStmt){.knotInit = {.knot = knotName, .v = fName}, STMT_KNOT_INIT});
}

static void knotInits(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv* env, IRBlock** block, ORef bindings
) {
    for (;/*ever*/;) {
        if (isPair(state, bindings)) {
            Pair const* const bindingsPair = toPtr(uncheckedORefToPair(bindings));

            knotInit(state, compiler, fn, env, block, bindingsPair->car);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(state, bindings)) {
            return;
        } else {
            assert(false); // TODO: Proper invalid bindings error (actually unreachable tho)
        }
    }
}

static IRName letfnToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    if (!isPair(state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    Pair const* const argsPair = pairToPtr(uncheckedORefToPair(args));
    ORef const bindings = argsPair->car;
    ORef const body = argsPair->cdr;

    ToCpsEnv letfnEnv = knotCreations(state, compiler, *block, env, bindings);

    knotInits(state, compiler, fn, &letfnEnv, block, bindings);

    IRName const bodyName = bodyToCPS(state, compiler, fn, &letfnEnv, block, body, k);
    freeToCpsEnv(&letfnEnv);
    return bodyName;
}

static IRName callToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef callee, ORef args, ToCpsCont k
) {
    IRName const calleeName =
        exprToIR(state, compiler, fn, env, block, callee,
                 (ToCpsCont){.type = TO_CPS_CONT_VAL});
    Args cpsArgs = createArgs(compiler);
    for (;/*ever*/;) {
        if (isPair(state, args)) {
            Pair const* const argsPair = pairToPtr(uncheckedORefToPair(args));

            ORef const arg = argsPair->car;
            IRName const argName =
                exprToIR(state, compiler, fn, env, block, arg,
                         (ToCpsCont){.type = TO_CPS_CONT_VAL});
            pushArg(compiler, &cpsArgs, argName);

            args = argsPair->cdr;
        } else if (isEmptyList(state, args)) {
            break;
        } else {
            assert(false); // TODO: proper improper args error
        }
    }

    IRName const retValName = toCpsContDestName(compiler, k);

    if (k.type != TO_CPS_CONT_RETURN) {
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

static IRName useToCPS(
    Compiler* compiler, ToCpsEnv const* env, IRBlock** block, SymbolRef sym, ToCpsCont k
) {
    ToCpsFrameDef const def = useSymbolDef(env, sym);
    switch (def.type) {
    case FRAME_DEF_NAME: {
        IRName const name = def.name;

        if (irNameIsValid(name)) {
            if (k.type == TO_CPS_CONT_RETURN) {
                createIRReturn(*block, k.ret.cont, name);
            }

            return name;
        } else {
            return globalToCPS(compiler, *block, sym, k);
        }
    }; break;

    case FRAME_DEF_KNOT: {
        IRName const knotName = def.knotName;

        IRName const name = renameIRName(compiler, knotName);
        pushIRStmt(compiler, &(*block)->stmts,
                   (IRStmt){.knotGet = {.name = name, .knot = knotName}, STMT_KNOT_GET});

        if (k.type == TO_CPS_CONT_RETURN) {
            createIRReturn(*block, k.ret.cont, name);
        }

        return name;
    }; break;
    }

    assert(false); // Unreachable
    return invalidIRName;
}

static IRName exprToIR(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef expr, ToCpsCont k
) {
    if (isHeaped(expr)) {
        if (isPair(state, expr)) {
            Pair const* const pair = pairToPtr(uncheckedORefToPair(expr));
            ORef const callee = pair->car;
            ORef const args = pair->cdr;

            if (isSymbol(state, callee)) {
                SymbolRef const calleeSym = uncheckedORefToSymbol(callee);

                // OPTIMIZE: Symbol comparisons instead of `strEq`:
                if (strEq(symbolName(calleeSym), (Str){"fn", /*HACK:*/2})) {
                    return fnToCPS(state, compiler, fn, env, block, args, k);
                } else if (strEq(symbolName(calleeSym), (Str){"if", /*HACK:*/2})) {
                    return ifToCPS(state, compiler, fn, env, block, args, k);
                } else if (strEq(symbolName(calleeSym), (Str){"quote", /*HACK:*/5})) {
                    return quoteToCPS(state, compiler, block, args, k);
                } else if (strEq(symbolName(calleeSym), (Str){"def", /*HACK:*/3})) {
                    return defToCPS(state, compiler, fn, env, block, args, k);
                } else if (strEq(symbolName(calleeSym), (Str){"let", /*HACK:*/3})) {
                    return letToCPS(state, compiler, fn, env, block, args, k);
                } else if (strEq(symbolName(calleeSym), strLit("letfn"))) {
                    return letfnToCPS(state, compiler, fn, env, block, args, k);
                }
            }

            return callToCPS(state, compiler, fn, env, block, callee, args, k);
        } else if (isSymbol(state, expr)) {
            SymbolRef const sym = uncheckedORefToSymbol(expr);

            return useToCPS(compiler, env, block, sym, k);
        }
    }

    // Else a constant:
    return constToCPS(compiler, *block, expr, k);
}

static IRFn topLevelExprToIR(State const* state, Compiler* compiler, ORef expr) {
    IRFn fn = createIRFn(compiler, toORef(Zero));

    IRBlock* entryBlock = createIRBlock(compiler, &fn, 0);

    ToCpsEnv env = createToCpsEnv(nullptr);
    IRName const self = freshName(compiler);
    pushIRParam(compiler, entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(compiler, entryBlock, ret);

    ToCpsCont const retK = {{.ret = {.cont = ret}}, TO_CPS_CONT_RETURN};
    exprToIR(state, compiler, &fn, &env, &entryBlock, expr, retK);

    freeToCpsEnv(&env);
    return fn;
}
