#include <string.h>

#include "compiler.h"
#include "state.h"

typedef enum BindingsType {
    BINDINGS_PAR,
    BINDINGS_SEQ
} BindingsType;

typedef struct ToCpsFrame {
    ORef* keys;
    IRName* vals;
    size_t count;
    size_t cap;
} ToCpsFrame;

inline static void freeToCpsFrame(ToCpsFrame* frame) { free(frame->keys); }

static ToCpsFrame createToCpsFrame(void) {
    size_t const cap = 2;
    void* const kvs = malloc(cap * (sizeof *(ToCpsFrame){}.keys + sizeof *(ToCpsFrame){}.vals));
    ORef* const keys = (ORef*)kvs;
    memset(keys, 0, cap * sizeof *keys);
    IRName* const vals = (IRName*)(keys + cap);

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
    IRName* const oldVals = frame->vals;

    size_t const newCap = oldCap << 1;
    void* const newKvs =
        malloc(newCap * (sizeof *(ToCpsFrame){}.keys + sizeof *(ToCpsFrame){}.vals));
    ORef* const newKeys = (ORef*)newKvs;
    memset(newKeys, 0, newCap * sizeof *newKeys);
    IRName* const newVals = (IRName*)(newKeys + newCap);

    for (size_t i = 0; i < oldCap; ++i) {
        ORef const k = oldKeys[i];

        if (!eq(k, fixnumToORef(Zero))) {
            size_t const h = (uintptr_t)fixnumToInt(symbolToPtr(uncheckedORefToSymbol(k))->hash);

            size_t const maxIndex = newCap - 1;
            for (size_t collisions = 0, j = h & maxIndex;;
                ++collisions, j = (j + collisions) & maxIndex
            ) {
                ORef* const maybeK = newKeys + j;

                if (eq(*maybeK, fixnumToORef(Zero))) {
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

        if (eq(k, symbolToORef(sym))) {
            return (BucketIdx){.idx = i, .occupied = true};
        } else if (eq(k, fixnumToORef(Zero))) {
            return (BucketIdx){.idx = i, .occupied = false};
        }
    }
}

static IRName toCpsFrameFind(ToCpsFrame const* frame, SymbolRef sym) {
    BucketIdx const bucketIdx = toCpsFrameFindIdx(frame, sym);
    if (!bucketIdx.occupied) { return invalidIRName; }

    return frame->vals[bucketIdx.idx];
}

static void toCpsFrameSet(ToCpsFrame* frame, SymbolRef sym, IRName name) {
    BucketIdx bucketIdx = toCpsFrameFindIdx(frame, sym);
    if (bucketIdx.occupied) {
        frame->vals[bucketIdx.idx] = name;
    } else {
        size_t idx = bucketIdx.idx;

        size_t const newCount = frame->count + 1;
        if (newCount > (frame->cap >> 1)) {
            rehashToCpsFrame(frame);
            bucketIdx = toCpsFrameFindIdx(frame, sym);
            assert(!bucketIdx.occupied);
            idx = bucketIdx.idx;
        }

        frame->keys[idx] = symbolToORef(sym);
        frame->vals[idx] = name;
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

static IRName useSymbolIRName(ToCpsEnv const* env, SymbolRef sym) {
    for (; env; env = env->parent) {
        IRName const name = toCpsFrameFind(&env->frame, sym);
        if (irNameIsValid(name)) { return name; }
    }

    return invalidIRName;
}

static IRName defSymbolIRName(ToCpsEnv* env, SymbolRef sym, IRName name, BindingsType type) {
     // FIXME: Proper error (duplicate defs):
    if (type == BINDINGS_PAR) {
        assert(!irNameIsValid(toCpsFrameFind(&env->frame, sym)));
    }

    toCpsFrameSet(&env->frame, sym, name);

    return name;
}

typedef struct ToCpsContReturn {
    IRName cont;
} ToCpsContReturn;

typedef struct ToCpsCont {
    union {
        ToCpsContReturn ret;
        IRName name;
    };
    enum {
        TO_CPS_CONT_VAL,
        TO_CPS_CONT_BIND,
        TO_CPS_CONT_RETURN
    } type;
} ToCpsCont;

static IRName toCpsContDestName(Compiler* compiler, ToCpsCont k) {
    switch (k.type) {
    case TO_CPS_CONT_BIND: return k.name;

    case TO_CPS_CONT_VAL: // fallthrough
    case TO_CPS_CONT_RETURN: return freshName(compiler);
    }

    return invalidIRName; // Unreachable
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
        defSymbolIRName(fnEnv, paramSym, paramName, BINDINGS_PAR);

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
        defSymbolIRName(fnEnv, sym, paramName, BINDINGS_PAR);
        setParamType(compiler, &fn->domain, idx, typeName);

        return true;
    } else {
        return false;
    }
}

static IRName fnToCPS(
    State const* state, Compiler* compiler, IRFn* fn, ToCpsEnv const* env, IRBlock** block,
    ORef args, ToCpsCont k
) {
    IRFn innerFn = createIRFn(compiler);

    IRBlock* entryBlock = createIRBlock(compiler, &innerFn, 0);

    ToCpsEnv fnEnv = createToCpsEnv(env);
    IRName const self = freshName(compiler);
    pushIRParam(compiler, entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(compiler, entryBlock, ret);

    if (!isPair(state, args)) {
        assert(false); // TODO: Proper args error (`(fn)`)
    }
    Pair const* argsPair = pairToPtr(uncheckedORefToPair(args));

    size_t arity = 0;

    for (ORef params = argsPair->car; !isEmptyList(state, params); ++arity) {
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

    args = argsPair->cdr;
    if (!isPair(state, args)) {
        assert(false); // TODO: Proper args error (missing body)
    }
    argsPair = pairToPtr(uncheckedORefToPair(args));

    ORef const body = argsPair->car;

    if (!isEmptyList(state, argsPair->cdr)) {
        assert(false); // TODO: Proper args error or implement body stmts
    }

    ToCpsCont const retK = {{.ret = {.cont = ret}}, TO_CPS_CONT_RETURN};
    exprToIR(state, compiler, &innerFn, &fnEnv, &entryBlock, body, retK);
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

    IRBlock* conseqBlock = createIRBlock(compiler, fn, 1);
    pushCaller(conseqBlock, ifLabel);
    ifTransfer->conseq = conseqBlock->label;
    IRName const conseqName =
        exprToIR(state, compiler, fn, env, &conseqBlock, conseq, k);

    IRBlock* altBlock = createIRBlock(compiler, fn, 1);
    pushCaller(altBlock, ifLabel);
    ifTransfer->alt = altBlock->label;
    IRName const altName = exprToIR(state, compiler, fn, env, &altBlock, alt, k);

    if (k.type != TO_CPS_CONT_RETURN) {
        // FIXME: If we avoid `goto`s to `goto`s, 2 might not suffice:
        IRBlock* const joinBlock = createIRBlock(compiler, fn, 2);
        // FIXME: If `k` provides a target name it should be used for `phi`. Now it
        // gets pushed into the branches and possibly multiply defined:
        IRName const phi = freshName(compiler);
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

    ToCpsCont const defK = (ToCpsCont){{}, TO_CPS_CONT_VAL};
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
    Pair const* argsPair = pairToPtr(uncheckedORefToPair(args));

    for (ORef bindings = argsPair->car;;) {
        if (isPair(state, bindings)) {
            Pair const* const bindingsPair =
                pairToPtr(uncheckedORefToPair(bindings));

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
            ToCpsCont const valK = (ToCpsCont){
                .name = binderName,
                .type = TO_CPS_CONT_BIND
            };
            IRName const finalName =
                exprToIR(state, compiler, fn, &letEnv, block, val, valK);
            // If `finalName != binderName` we have a local copy e.g.
            // `(let ((x 5) (y x)) ...)` and `exprToIR` emitted nothing. Putting
            // `finalName` to env implements the rest of copy propagation:
            defSymbolIRName(&letEnv, binder, finalName, BINDINGS_SEQ);

            bindings = bindingsPair->cdr;
        } else if (isEmptyList(state, bindings)) {
            break;
        } else {
            assert(false); // TODO: Proper invalid bindings error
        }
    }

    args = argsPair->cdr;
    if (!isPair(state, args)) {
        assert(false); // TODO: Proper invalid args error
    }
    argsPair = pairToPtr(uncheckedORefToPair(args));

    ORef const body = argsPair->car;

    if (!isEmptyList(state, argsPair->cdr)) {
        assert(false); // TODO: Proper invalid args error
    }

    IRName const bodyName = exprToIR(state, compiler, fn, &letEnv, block, body, k);
    freeToCpsEnv(&letEnv);
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
    IRName const name = useSymbolIRName(env, sym);
    if (irNameIsValid(name)) {
        if (k.type == TO_CPS_CONT_RETURN) {
            createIRReturn(*block, k.ret.cont, name);
        }

        return name;
    } else {
        return globalToCPS(compiler, *block, sym, k);
    }
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
    IRFn fn = createIRFn(compiler);

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
