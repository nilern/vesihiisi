typedef struct IRName { size_t index; } IRName;

static const IRName invalidIRName = {0};

inline static bool irNameIsValid(IRName name) { return name.index != invalidIRName.index; }

inline static bool irNameEq(IRName name1, IRName name2) { return name1.index == name2.index; }

// OPTIMIZE: As simple and fast as possible but actually a terrible hash:
inline static size_t irNameHash(IRName name) { return name.index; }

typedef struct IRLabel { size_t blockIndex; } IRLabel;

typedef struct IRConst { uint8_t index; } IRConst;

typedef struct Compiler {
    Arena arena;
    ORef* nameSyms;
    size_t nameCount;
    size_t nameCap;
} Compiler;

static Compiler createCompiler(void) {
    size_t const nameCap = 2;
    ORef* const nameSyms = malloc(nameCap * sizeof *nameSyms);

    // Reserve (IRName){0} as invalid:
    size_t const nameCount = 1;
    nameSyms[0] = fixnumToORef(Zero);
    
    return (Compiler){
        .arena = newArena(defaultArenaBlockSize),
        .nameSyms = nameSyms,
        .nameCount = nameCount,
        .nameCap = nameCap
    };
}

static void freeCompiler(Compiler* compiler) {
    freeArena(&compiler->arena);
    free(compiler->nameSyms);
}

static IRName renameSymbolImpl(Compiler* compiler, ORef maybeSym) {
    if (compiler->nameCount < compiler->nameCap) {
        size_t const newCap = compiler->nameCap + (compiler->nameCap >> 1);
        compiler->nameSyms = realloc(compiler->nameSyms, newCap * sizeof *compiler->nameSyms);
        compiler->nameCap = newCap;
    }

    size_t const index = compiler->nameCount;
    compiler->nameSyms[compiler->nameCount++] = maybeSym;
    return (IRName){index};
}

inline static IRName renameSymbol(Compiler* compiler, SymbolRef sym) {
    return renameSymbolImpl(compiler, symbolToORef(sym));
}

inline static IRName freshName(Compiler* compiler) {
    return renameSymbolImpl(compiler, fixnumToORef(Zero));
}

inline static IRName renameIRName(Compiler* compiler, IRName name) {
    return renameSymbolImpl(compiler, compiler->nameSyms[name.index]);
}

struct IRBlock;

typedef struct IRFn {
    struct IRBlock** blocks; // OPTIMIZE: `IRBlock* blocks`
    size_t blockCount;
    size_t blockCap;

    ORef* consts;
    uint8_t constCount;
    uint8_t constCap;
} IRFn;

static void markIRBlock(State* state, struct IRBlock* block);

static void markIRFn(State* state, IRFn* fn) {
    {
        size_t const blockCount = fn->blockCount;
        for (size_t i = 0; i < blockCount; ++i) {
            markIRBlock(state, fn->blocks[i]);
        }
    }

    {
        size_t const constCount = fn->constCount;
        for (size_t i = 0; i < constCount; ++i) {
            fn->consts[i] = mark(&state->heap, fn->consts[i]);
        }
    }
}

static void assertIRBlockInTospace(State const* state, struct IRBlock const* block);

static void assertIRFnInTospace(State const* state, IRFn const* fn) {
    {
        size_t const blockCount = fn->blockCount;
        for (size_t i = 0; i < blockCount; ++i) {
            assertIRBlockInTospace(state, fn->blocks[i]);
        }
    }

    {
        size_t const constCount = fn->constCount;
        for (size_t i = 0; i < constCount; ++i) {
            ORef const v = fn->consts[i];
            if (isHeaped(v)) {
                assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
            }
        }
    }
}

typedef struct Args {
    IRName* names;
    size_t count;
    size_t cap;
} Args;

static Args createArgs(Compiler* compiler) {
    size_t const cap = 2;
    IRName* names = amalloc(&compiler->arena, cap * sizeof *names);

    return (Args){.names = names, .count = 0, .cap = cap};
}

static void pushArg(Compiler* compiler, Args* args, IRName arg) {
    if (args->count == args->cap) {
        size_t const newCap = args->cap + args->cap / 2;
        args->names = arealloc(&compiler->arena, args->names, args->cap * sizeof *args->names,
                               newCap * sizeof *args->names);
        args->cap = newCap;
    }

    args->names[args->count++] = arg;
}

typedef struct GlobalDef {
    IRConst name;
    IRName val;
} GlobalDef;

typedef struct IRGlobal {
    IRName tmpName;
    IRConst name;
} IRGlobal;

typedef struct ConstDef {
    IRName name;
    IRConst v;
} ConstDef;

typedef struct Clover {
    IRName name;
    IRName closure;
    IRName origName;
    uint8_t idx;
} Clover;

typedef struct FnDef {
    IRName name;
    IRFn fn;
    IRConst v;
    Args closes;
} FnDef;

typedef struct MoveStmt {
    IRName dest;
    IRName src;
} MoveStmt;

typedef struct SwapStmt {
    IRName reg1;
    IRName reg2;
} SwapStmt;

typedef struct IRStmt {
    union {
        GlobalDef globalDef;
        IRGlobal global;
        ConstDef constDef;
        Clover clover;
        FnDef fnDef;
        MoveStmt mov;
        SwapStmt swap;
    };
    enum IRStmtType {
        STMT_GLOBAL_DEF,
        STMT_GLOBAL,
        STMT_CONST_DEF,
        STMT_CLOVER,
        STMT_FN_DEF,
        STMT_MOVE,
        STMT_SWAP
    } type;
} IRStmt;

static void markIRStmt(State* state, IRStmt* stmt) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: case STMT_GLOBAL: case STMT_CONST_DEF: case STMT_CLOVER: break;

    case STMT_FN_DEF: markIRFn(state, &stmt->fnDef.fn); break;

    case STMT_MOVE: case STMT_SWAP: break;
    }
}

static void assertIRStmtInTospace(State const* state, IRStmt const* stmt) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: case STMT_GLOBAL: case STMT_CONST_DEF: case STMT_CLOVER: break;

    case STMT_FN_DEF: assertIRFnInTospace(state, &stmt->fnDef.fn); break;

    case STMT_MOVE: case STMT_SWAP: break;
    }
}

inline static IRStmt globalDefToStmt(GlobalDef globalDef) {
    return (IRStmt){{.globalDef = globalDef}, STMT_GLOBAL_DEF};
}

inline static IRStmt globalToStmt(IRGlobal global) {
    return (IRStmt){{.global = global}, STMT_GLOBAL};
}

inline static IRStmt constDefToStmt(ConstDef cdef) {
    return (IRStmt){{.constDef = cdef}, STMT_CONST_DEF};
}

inline static IRStmt fnDefToStmt(FnDef fnDef) { return (IRStmt){{.fnDef = fnDef}, STMT_FN_DEF}; }

inline static IRStmt moveToStmt(MoveStmt mov) { return (IRStmt){{.mov = mov}, STMT_MOVE}; }

inline static IRStmt swapToStmt(SwapStmt swap) { return (IRStmt){{.swap = swap}, STMT_SWAP}; }

static void swapStmts(void* restrict x, void* restrict y) {
    IRStmt* const xStmt = x;
    IRStmt* const yStmt = y;

    IRStmt const tmp = *xStmt;
    *xStmt = *yStmt;
    *yStmt = tmp;
}

typedef struct Call {
    IRName callee;
    IRLabel retLabel;
    Args closes;
    Args args;
} Call;

typedef struct Tailcall {
    IRName callee;
    IRName retFrame;
    Args args;
} Tailcall;

typedef struct IRIf {
    IRName cond;
    IRLabel conseq;
    IRLabel alt;
} IRIf;

typedef struct IRGoto {
    IRLabel dest;
    Args args;
} IRGoto;

typedef struct IRReturn {
    IRName callee;
    IRName arg;
} IRReturn;

typedef struct IRTransfer {
    union {
        Call call;
        Tailcall tailcall;
        IRIf iff;
        IRGoto gotoo;
        IRReturn ret;
    };
    enum {
        TRANSFER_CALL,
        TRANSFER_TAILCALL,
        TRANSFER_IF,
        TRANSFER_GOTO,
        TRANSFER_RETURN
    } type;
} IRTransfer;

typedef struct Callers {
    IRLabel* vals;
    size_t count;
    size_t cap;
} Callers;

static Callers createCallers(Compiler* compiler, size_t cap) {
    if (cap < 2) { cap = 2; }
    IRLabel* const vals = amalloc(&compiler->arena, cap * sizeof *vals);
    return (Callers){.vals = vals, .count = 0, .cap = cap};
}

typedef struct Stmts {
    IRStmt* vals;
    size_t count;
    size_t cap;
} Stmts;

static Stmts newStmtsWithCap(Compiler* compiler, size_t cap) {
    if (cap < 2) { cap = 2; }
    IRStmt* const vals = amalloc(&compiler->arena, cap * sizeof *vals);
    return (Stmts){.vals = vals, .count = 0, .cap = cap};
}

inline static Stmts newStmts(Compiler* compiler) { return newStmtsWithCap(compiler, 2); }

typedef struct IRBlock {
    IRLabel label;

    Callers callers;

    BitSet liveIns;

    IRName* params;
    size_t paramCount;
    size_t paramCap;
    
    Stmts stmts;
    
    IRTransfer transfer;
} IRBlock;

static void markIRBlock(State* state, IRBlock* block) {
    size_t stmtCount = block->stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        markIRStmt(state, &block->stmts.vals[i]);
    }
}

static void assertIRBlockInTospace(State const* state, IRBlock const* block) {
    size_t stmtCount = block->stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        assertIRStmtInTospace(state, &block->stmts.vals[i]);
    }
}

inline static void pushCaller(IRBlock* block, IRLabel caller) {
    block->callers.vals[block->callers.count++] = caller;
}

static void pushIRStmt(Compiler* compiler, Stmts* stmts, IRStmt stmt) {
    if (stmts->count == stmts->cap) {
        size_t const newCap = stmts->cap + (stmts->cap >> 1);
        stmts->vals = arealloc(&compiler->arena, stmts->vals, stmts->cap * sizeof *stmts->vals,
                               newCap * sizeof *stmts->vals);
        stmts->cap = newCap;
    }

    stmts->vals[stmts->count++] = stmt;
}

static IRFn createIRFn(Compiler* compiler) {
    uint8_t const constCap = 2;
    ORef* const consts = amalloc(&compiler->arena, constCap * sizeof *consts);

    size_t const blockCap = 2;
    IRBlock** const blocks = amalloc(&compiler->arena, blockCap * sizeof *blocks);

    return (IRFn){
        .blocks = blocks,
        .blockCount = 0,
        .blockCap = blockCap,

        .consts = consts,
        .constCount = 0,
        .constCap = constCap
    };
}

inline static BitSet const* fnFreeVars(IRFn const* fn) { return &fn->blocks[0]->liveIns; }

static IRConst pushFnConst(Compiler* compiler, IRFn* fn, ORef c) {
    if (fn->constCount == fn->constCap) {
        uint8_t const newCap = fn->constCap + (fn->constCap >> 1);
        fn->consts = arealloc(&compiler->arena, fn->consts, fn->constCap * sizeof *fn->consts,
                              newCap * sizeof *fn->consts);
        fn->constCap = newCap;
    }

    uint8_t const index = fn->constCount;
    fn->consts[fn->constCount++] = c;
    return (IRConst){index};
}

static IRConst fnConst(Compiler* compiler, IRFn* fn, ORef c) {
    // Linear search is actually good since there usually aren't that many constants per fn:
    size_t const constCount = fn->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        ORef const ic = fn->consts[i];
        if (eq(ic, c)) { return (IRConst){(uint8_t)i}; }
    }

    return pushFnConst(compiler, fn, c);
}

inline static IRConst allocFnConst(Compiler* compiler, IRFn* fn) {
    return pushFnConst(compiler, fn, fixnumToORef(Zero));
}

inline static void setFnConst(IRFn* fn, IRConst c, ORef v) {
    assert(c.index < fn->constCount);

    fn->consts[c.index] = v;
}

static IRBlock const* irLabelBlock(IRFn const* fn, IRLabel label) {
    assert(label.blockIndex < fn->blockCount);

    return fn->blocks[label.blockIndex];
}

static IRBlock* createIRBlock(Compiler* compiler, IRFn* fn, size_t callerCap) {
    Callers const callers = createCallers(compiler, callerCap);

    BitSet const liveIns = createBitSet(&compiler->arena, 0);

    size_t const paramCap = 2;
    IRName* const params = amalloc(&compiler->arena, paramCap * sizeof *params);
    
    IRBlock* const block = amalloc(&compiler->arena, sizeof *block);
    *block = (IRBlock){
        .label = (IRLabel){fn->blockCount},

        .callers = callers,

        .liveIns = liveIns,

        .params = params,
        .paramCount = 0,
        .paramCap = paramCap,

        .stmts = newStmts(compiler),
        
        .transfer = {}
    };
    
    if (fn->blockCount == fn->blockCap) {
        size_t const newCap = fn->blockCap + (fn->blockCap >> 1);
        fn->blocks = arealloc(&compiler->arena, fn->blocks, fn->blockCap * sizeof *fn->blocks,
                              newCap * sizeof *fn->blocks);
        fn->blockCap = newCap;
    }
    fn->blocks[fn->blockCount++] = block;
    
    return block;
}

static void pushIRParam(Compiler* compiler, IRBlock* block, IRName param) {
    if (block->paramCount == block->paramCap) {
        size_t const newCap = block->paramCap + (block->paramCap >> 1);
        block->params =
            arealloc(&compiler->arena, block->params, block->paramCap * sizeof *block->params,
                     newCap * sizeof *block->params);
        block->paramCap = newCap;
    }

    block->params[block->paramCount++] = param;
}

static void createCall(IRBlock* block, IRName callee, IRLabel retLabel, Args closes, Args args) {
    block->transfer = (IRTransfer){
        .type = TRANSFER_CALL,
        .call = (Call){.callee = callee, .retLabel = retLabel, .closes = closes, .args = args}
    };
}

static void createTailcall(IRBlock* block, IRName callee, IRName retFrame, Args args) {
    block->transfer = (IRTransfer){
        .type = TRANSFER_TAILCALL,
        .tailcall = (Tailcall){.callee = callee, .retFrame = retFrame, .args = args}
    };
}

static IRIf* createIRIf(IRBlock* block, IRName cond, IRLabel conseqLabel, IRLabel altLabel) {
    block->transfer = (IRTransfer){
        .type = TRANSFER_IF,
        .iff = (IRIf){.cond = cond, .conseq = conseqLabel, .alt = altLabel}
    };

    return &block->transfer.iff;
}

static void createIRGoto(Compiler* compiler, IRBlock* block, IRLabel destLabel, IRName arg) {
    Args args = createArgs(compiler);
    pushArg(compiler, &args, arg);

    block->transfer = (IRTransfer){
        .type = TRANSFER_GOTO,
        .gotoo = (IRGoto){.dest = destLabel, .args = args}
    };
}

static void createIRReturn(IRBlock* block, IRName callee, IRName arg) {
    block->transfer = (IRTransfer){
        .type = TRANSFER_RETURN,
        .ret = (IRReturn){.callee = callee, .arg = arg}
    };
}

typedef void (PrintIRNameFn)(State const* state, FILE* dest, Compiler const* compiler, IRName name);

static void printIRName(State const* state, FILE* dest, Compiler const* compiler, IRName name) {
    assert(name.index < compiler->nameCount);
    ORef const maybeSym = compiler->nameSyms[name.index];
    if (isSymbol(state, maybeSym)) {
        print(state, dest, maybeSym);
    }
    fprintf(dest, "$%ld", name.index);
}

inline static void printIRReg(
    State const* /*state*/, FILE* dest, Compiler const* /*compiler*/, IRName name
) {
    fprintf(dest, "r%ld", name.index);
}

static void printIRLabel(FILE* dest, IRLabel label) {
    fprintf(dest, ":%ld", label.blockIndex);
}

inline static void printIRConst(State const* state, FILE* dest, IRFn const* fn, IRConst c) {
    print(state, dest, fn->consts[c.index]);
}

static void printArgs(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    Args const* args
) {
    size_t const count = args->count;
    for (size_t i = 0; i < count; ++i) {
        if (i > 0) { fputc(' ', dest); }
        printName(state, dest, compiler, args->names[i]);
    }
}

static void printNestedIRFn(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting
);

static void printStmt(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting, IRStmt const* stmt
) {
    for (size_t i = 0; i < nesting + 1; ++i) { fprintf(dest, "  "); }

    switch (stmt->type) {
    case STMT_GLOBAL_DEF: {
        GlobalDef const globalDef = stmt->globalDef;
        fprintf(dest, "(def ");
        printIRConst(state, dest, fn, globalDef.name);
        fputc(' ', dest);
        printName(state, dest, compiler, globalDef.val);
        fputc(')', dest);
    }; break;

    case STMT_GLOBAL: {
        IRGlobal const global = stmt->global;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, global.tmpName);
        fprintf(dest, " (global ");
        printIRConst(state, dest, fn, global.name);
        fprintf(dest, "))");
    }; break;

    case STMT_CONST_DEF: {
        ConstDef const cdef = stmt->constDef;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, cdef.name);
        fputc(' ', dest);
        printIRConst(state, dest, fn, cdef.v);
        fputc(')', dest);
    }; break;

    case STMT_CLOVER: {
        Clover const clover = stmt->clover;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, clover.name);
        fprintf(dest, " (clover ");
        printName(state, dest, compiler, clover.closure);
        fputc(' ', dest);
        printIRName(state, dest, compiler, clover.origName);
        fprintf(dest, " %u))", clover.idx);
    }; break;

    case STMT_FN_DEF: {
        FnDef const* const fnDef = &stmt->fnDef;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, fnDef->name);
        fprintf(dest, " (closure\n");
        printNestedIRFn(state, dest, compiler, printName, &fnDef->fn, nesting + 2);
        fputc('\n', dest);
        for (size_t i = 0; i < nesting + 2; ++i) { fprintf(dest, "  "); }
        printArgs(state, dest, compiler, printName, &fnDef->closes);
        fprintf(dest, "))\n");
    }; break;

    case STMT_MOVE: {
        MoveStmt const mov = stmt->mov;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, mov.dest);
        fputc(' ', dest);
        printName(state, dest, compiler, mov.src);
        fputc(')', dest);
    }; break;

    case STMT_SWAP: {
        SwapStmt const swap = stmt->swap;
        fprintf(dest, "(swap ");
        printName(state, dest, compiler, swap.reg1);
        fputc(' ', dest);
        printName(state, dest, compiler, swap.reg2);
        fputc(')', dest);
    }; break;
    }
}

static void printTransfer(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    size_t nesting, IRTransfer const* transfer
) {
    for (size_t i = 0; i < nesting + 1; ++i) { fprintf(dest, "  "); }

    switch (transfer->type) {
    case TRANSFER_CALL: {
        fprintf(dest, "(call ");
        printName(state, dest, compiler, transfer->call.callee);
        fprintf(dest, " (");
        printIRLabel(dest, transfer->call.retLabel);
        fputc(' ', dest);
        printArgs(state, dest, compiler, printName, &transfer->call.closes);
        fprintf(dest, ") ");
        printArgs(state, dest, compiler, printName, &transfer->call.args);
        fputc(')', dest);
    }; break;

    case TRANSFER_TAILCALL: {
        fprintf(dest, "(tailcall ");
        printName(state, dest, compiler, transfer->tailcall.callee);
        fputc(' ', dest);
        printName(state, dest, compiler, transfer->tailcall.retFrame);
        fputc(' ', dest);
        printArgs(state, dest, compiler, printName, &transfer->tailcall.args);
        fputc(')', dest);
    }; break;

    case TRANSFER_IF: {
        fprintf(dest, "(if ");
        printName(state, dest, compiler, transfer->iff.cond);
        fputc(' ', dest);
        printIRLabel(dest, transfer->iff.conseq);
        fputc(' ', dest);
        printIRLabel(dest, transfer->iff.alt);
        fputc(')', dest);
    }; break;

    case TRANSFER_GOTO: {
        fprintf(dest, "(goto ");
        printIRLabel(dest, transfer->gotoo.dest);
        fputc(' ', dest);
        printArgs(state, dest, compiler, printName, &transfer->gotoo.args);
        fputc(')', dest);
    }; break;

    case TRANSFER_RETURN: {
        fprintf(dest, "(return ");
        printName(state, dest, compiler, transfer->ret.callee);
        fputc(' ', dest);
        printName(state, dest, compiler, transfer->ret.arg);
        fputc(')', dest);
    }; break;
    }
}

static void printBlock(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting, IRBlock* block
) {
    for (size_t i = 0; i < nesting; ++i) { fprintf(dest, "  "); }
    fprintf(dest, "(label ");

    printIRLabel(dest, block->label);

    fprintf(dest, " (");
    {
        size_t printed = 0;
        for (BitSetIter it = newBitSetIter(&block->liveIns);;) {
            MaybeSize const maybeIdx = bitSetIterNext(&it);
            if (!maybeIdx.hasVal) { break; }

            if (printed > 0) { fputc(' ', dest); }
            printIRName(state, dest, compiler, (IRName){maybeIdx.val});
            ++printed;
        }
    }

    fprintf(dest, ") (");
    size_t const paramCount = block->paramCount;
    for (size_t i = 0; i < paramCount; ++i) {
        if (i > 0) { fputc(' ', dest); }
        printName(state, dest, compiler, block->params[i]);
    }
    fputc(')', dest);

    size_t const callerCount = block->callers.count;
    if (callerCount > 0) {
        fprintf(dest, " callers (");

        for (size_t i = 0; i < callerCount; ++i) {
            if (i > 0) { fputc(' ', dest); }
            printIRLabel(dest, block->callers.vals[i]);
        }

        fputc(')', dest);
    }

    fputc('\n', dest);
    
    size_t const stmtCount = block->stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        printStmt(state, dest, compiler, printName, fn, nesting, &block->stmts.vals[i]);
        fputc('\n', dest);
    }
    
    printTransfer(state, dest, compiler, printName, nesting, &block->transfer);

    fprintf(dest, ")");
}

static void printNestedIRFn(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting
) {
    for (size_t i = 0; i < nesting; ++i) { fprintf(dest, "  "); }
    fprintf(dest, "(fn\n");
    
    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        if (i > 0) { fprintf(dest, "\n\n"); }
        printBlock(state, dest, compiler, printName, fn, nesting + 1, fn->blocks[i]);
    }
    
    fprintf(dest, ")");
}

static void printIRFn(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn
) {
    printNestedIRFn(state, dest, compiler, printName, fn, 0);
}

