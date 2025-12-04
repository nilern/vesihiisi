typedef struct IRName { size_t index; } IRName;

typedef enum IRAtomTag {
    IR_ATOM_NAME = 0b0,
    IR_ATOM_CONST = 0b1
} IRAtomTag;

static uintptr_t const irAtomTagWidth = 1;
static uintptr_t const irAtomTagBits = (1 << irAtomTagWidth) - 1; // `irAtomTagWidth` ones

typedef struct IRAtom { uintptr_t bits; } IRAtom;

inline static IRAtomTag getIRAtomTag(IRAtom atom) { return (IRAtomTag)(atom.bits & irAtomTagBits); }

inline static IRAtom tagConst(size_t index) {
    return (IRAtom){(uintptr_t)((index << irAtomTagWidth) | IR_ATOM_CONST)};
}

inline static IRName uncheckedIRAtomToName(IRAtom atom) {
    return (IRName){atom.bits >> irAtomTagWidth};
}

typedef struct Compiler {
    ORef* nameSyms;
    size_t nameCount;
    size_t nameCap;
} Compiler;

static Compiler createCompiler(void) {
    size_t const nameCap = 2;
    ORef* const nameSyms = malloc(nameCap * sizeof *nameSyms);
    
    return (Compiler){
        .nameSyms = nameSyms,
        .nameCount = 0,
        .nameCap = nameCap
    };
}

static void freeCompiler(Compiler* compiler) {
    free(compiler->nameSyms);
}

static IRName freshName(Compiler* compiler) {
    if (compiler->nameCount < compiler->nameCap) {
        size_t const newCap = compiler->nameCap + (compiler->nameCap >> 1);
        compiler->nameSyms = realloc(compiler->nameSyms, newCap * sizeof *compiler->nameSyms);
    }

    size_t const index = compiler->nameCount;
    compiler->nameSyms[compiler->nameCount++] = fixnumToORef(Zero);
    return (IRName){index};
}

typedef enum IRStmtType {
    STMT_CLOSURE
} IRStmtType;

typedef struct IRStmt {
    IRStmtType type;
} IRStmt;

static void freeStmt(IRStmt* stmt) {
    switch (stmt->type) {
    case STMT_CLOSURE:
        assert(false); // TODO
        break;
    }
}

typedef enum IRTransferType {
    TRANSFER_CONTINUE
} IRTransferType;

typedef struct IRContinue {
    IRName callee;
    IRAtom* args;
    size_t argCount;
    size_t argCap;
} IRContinue;

inline static void freeContinue(IRContinue* contTransfer) {
    free(contTransfer->args);
}

typedef struct IRTransfer {
    IRTransferType type;
    union {
        IRContinue cont;
    };
} IRTransfer;

static void freeTransfer(IRTransfer* transfer) {
    switch (transfer->type) {
    case TRANSFER_CONTINUE:
        freeContinue(&transfer->cont);
        break;
    }
}

typedef struct IRBlock {
    IRName label;
    IRName* params;
    size_t paramCount;
    size_t paramCap;
    
    IRStmt* stmts;
    size_t stmtCount;
    size_t stmtCap;
    
    IRTransfer transfer;
} IRBlock;

static void freeBlock(IRBlock* block) {
    free(block->params);
    
    size_t const stmtCount = block->stmtCount;
    for (size_t i = 0; i < stmtCount; ++i) {
        freeStmt(&block->stmts[i]);
    }
    free(block->stmts);
    
    freeTransfer(&block->transfer);
}

typedef struct IRFn {
    IRBlock** blocks;
    size_t blockCount;
    size_t blockCap;
    
    ORef* consts;
    size_t constCount;
    size_t constCap;
} IRFn;

static IRFn createIRFn() {
    size_t const blockCap = 2;
    IRBlock** const blocks = malloc(blockCap * sizeof *blocks);

    size_t const constCap = 2;
    ORef* const consts = malloc(constCap * sizeof *consts);

    return (IRFn){
        .blocks = blocks,
        .blockCount = 0,
        .blockCap = blockCap,

        .consts = consts,
        .constCount = 0,
        .constCap = constCap
    };
}

static void freeIRFn(IRFn* fn) {
    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        freeBlock(fn->blocks[i]);
    }
    free(fn->blocks);
    
    free(fn->consts);
}

static IRAtom fnConst(IRFn* fn, ORef c) {
    // Linear search is actually good since there usually aren't that many constants per fn:
    size_t const constCount = fn->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        ORef const ic = fn->consts[i];
        if (eq(ic, c)) { return tagConst(i); }
    }

    if (fn->constCount == fn->constCap) {
        size_t const newCap = fn->constCap + (fn->constCap >> 1);
        fn->consts = realloc(fn->consts, newCap * sizeof *fn->consts);
    }
    
    size_t const index = fn->constCount;
    fn->consts[fn->constCount++] = c;
    return tagConst(index);
}

static ORef uncheckedIRAtomToConst(IRFn const* fn, IRAtom atom) {
    size_t const index = atom.bits >> irAtomTagWidth;
    assert(index < fn->constCount);
    return fn->consts[index];
}

static IRBlock* createIRBlock(IRFn* fn, IRName label, size_t paramCap) {
    if (paramCap < 2) { paramCap = 2; }
    IRName* const params = malloc(paramCap * sizeof *params);
    
    size_t const stmtCap = 2;
    IRStmt* const stmts = malloc(stmtCap * sizeof *stmts);
    
    IRBlock* const block = malloc(sizeof *block);
    *block = (IRBlock){
        .label = label,
        .params = params,
        .paramCount = 0,
        .paramCap = paramCap,
        
        .stmts = stmts,
        .stmtCount = 0,
        .stmtCap = stmtCap,
        
        .transfer = {}
    };
    
    if (fn->blockCount == fn->blockCap) {
        size_t const newCap = fn->blockCap + (fn->blockCap >> 1);
        fn->blocks = realloc(fn->blocks, newCap * sizeof *fn->blocks);
    }
    fn->blocks[fn->blockCount++] = block;
    
    return block;
}

static void pushIRParam(IRBlock* block, IRName param) {
    assert(block->paramCount < block->paramCap);
    block->params[block->paramCount++] = param;
}

static IRContinue* createIRContinue(IRBlock* block, IRName callee, size_t argCap) {
    if (argCap < 2) { argCap = 2; }
    IRAtom* const args = malloc(argCap * sizeof *args);
    
    IRContinue* const contTransfer = &block->transfer.cont;
    *contTransfer = (IRContinue){
        .callee = callee,
        .args = args,
        .argCount = 0,
        .argCap = argCap
    };
    
    return contTransfer;
}

static void irContinuePushArg(IRContinue* contTransfer, IRAtom arg) {
    assert(contTransfer->argCount < contTransfer->argCap);
    contTransfer->args[contTransfer->argCount++] = arg;
}

static IRFn topLevelExprToIR(State const* state, Compiler* compiler, ORef expr) {
    IRFn fn = createIRFn();
    
    IRName const entry = freshName(compiler);
    IRBlock* const entryBlock = createIRBlock(&fn, entry, 1);
    IRName const self = freshName(compiler);
    pushIRParam(entryBlock, self);
    IRName const ret = freshName(compiler);
    pushIRParam(entryBlock, ret);

    if (isHeaped(expr)) {
        if (isPair(state, expr)) {
            assert(false); // TODO
        } else if (isSymbol(state, expr)) {
            assert(false); // TODO
        }
    }

    // Else a constant:
    IRContinue* continueTransfer = createIRContinue(entryBlock, ret, 1);
    IRAtom const c = fnConst(&fn, expr);
    irContinuePushArg(continueTransfer, c);
    
    return fn;
}

static void printIRName(State const* state, FILE* dest, Compiler const* compiler, IRName name) {
    assert(name.index < compiler->nameCount);
    ORef const maybeSym = compiler->nameSyms[name.index];
    if (isSymbol(state, maybeSym)) {
        print(state, dest, maybeSym);
    } else {
        fprintf(dest, "$%ld", name.index);
    }
}

static void printIRAtom(
    State const* state, FILE* dest, Compiler const* compiler, IRFn const* fn, IRAtom atom
) {
    switch (getIRAtomTag(atom)) {
    case IR_ATOM_NAME:
        printIRName(state, dest, compiler, uncheckedIRAtomToName(atom));
        break;

    case IR_ATOM_CONST:
        print(state, dest, uncheckedIRAtomToConst(fn, atom));
        break;
    }
}

static void printStmt(
    State const* /*state*/, FILE* dest, Compiler const* /*compiler*/, IRFn const* /*fn*/,
    size_t nesting, IRStmt const* stmt
) {
    for (size_t i = 0; i < nesting + 2; ++i) { fprintf(dest, "  "); }

    switch (stmt->type) {
    case STMT_CLOSURE:
        assert(false); // TODO
        break;
    }
}

static void printContinue(
    State const* state, FILE* dest, Compiler const* compiler, IRFn const* fn,
    IRContinue const* cont
) {
    fprintf(dest, "(continue ");

    printIRName(state, dest, compiler, cont->callee);

    size_t const argCount = cont->argCount;
    for (size_t i = 0; i < argCount; ++i) {
        fputc(' ', dest);
        printIRAtom(state, dest, compiler, fn, cont->args[i]);
    }

    fputc(')', dest);
}

static void printTransfer(
    State const* state, FILE* dest, Compiler const* compiler, IRFn const* fn, size_t nesting,
    IRTransfer const* transfer
) {
    for (size_t i = 0; i < nesting + 2; ++i) { fprintf(dest, "  "); }

    switch (transfer->type) {
    case TRANSFER_CONTINUE:
        printContinue(state, dest, compiler, fn, &transfer->cont);
        break;
    }
}

static void printBlock(
    State const* state, FILE* dest, Compiler const* compiler, IRFn const* fn, size_t nesting,
    IRBlock* block
) {
    for (size_t i = 0; i < nesting + 1; ++i) { fprintf(dest, "  "); }
    fprintf(dest, "(label (");
    printIRName(state, dest, compiler, block->label);
    size_t const paramCount = block->paramCount;
    for (size_t i = 0; i < paramCount; ++i) {
        fputc(' ', dest);
        printIRName(state, dest, compiler, block->params[i]);
    }
    fprintf(dest, ")\n");
    
    size_t const stmtCount = block->stmtCount;
    for (size_t i = 0; i < stmtCount; ++i) {
        printStmt(state, dest, compiler, fn, nesting, &block->stmts[i]);
    }
    
    printTransfer(state, dest, compiler, fn, nesting, &block->transfer);

    fprintf(dest, ")");
}

static void printNestedIRFn(
    State const* state, FILE* dest, Compiler const* compiler, IRFn const* fn, size_t nesting
) {
    fprintf(dest, "(fn\n");
    
    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        if (i > 0) { fprintf(dest, "\n\n"); }
        printBlock(state, dest, compiler, fn, nesting, fn->blocks[i]);
    }
    
    fprintf(dest, ")");
}

static void printIRFn(State const* state, FILE* dest, Compiler const* compiler, IRFn const* fn) {
    printNestedIRFn(state, dest, compiler, fn, 0);
}

