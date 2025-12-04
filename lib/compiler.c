typedef struct IRName { size_t index; } IRName;

typedef struct IRConst { uint8_t index; } IRConst;

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
        compiler->nameCap = newCap;
    }

    size_t const index = compiler->nameCount;
    compiler->nameSyms[compiler->nameCount++] = fixnumToORef(Zero);
    return (IRName){index};
}

typedef enum IRStmtType {
    STMT_CONST_DEF
} IRStmtType;

typedef struct ConstDef {
    IRName name;
    IRConst v;
} ConstDef;

typedef struct IRStmt {
    IRStmtType type;
    union {
        ConstDef constDef;
    };
} IRStmt;

static void freeStmt(IRStmt* stmt) {
    switch (stmt->type) {
    case STMT_CONST_DEF: break;
    }
}

inline static IRStmt constDefToStmt(ConstDef cdef) {
    return (IRStmt){STMT_CONST_DEF, {.constDef = cdef}};
}

typedef enum IRTransferType {
    TRANSFER_RETURN
} IRTransferType;

typedef struct IRReturn {
    IRName callee;
    IRName* args;
    size_t argCount;
    size_t argCap;
} IRReturn;

inline static void freeReturn(IRReturn* contTransfer) {
    free(contTransfer->args);
}

typedef struct IRTransfer {
    IRTransferType type;
    union {
        IRReturn ret;
    };
} IRTransfer;

static void freeTransfer(IRTransfer* transfer) {
    switch (transfer->type) {
    case TRANSFER_RETURN:
        freeReturn(&transfer->ret);
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

static void pushIRStmt(IRBlock* block, IRStmt stmt) {
    if (block->stmtCount == block->stmtCap) {
        size_t const newCap = block->stmtCap + (block->stmtCap >> 1);
        block->stmts = realloc(block->stmts, newCap * sizeof *block->stmts);
        block->stmtCap = newCap;
    }

    block->stmts[block->stmtCount++] = stmt;
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

static IRConst fnConst(IRFn* fn, ORef c) {
    // Linear search is actually good since there usually aren't that many constants per fn:
    size_t const constCount = fn->constCount;
    for (size_t i = 0; i < constCount; ++i) {
        ORef const ic = fn->consts[i];
        if (eq(ic, c)) { return (IRConst){(uint8_t)i}; }
    }

    if (fn->constCount == fn->constCap) {
        size_t const newCap = fn->constCap + (fn->constCap >> 1);
        fn->consts = realloc(fn->consts, newCap * sizeof *fn->consts);
         fn->constCap = newCap;
    }
    
    size_t const index = fn->constCount;
    fn->consts[fn->constCount++] = c;
    return (IRConst){(uint8_t)index};
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
        fn->blockCap = newCap;
    }
    fn->blocks[fn->blockCount++] = block;
    
    return block;
}

static void pushIRParam(IRBlock* block, IRName param) {
    assert(block->paramCount < block->paramCap);
    block->params[block->paramCount++] = param;
}

static IRReturn* createIRReturn(IRBlock* block, IRName callee, size_t argCap) {
    if (argCap < 2) { argCap = 2; }
    IRName* const args = malloc(argCap * sizeof *args);
    
    IRReturn* const contTransfer = &block->transfer.ret;
    *contTransfer = (IRReturn){
        .callee = callee,
        .args = args,
        .argCount = 0,
        .argCap = argCap
    };
    
    return contTransfer;
}

static void irReturnPushArg(IRReturn* retTransfer, IRName arg) {
    assert(retTransfer->argCount < retTransfer->argCap);
    retTransfer->args[retTransfer->argCount++] = arg;
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

inline static void printIRConst(State const* state, FILE* dest, IRFn const* fn, IRConst c) {
    print(state, dest, fn->consts[c.index]);
}

static void printStmt(
    State const* state, FILE* dest, Compiler const* compiler, IRFn const* fn,
    size_t nesting, IRStmt const* stmt
) {
    for (size_t i = 0; i < nesting + 2; ++i) { fprintf(dest, "  "); }

    switch (stmt->type) {
    case STMT_CONST_DEF:
        ConstDef const cdef = stmt->constDef;
        fprintf(dest, "(let ");
        printIRName(state, dest, compiler, cdef.name);
        fputc(' ', dest);
        printIRConst(state, dest, fn, cdef.v);
        fputc(')', dest);
        break;
    }
}

static void printReturn(
    State const* state, FILE* dest, Compiler const* compiler, IRReturn const* cont
) {
    fprintf(dest, "(return ");

    printIRName(state, dest, compiler, cont->callee);

    size_t const argCount = cont->argCount;
    for (size_t i = 0; i < argCount; ++i) {
        fputc(' ', dest);
        printIRName(state, dest, compiler, cont->args[i]);
    }

    fputc(')', dest);
}

static void printTransfer(
    State const* state, FILE* dest, Compiler const* compiler, size_t nesting,
    IRTransfer const* transfer
) {
    for (size_t i = 0; i < nesting + 2; ++i) { fprintf(dest, "  "); }

    switch (transfer->type) {
    case TRANSFER_RETURN:
        printReturn(state, dest, compiler, &transfer->ret);
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
        fputc('\n', dest);
    }
    
    printTransfer(state, dest, compiler, nesting, &block->transfer);

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

