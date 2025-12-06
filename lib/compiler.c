typedef struct IRName { size_t index; } IRName;

inline static bool irNameEq(IRName name1, IRName name2) { return name1.index == name2.index; }

// OPTIMIZE: As fast as possible but actually a terrible hash:
inline static size_t irNameHash(IRName name) { return name.index; }

typedef struct IRConst { uint8_t index; } IRConst;

typedef struct Compiler {
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
        .nameSyms = nameSyms,
        .nameCount = nameCount,
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

typedef struct IRStmt {
    union {
        GlobalDef globalDef;
        IRGlobal global;
        ConstDef constDef;
    };
    enum IRStmtType {
        STMT_GLOBAL_DEF,
        STMT_GLOBAL,
        STMT_CONST_DEF
    } type;
} IRStmt;

static void freeStmt(IRStmt* stmt) {
    switch (stmt->type) {
    case STMT_GLOBAL_DEF: // fallthrough
    case STMT_GLOBAL: // fallthrough
    case STMT_CONST_DEF: break;
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

typedef struct IRIf {
    IRName cond;
    IRName conseq;
    IRName alt;
} IRIf;

typedef struct IRGoto {
    IRName dest;
    IRName arg;
} IRGoto;

typedef struct IRReturn {
    IRName callee;
    IRName arg;
} IRReturn;

typedef struct IRTransfer {
    union {
        IRIf iff;
        IRGoto gotoo;
        IRReturn ret;
    };
    enum {
        TRANSFER_IF,
        TRANSFER_GOTO,
        TRANSFER_RETURN
    } type;
} IRTransfer;

static void freeTransfer(IRTransfer* transfer) {
    switch (transfer->type) {
    case TRANSFER_IF: // fallthrough
    case TRANSFER_GOTO: // fallthrough
    case TRANSFER_RETURN: break;
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
    IRBlock** blocks; // OPTIMIZE: `IRBlock* blocks`
    size_t blockCount;
    size_t blockCap;
    
    ORef* consts;
    uint8_t constCount;
    uint8_t constCap;
} IRFn;

static IRFn createIRFn() {
    size_t const blockCap = 2;
    IRBlock** const blocks = malloc(blockCap * sizeof *blocks);

    uint8_t const constCap = 2;
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
        uint8_t const newCap = fn->constCap + (fn->constCap >> 1);
        fn->consts = realloc(fn->consts, newCap * sizeof *fn->consts);
        fn->constCap = newCap;
    }
    
    uint8_t const index = fn->constCount;
    fn->consts[fn->constCount++] = c;
    return (IRConst){index};
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

static IRIf* createIRIf(IRBlock* block, IRName cond, IRName conseqLabel, IRName altLabel) {
    block->transfer.type = TRANSFER_IF;
    IRIf* const contTransfer = &block->transfer.iff;
    *contTransfer = (IRIf){.cond = cond, .conseq = conseqLabel, .alt = altLabel};
    return contTransfer;
}

static IRGoto* createIRGoto(IRBlock* block, IRName destLabel, IRName arg) {
    block->transfer.type = TRANSFER_GOTO;
    IRGoto* const contTransfer = &block->transfer.gotoo;
    *contTransfer = (IRGoto){.dest = destLabel, .arg = arg};
    return contTransfer;
}

static IRReturn* createIRReturn(IRBlock* block, IRName callee, IRName arg) {
    block->transfer.type = TRANSFER_RETURN;
    IRReturn* const contTransfer = &block->transfer.ret;
    *contTransfer = (IRReturn){.callee = callee, .arg = arg};
    return contTransfer;
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
    case STMT_GLOBAL_DEF: {
        GlobalDef const globalDef = stmt->globalDef;
        fprintf(dest, "(def ");
        printIRConst(state, dest, fn, globalDef.name);
        fputc(' ', dest);
        printIRName(state, dest, compiler, globalDef.val);
        fputc(')', dest);
        break;
    }

    case STMT_GLOBAL: {
        IRGlobal const global = stmt->global;
        fprintf(dest, "(let ");
        printIRName(state, dest, compiler, global.tmpName);
        fputc(' ', dest);
        printIRConst(state, dest, fn, global.name);
        fputc(')', dest);
        break;
    }

    case STMT_CONST_DEF: {
        ConstDef const cdef = stmt->constDef;
        fprintf(dest, "(let ");
        printIRName(state, dest, compiler, cdef.name);
        fputc(' ', dest);
        printIRConst(state, dest, fn, cdef.v);
        fputc(')', dest);
        break;
    }
    }
}

static void printTransfer(
    State const* state, FILE* dest, Compiler const* compiler, size_t nesting,
    IRTransfer const* transfer
) {
    for (size_t i = 0; i < nesting + 2; ++i) { fprintf(dest, "  "); }

    switch (transfer->type) {
    case TRANSFER_IF: {
        fprintf(dest, "(if ");
        printIRName(state, dest, compiler, transfer->iff.cond);
        fputc(' ', dest);
        printIRName(state, dest, compiler, transfer->iff.conseq);
        fputc(' ', dest);
        printIRName(state, dest, compiler, transfer->iff.alt);
        fputc(')', dest);
        break;
    }

    case TRANSFER_GOTO: {
        fprintf(dest, "(goto ");
        printIRName(state, dest, compiler, transfer->gotoo.dest);
        fputc(' ', dest);
        printIRName(state, dest, compiler, transfer->gotoo.arg);
        fputc(')', dest);
        break;
    }

    case TRANSFER_RETURN: {
        fprintf(dest, "(return ");
        printIRName(state, dest, compiler, transfer->ret.callee);
        fputc(' ', dest);
        printIRName(state, dest, compiler, transfer->ret.arg);
        fputc(')', dest);
        break;
    }
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

