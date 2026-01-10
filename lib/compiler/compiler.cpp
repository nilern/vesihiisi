#include "compiler.hpp"

#include "../state.hpp"
#include "../bytecode.hpp"
#include "tocps.hpp"
#include "liveness.hpp"
#include "pureloads.hpp"
#include "regalloc.hpp"
#include "cloverindexing.hpp"
#include "bytecodegen.hpp"

namespace {

// OPTIMIZE: As simple and fast as possible but actually a terrible hash:
inline size_t irNameHash(IRName name) { return name.index; }

Compiler createCompiler(void) {
    size_t const nameCap = 2;
    ORef* const nameSyms = (ORef*)malloc(nameCap * sizeof *nameSyms);

    // Reserve (IRName){0} as invalid:
    size_t const nameCount = 1;
    nameSyms[0] = Default;
    
    return Compiler{
        .arena = newArena(defaultArenaBlockSize),
        .nameSyms = nameSyms,
        .nameCount = nameCount,
        .nameCap = nameCap
    };
}

void freeCompiler(Compiler* compiler) {
    freeArena(&compiler->arena);
    free(compiler->nameSyms);
}

IRName renameSymbolImpl(Compiler* compiler, ORef maybeSym) {
    if (compiler->nameCount == compiler->nameCap) {
        size_t const newCap = compiler->nameCap + (compiler->nameCap >> 1);
        compiler->nameSyms = (ORef*)realloc(compiler->nameSyms, newCap * sizeof *compiler->nameSyms);
        compiler->nameCap = newCap;
    }

    size_t const index = compiler->nameCount;
    compiler->nameSyms[compiler->nameCount++] = maybeSym;
    return IRName{index};
}

IRName renameSymbol(Compiler* compiler, HRef<Symbol> sym) {
    return renameSymbolImpl(compiler, sym.oref());
}

IRName freshName(Compiler* compiler) {
    return renameSymbolImpl(compiler, Default);
}

IRName renameIRName(Compiler* compiler, IRName name) {
    return renameSymbolImpl(compiler, compiler->nameSyms[name.index]);
}

void markIRBlock(State* state, struct IRBlock* block);

void markIRFn(State* state, IRFn* fn) {
    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        markIRBlock(state, fn->blocks[i]);
    }

    fn->maybeName = state->heap.mark(fn->maybeName);
}

void assertIRBlockInTospace(State const* state, struct IRBlock const* block);

void assertIRFnInTospace(State const* state, IRFn const* fn) {
    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        assertIRBlockInTospace(state, fn->blocks[i]);
    }

    if (isHeaped(fn->maybeName)) {
        assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(fn->maybeName)));
    }
}

Args createArgs(Compiler* compiler) {
    size_t const cap = 2;
    IRName* names = (IRName*)amalloc(&compiler->arena, cap * sizeof *names);

    return Args{.names = names, .count = 0, .cap = cap};
}

void pushArg(Compiler* compiler, Args* args, IRName arg) {
    if (args->count == args->cap) {
        size_t const newCap = args->cap + args->cap / 2;
        args->names =
            (IRName*)arealloc(&compiler->arena, args->names, args->cap * sizeof *args->names,
                               newCap * sizeof *args->names);
        args->cap = newCap;
    }

    args->names[args->count++] = arg;
}

void markIRStmt(State* state, IRStmt* stmt) {
    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: {
        stmt->globalDef.name =
            HRef<Symbol>::fromUnchecked(state->heap.mark(stmt->globalDef.name.oref()));
    }; break;

    case IRStmt::GLOBAL: {
        stmt->global.name = HRef<Symbol>::fromUnchecked(state->heap.mark(stmt->global.name.oref()));
    }; break;

    case IRStmt::CONST_DEF: {
        stmt->constDef.v = state->heap.mark(stmt->constDef.v);
    }; break;

    case IRStmt::CLOVER: break;

    case IRStmt::METHOD_DEF: markIRFn(state, &stmt->methodDef.fn); break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP:
    case IRStmt::KNOT: case IRStmt::KNOT_INIT: case IRStmt::KNOT_GET: break;
    }
}

void assertIRStmtInTospace(State const* state, IRStmt const* stmt) {
    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: {
        assert(allocatedInSemispace(&state->heap.tospace, stmt->globalDef.name.ptr()));
    }; break;

    case IRStmt::GLOBAL: {
        assert(allocatedInSemispace(&state->heap.tospace, stmt->global.name.ptr()));
    }; break;

    case IRStmt::CONST_DEF: {
        ORef const v = stmt->constDef.v;
        if (isHeaped(v)) {
            assert(allocatedInSemispace(&state->heap.tospace, uncheckedORefToPtr(v)));
        }
    }; break;

    case IRStmt::CLOVER: break;

    case IRStmt::METHOD_DEF: assertIRFnInTospace(state, &stmt->methodDef.fn); break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP:
    case IRStmt::KNOT: case IRStmt::KNOT_INIT: case IRStmt::KNOT_GET: break;
    }
}

Callers createCallers(Compiler* compiler, size_t cap) {
    if (cap < 2) { cap = 2; }
    IRLabel* const vals = (IRLabel*)amalloc(&compiler->arena, cap * sizeof *vals);
    return Callers{.vals = vals, .count = 0, .cap = cap};
}

Stmts newStmtsWithCap(Compiler* compiler, size_t cap) {
    if (cap < 2) { cap = 2; }
    IRStmt* const vals = (IRStmt*)amalloc(&compiler->arena, cap * sizeof *vals);
    return Stmts{.vals = vals, .count = 0, .cap = cap};
}

inline Stmts newStmts(Compiler* compiler) { return newStmtsWithCap(compiler, 2); }

void markIRBlock(State* state, IRBlock* block) {
    size_t stmtCount = block->stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        markIRStmt(state, &block->stmts.vals[i]);
    }
}

void assertIRBlockInTospace(State const* state, IRBlock const* block) {
    size_t stmtCount = block->stmts.count;
    for (size_t i = 0; i < stmtCount; ++i) {
        assertIRStmtInTospace(state, &block->stmts.vals[i]);
    }
}

void pushIRStmt(Compiler* compiler, Stmts* stmts, IRStmt stmt) {
    if (stmts->count == stmts->cap) {
        size_t const newCap = stmts->cap + (stmts->cap >> 1);
        stmts->vals =
            (IRStmt*)arealloc(&compiler->arena, stmts->vals, stmts->cap * sizeof *stmts->vals,
                               newCap * sizeof *stmts->vals);
        stmts->cap = newCap;
    }

    stmts->vals[stmts->count++] = stmt;
}

IRFn createIRFn(Compiler* compiler, ORef maybeName) {
    size_t const blockCap = 2;
    IRBlock** const blocks = (IRBlock**)amalloc(&compiler->arena, blockCap * sizeof *blocks);

    return IRFn{
        .blocks = blocks,
        .blockCount = 0,
        .blockCap = blockCap,

        .maybeName = maybeName,
        .domain = IRDomain{.vals = nullptr, .count = 0, .cap = 0},
        .hasVarArg = false
    };
}

void setParamType(Compiler* compiler, IRDomain* domain, size_t idx, IRName typeName) {
    if (!domain->vals) {
        size_t cap = 2;
        if (idx >= cap) { cap = idx + 1; }
        domain->vals = (IRName*)amalloc(&compiler->arena, cap * sizeof *domain->vals);
        domain->cap = cap;
    } else if (idx >= domain->cap) {
        size_t newCap = domain->cap + domain->cap / 2;
        if (idx >= newCap) { newCap = idx + 1; }
        domain->vals =
            (IRName*)arealloc(&compiler->arena, domain->vals, domain->cap,
                                newCap * sizeof *domain->vals);
        domain->cap = newCap;
    }

    for (size_t i = domain->count; i < idx; ++i) {
        domain->vals[i] = invalidIRName;
    }

    domain->vals[idx] = typeName;

    domain->count = idx + 1;
}


void completeIRDomain(Compiler *compiler, IRDomain *domain, size_t arity) {
    if (domain->vals) {
        if (arity > domain->cap) {
            domain->vals = (IRName*)arealloc(&compiler->arena, domain->vals, domain->cap, arity);
            domain->cap = arity;
        }

        for (size_t i = domain->count; i < arity; ++i) {
            domain->vals[i] = invalidIRName;
        }

        domain->count = arity;
    }
}

IRBlock* createIRBlock(Compiler* compiler, IRFn* fn, size_t callerCap) {
    Callers const callers = createCallers(compiler, callerCap);

    BitSet const liveIns = createBitSet(&compiler->arena, 0);

    size_t const paramCap = 2;
    IRName* const params = (IRName*)amalloc(&compiler->arena, paramCap * sizeof *params);
    
    IRBlock* const block = (IRBlock*)amalloc(&compiler->arena, sizeof *block);
    *block = IRBlock{
        .label = IRLabel{fn->blockCount},

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
        fn->blocks =
            (IRBlock**)arealloc(&compiler->arena, fn->blocks, fn->blockCap * sizeof *fn->blocks,
                              newCap * sizeof *fn->blocks);
        fn->blockCap = newCap;
    }
    fn->blocks[fn->blockCount++] = block;
    
    return block;
}

void pushIRParam(Compiler* compiler, IRBlock* block, IRName param) {
    if (block->paramCount == block->paramCap) {
        size_t const newCap = block->paramCap + (block->paramCap >> 1);
        block->params =
            (IRName*)arealloc(&compiler->arena, block->params,
                            block->paramCap * sizeof *block->params,
                            newCap * sizeof *block->params);
        block->paramCap = newCap;
    }

    block->params[block->paramCount++] = param;
}

void createCall(IRBlock* block, IRName callee, IRLabel retLabel, Args closes, Args args) {
    block->transfer = IRTransfer{
        .call = Call{.callee = callee, .retLabel = retLabel, .closes = closes, .args = args},
        .type = IRTransfer::CALL
    };
}

void createTailcall(IRBlock* block, IRName callee, IRName retFrame, Args args) {
    block->transfer = IRTransfer{
        .tailcall = Tailcall{.callee = callee, .retFrame = retFrame, .args = args},
        .type = IRTransfer::TAILCALL
    };
}

IRIf* createIRIf(IRBlock* block, IRName cond, IRLabel conseqLabel, IRLabel altLabel) {
    block->transfer = IRTransfer{
        .iff = IRIf{.cond = cond, .conseq = conseqLabel, .alt = altLabel},
        .type = IRTransfer::IF
    };

    return &block->transfer.iff;
}

void createIRGoto(Compiler* compiler, IRBlock* block, IRLabel destLabel, IRName arg) {
    Args args = createArgs(compiler);
    pushArg(compiler, &args, arg);

    block->transfer = IRTransfer{
        .gotoo = IRGoto{.dest = destLabel, .args = args},
        .type = IRTransfer::GOTO
    };
}

void createIRReturn(IRBlock* block, IRName callee, IRName arg) {
    block->transfer = IRTransfer{
        .ret = IRReturn{.callee = callee, .arg = arg},
        .type = IRTransfer::RETURN,
    };
}

typedef void (PrintIRNameFn)(State const* state, FILE* dest, Compiler const* compiler, IRName name);

void printIRName(State const* state, FILE* dest, Compiler const* compiler, IRName name) {
    assert(name.index < compiler->nameCount);
    ORef const maybeSym = compiler->nameSyms[name.index];
    if (isSymbol(state, maybeSym)) {
        print(state, dest, maybeSym);
    }
    fprintf(dest, "$%ld", name.index);
}

inline void printIRReg(
    State const* /*state*/, FILE* dest, Compiler const* /*compiler*/, IRName name
) {
    fprintf(dest, "r%ld", name.index);
}

void printIRLabel(FILE* dest, IRLabel label) {
    fprintf(dest, ":%ld", label.blockIndex);
}

void printArgs(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    Args const* args
) {
    size_t const count = args->count;
    for (size_t i = 0; i < count; ++i) {
        if (i > 0) { fputc(' ', dest); }
        printName(state, dest, compiler, args->names[i]);
    }
}

void printNestedIRFn(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting
);

void printStmt(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    size_t nesting, IRStmt const* stmt
) {
    for (size_t i = 0; i < nesting + 1; ++i) { fprintf(dest, "  "); }

    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: {
        GlobalDef const globalDef = stmt->globalDef;
        fprintf(dest, "(def ");
        print(state, dest, globalDef.name.oref());
        fputc(' ', dest);
        printName(state, dest, compiler, globalDef.val);
        fputc(')', dest);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal const global = stmt->global;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, global.tmpName);
        fprintf(dest, " (global ");
        print(state, dest, global.name.oref());
        fprintf(dest, "))");
    }; break;

    case IRStmt::CONST_DEF: {
        ConstDef const cdef = stmt->constDef;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, cdef.name);
        fputc(' ', dest);
        print(state, dest, cdef.v);
        fputc(')', dest);
    }; break;

    case IRStmt::CLOVER: {
        Clover const clover = stmt->clover;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, clover.name);
        fprintf(dest, " (clover ");
        printName(state, dest, compiler, clover.closure);
        fputc(' ', dest);
        printIRName(state, dest, compiler, clover.origName);
        fprintf(dest, " %u))", clover.idx);
    }; break;

    case IRStmt::METHOD_DEF: {
        MethodDef const* const methodDef = &stmt->methodDef;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, methodDef->name);
        fprintf(dest, " (method\n");
        printNestedIRFn(state, dest, compiler, printName, &methodDef->fn, nesting + 2);
        fprintf(dest, "))\n");
    }; break;

    case IRStmt::CLOSURE: {
        IRClosure const* const closure = &stmt->closure;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, closure->name);
        fprintf(dest, " (closure ");
        printName(state, dest, compiler, closure->method);
        if (closure->closes->count > 0) { putc(' ', dest); }
        printArgs(state, dest, compiler, printName, closure->closes);
        fprintf(dest, "))");
    }; break;

    case IRStmt::MOVE: {
        MoveStmt const mov = stmt->mov;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, mov.dest);
        fputc(' ', dest);
        printName(state, dest, compiler, mov.src);
        fputc(')', dest);
    }; break;

    case IRStmt::SWAP: {
        SwapStmt const swap = stmt->swap;
        fprintf(dest, "(swap ");
        printName(state, dest, compiler, swap.reg1);
        fputc(' ', dest);
        printName(state, dest, compiler, swap.reg2);
        fputc(')', dest);
    }; break;

    case IRStmt::KNOT: {
        KnotStmt const knotStmt = stmt->knot;
        fputs("(let ", dest);
        printName(state, dest, compiler, knotStmt.name);
        fputs(" (knot))", dest);
    }; break;

    case IRStmt::KNOT_INIT: {
        KnotInitStmt const knotInit = stmt->knotInit;
        fputs("(knot-init! ", dest);
        printName(state, dest, compiler, knotInit.knot);
        fputc(' ', dest);
        printName(state, dest, compiler, knotInit.v);
        fputc(')', dest);
    }; break;

    case IRStmt::KNOT_GET: {
        KnotGetStmt const knotGet = stmt->knotGet;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, knotGet.name);
        fprintf(dest, " (knot-get ");
        printName(state, dest, compiler, knotGet.knot);
        fputs("))", dest);
    }; break;
    }
}

void printTransfer(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    size_t nesting, IRTransfer const* transfer
) {
    for (size_t i = 0; i < nesting + 1; ++i) { fprintf(dest, "  "); }

    switch (transfer->type) {
    case IRTransfer::CALL: {
        fprintf(dest, "(call ");
        printName(state, dest, compiler, transfer->call.callee);
        fprintf(dest, " (");
        printIRLabel(dest, transfer->call.retLabel);
        if (transfer->call.closes.count > 0) { fputc(' ', dest); }
        printArgs(state, dest, compiler, printName, &transfer->call.closes);
        fprintf(dest, ") ");
        printArgs(state, dest, compiler, printName, &transfer->call.args);
        fputc(')', dest);
    }; break;

    case IRTransfer::TAILCALL: {
        fprintf(dest, "(tailcall ");
        printName(state, dest, compiler, transfer->tailcall.callee);
        fputc(' ', dest);
        printName(state, dest, compiler, transfer->tailcall.retFrame);
        fputc(' ', dest);
        printArgs(state, dest, compiler, printName, &transfer->tailcall.args);
        fputc(')', dest);
    }; break;

    case IRTransfer::IF: {
        fprintf(dest, "(if ");
        printName(state, dest, compiler, transfer->iff.cond);
        fputc(' ', dest);
        printIRLabel(dest, transfer->iff.conseq);
        fputc(' ', dest);
        printIRLabel(dest, transfer->iff.alt);
        fputc(')', dest);
    }; break;

    case IRTransfer::GOTO: {
        fprintf(dest, "(goto ");
        printIRLabel(dest, transfer->gotoo.dest);
        fputc(' ', dest);
        printArgs(state, dest, compiler, printName, &transfer->gotoo.args);
        fputc(')', dest);
    }; break;

    case IRTransfer::RETURN: {
        fprintf(dest, "(return ");
        printName(state, dest, compiler, transfer->ret.callee);
        fputc(' ', dest);
        printName(state, dest, compiler, transfer->ret.arg);
        fputc(')', dest);
    }; break;
    }
}

void printBlock(
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
            Maybe<size_t> const maybeIdx = bitSetIterNext(&it);
            if (!maybeIdx.hasVal) { break; }

            if (printed > 0) { fputc(' ', dest); }
            printIRName(state, dest, compiler, IRName{maybeIdx.val});
            ++printed;
        }
    }

    fprintf(dest, ") (");
    size_t const paramCount = block->paramCount;
    for (size_t i = 0; i < paramCount; ++i) {
        if (i > 0) { fputc(' ', dest); }
        if (i == paramCount - 1 && block == fn->blocks[0] && fn->hasVarArg) {
            fputs(". ", dest);
        }
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
        printStmt(state, dest, compiler, printName, nesting, &block->stmts.vals[i]);
        fputc('\n', dest);
    }
    
    printTransfer(state, dest, compiler, printName, nesting, &block->transfer);

    fprintf(dest, ")");
}

void printNestedIRFn(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting
) {
    for (size_t i = 0; i < nesting; ++i) { fprintf(dest, "  "); }
    fprintf(dest, "(fn ");
    
    if (isHeaped(fn->maybeName)) {
        print(state, dest, fn->maybeName);
    } else {
        putc('_', dest);
    }
    fputs(" (", dest);

    size_t const domainCount = fn->domain.count;
    for (size_t i = 0; i < domainCount; ++i) {
        if (i > 0) { putc(' ', dest); }
        if (i == domainCount - 1 && fn->hasVarArg) { fprintf(dest, ". "); }
        printName(state, dest, compiler, fn->domain.vals[i]);
    }
    fprintf(dest, ")\n");

    size_t const blockCount = fn->blockCount;
    for (size_t i = 0; i < blockCount; ++i) {
        if (i > 0) { fprintf(dest, "\n\n"); }
        printBlock(state, dest, compiler, printName, fn, nesting + 1, fn->blocks[i]);
    }
    
    putc(')', dest);
}

void printIRFn(
    State const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn
) {
    printNestedIRFn(state, dest, compiler, printName, fn, 0);
}

HRef<Method> compile(State* state, ORef expr, bool debug) {
    Compiler compiler = createCompiler();

    IRFn irFn = topLevelExprToIR(state, &compiler, expr);
    if (debug) {
        puts(";; # IR:");
        printIRFn(state, stdout, &compiler, printIRName, &irFn);
        puts("\n");
    }

    enlivenFn(&compiler, &irFn);
    if (debug) {
        puts(";; # Enlivened IR:");
        printIRFn(state, stdout, &compiler, printIRName, &irFn);
        puts("\n");
    }

    fnWithPureLoads(&compiler, &irFn);
    if (debug) {
        puts(";; # Cachy-loading IR:");
        printIRFn(state, stdout, &compiler, printIRName, &irFn);
        puts("\n");
    }

    regAllocFn(&compiler, &irFn);
    if (debug) {
        puts(";; # Registral IR:");
        printIRFn(state, stdout, &compiler, printIRReg, &irFn);
        puts("\n");
    }

    indexToplevelFnClovers(&compiler, &irFn);
    if (debug) {
        puts(";; # Concrete IR:");
        printIRFn(state, stdout, &compiler, printIRReg, &irFn);
        puts("\n");
    }

    HRef<Method> const method = emitToplevelMethod(state, &compiler, &irFn);
    if (debug) {
        puts(";; # Bytecode:");
        disassemble(state, stdout, method);
        puts("");
    }

    freeCompiler(&compiler);
    return method;
}

} // namespace
