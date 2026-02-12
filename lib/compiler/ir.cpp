#include "ir.hpp"

#include "../rt.hpp"
#include "../write.hpp"

namespace {

IRName renameSymbolImpl(Compiler* compiler, ORef maybeSym) {
    size_t const idx = compiler->nameSyms.count();
    compiler->nameSyms.push(maybeSym);
    return IRName{idx};
}

IRName renameSymbol(Compiler* compiler, HRef<Symbol> sym) {
    return renameSymbolImpl(compiler, sym);
}

IRName freshName(Compiler* compiler) {
    return renameSymbolImpl(compiler, Default);
}

IRName renameIRName(Compiler* compiler, IRName name) {
    return renameSymbolImpl(compiler, compiler->nameSyms[name.index]);
}

[[nodiscard]]
bool markIRBlock(RT* state, struct IRBlock* block);

bool markIRFn(RT* state, IRFn* fn) {
    for (IRBlock* block : fn->blocks) {
        if (!markIRBlock(state, block)) { return false; }
    }

    fn->maybeName = TRY_NULLOPT_TO_FALSE(state->heap.mark(fn->maybeName));

    return true;
}

void assertIRBlockInTospace(RT const* state, struct IRBlock const* block);

void assertIRFnInTospace(RT const* state, IRFn const* fn) {
    for (IRBlock* block : fn->blocks) {
        assertIRBlockInTospace(state, block);
    }

    if (isHeaped(fn->maybeName)) {
        assert(state->heap.evacuated(&*HRef<Object>::fromUnchecked(fn->maybeName)));
    }
}

[[nodiscard]]
bool markIRStmt(RT* state, IRStmt* stmt) {
    stmt->maybeLoc = TRY_NULLOPT_TO_FALSE(state->heap.mark(stmt->maybeLoc));

    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: {
        stmt->define.name =
            HRef<Symbol>::fromUnchecked(TRY_NULLOPT_TO_FALSE(state->heap.mark(stmt->define.name)));
    }; break;

    case IRStmt::GLOBAL_SET: {
        stmt->globalSet.name =
            HRef<Symbol>::fromUnchecked(
                TRY_NULLOPT_TO_FALSE(state->heap.mark(stmt->globalSet.name)));
    }; break;

    case IRStmt::GLOBAL: {
        stmt->global.name =
            HRef<Symbol>::fromUnchecked(TRY_NULLOPT_TO_FALSE(state->heap.mark(stmt->global.name)));
    }; break;

    case IRStmt::CONST_DEF: {
        stmt->constDef.v = TRY_NULLOPT_TO_FALSE(state->heap.mark(stmt->constDef.v));
    }; break;

    case IRStmt::CLOVER: break;

    case IRStmt::METHOD_DEF: {
        if (!markIRFn(state, &stmt->methodDef.fn)) { return false; }
    }; break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP:
    case IRStmt::KNOT: case IRStmt::KNOT_INIT: case IRStmt::KNOT_GET:
    case IRStmt::FFI_CALL: break;
    }

    return true;
}

void assertIRStmtInTospace(RT const* state, IRStmt const* stmt) {
    if (isHeaped(stmt->maybeLoc)) {
        assert(state->heap.evacuated(&*HRef<Object>::fromUnchecked(stmt->maybeLoc)));
    }

    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: {
        assert(state->heap.evacuated(&*stmt->define.name));
    }; break;

    case IRStmt::GLOBAL_SET: {
        assert(state->heap.evacuated(&*stmt->globalSet.name));
    }; break;

    case IRStmt::GLOBAL: {
        assert(state->heap.evacuated(&*stmt->global.name));
    }; break;

    case IRStmt::CONST_DEF: {
        ORef const v = stmt->constDef.v;
        if (isHeaped(v)) {
            assert(state->heap.evacuated(&*HRef<Object>::fromUnchecked(v)));
        }
    }; break;

    case IRStmt::CLOVER: break;

    case IRStmt::METHOD_DEF: assertIRFnInTospace(state, &stmt->methodDef.fn); break;

    case IRStmt::CLOSURE: case IRStmt::MOVE: case IRStmt::SWAP:
    case IRStmt::KNOT: case IRStmt::KNOT_INIT: case IRStmt::KNOT_GET:
    case IRStmt::FFI_CALL: break;
    }
}

[[nodiscard]]
bool markIRTransfer(RT& state, IRTransfer& transfer) {
    transfer.maybeLoc = TRY_NULLOPT_TO_FALSE(state.heap.mark(transfer.maybeLoc));
    return true;
}

void assertIRTransferInTospace([[maybe_unused]] RT const& state, IRTransfer const& transfer) {
    if (isHeaped(transfer.maybeLoc)) {
        assert(state.heap.evacuated(&*HRef<Object>::fromUnchecked(transfer.maybeLoc)));
    }
}

bool markIRBlock(RT* state, IRBlock* block) {
    for (IRStmt& stmt : block->stmts) {
        if (!markIRStmt(state, &stmt)) { return false; }
    }

    return markIRTransfer(*state, block->transfer);
}

void assertIRBlockInTospace(RT const* state, IRBlock const* block) {
    for (IRStmt const& stmt : block->stmts) {
        assertIRStmtInTospace(state, &stmt);
    }

    assertIRTransferInTospace(*state, block->transfer);
}

IRFn::IRFn(Arena* t_arena, ORef t_maybeName) :
    blocks{t_arena},
    maybeName{t_maybeName},
    domain{.vals = nullptr, .count = 0, .cap = 0},
    hasVarArg{false},
    arena{t_arena}
{}

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
            (IRName*)arealloc(&compiler->arena, domain->vals, domain->cap * sizeof *domain->vals,
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
            domain->vals =
                (IRName*)arealloc(&compiler->arena, domain->vals,
                                    domain->cap * sizeof *domain->vals, arity * sizeof *domain->vals);
            domain->cap = arity;
        }

        for (size_t i = domain->count; i < arity; ++i) {
            domain->vals[i] = invalidIRName;
        }

        domain->count = arity;
    }
}

IRBlock* IRFn::createBlock(size_t callerCap) {
    IRBlock* const block = static_cast<IRBlock*>(amalloc(arena, sizeof *block));
    new (block) IRBlock{arena, IRLabel{blocks.count()}, callerCap};
    blocks.push(block);
    return block;
}

BitSet const* IRFn::freeVars() const { return &blocks[0]->liveIns; }

IRBlock::IRBlock(Arena* arena, IRLabel t_label, size_t callerCap) :
    label{t_label},
    callers{arena, callerCap},
    liveIns{createBitSet(arena, 0)},
    params{arena},
    stmts{arena},
    transfer{ // Placeholder:
        .maybeLoc = Default,
        .ret = IRReturn{invalidIRName, invalidIRName},
        .type = IRTransfer::RETURN
    }
{}

void IRBlock::createCall(
    IRName callee, IRLabel retLabel, AVec<IRName>&& closes, AVec<IRName>&& args, ORef maybeLoc
) {
    transfer = IRTransfer{
        .maybeLoc = maybeLoc,
        .call = Call{.callee = callee, .retLabel = retLabel, .closes = std::move(closes),
                     .args = std::move(args)},
        .type = IRTransfer::CALL
    };
}

void IRBlock::createTailcall(IRName callee, IRName retFrame, AVec<IRName>&& args, ORef maybeLoc) {
    transfer = IRTransfer{
        .maybeLoc = maybeLoc,
        .tailcall = Tailcall{.callee = callee, .retFrame = retFrame, .args = std::move(args)},
        .type = IRTransfer::TAILCALL
    };
}

IRIf* IRBlock::createIf(IRName cond, IRLabel conseqLabel, IRLabel altLabel, ORef maybeLoc) {
    transfer = IRTransfer{
        .maybeLoc = maybeLoc,
        .iff = IRIf{.cond = cond, .conseq = conseqLabel, .alt = altLabel},
        .type = IRTransfer::IF
    };

    return &transfer.iff;
}

void IRBlock::createGoto(Arena* arena, IRLabel destLabel, IRName arg, ORef maybeLoc) {
    auto args = AVec<IRName>{arena};
    args.push(arg);

    transfer = IRTransfer{
        .maybeLoc = maybeLoc,
        .gotoo = IRGoto{.dest = destLabel, .args = std::move(args)},
        .type = IRTransfer::GOTO
    };
}

void IRBlock::createReturn(IRName callee, IRName arg, ORef maybeLoc) {
    transfer = IRTransfer{
        .maybeLoc = maybeLoc,
        .ret = IRReturn{.callee = callee, .arg = arg},
        .type = IRTransfer::RETURN,
    };
}

typedef void (PrintIRNameFn)(RT const* state, FILE* dest, Compiler const* compiler, IRName name);

void IRName::print(RT const* state, FILE* dest, Compiler const* compiler) const {
    assert(index < compiler->nameSyms.count());
    ORef const maybeSym = compiler->nameSyms[index];
    if (isa<Symbol>(*state, maybeSym)) {
        write(state, dest, maybeSym);
    }
    fprintf(dest, "$%ld", index);
}

void printIRName(RT const* state, FILE* dest, Compiler const* compiler, IRName name) {
    name.print(state, dest, compiler);
}

void printIRReg(
    RT const* /*state*/, FILE* dest, Compiler const* /*compiler*/, IRName name
) {
    name.printAsReg(dest);
}

void printArgs(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    AVec<IRName> const* args
) {
    size_t const count = args->count();
    for (size_t i = 0; i < count; ++i) {
        if (i > 0) { fputc(' ', dest); }
        printName(state, dest, compiler, (*args)[i]);
    }
}

void printNestedIRFn(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting
    );

void printStmt(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    size_t nesting, IRStmt const* stmt
    ) {
    for (size_t i = 0; i < nesting + 1; ++i) { fprintf(dest, "  "); }

    switch (stmt->type) {
    case IRStmt::GLOBAL_DEF: {
        Define const define = stmt->define;
        fprintf(dest, "(def ");
        write(state, dest, define.name);
        fputc(' ', dest);
        printName(state, dest, compiler, define.val);
        fputc(')', dest);
    }; break;

    case IRStmt::GLOBAL_SET: {
        GlobalSet const globalSet = stmt->globalSet;
        fprintf(dest, "(set! ");
        write(state, dest, globalSet.name);
        fputc(' ', dest);
        printName(state, dest, compiler, globalSet.val);
        fputc(')', dest);
    }; break;

    case IRStmt::GLOBAL: {
        IRGlobal const global = stmt->global;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, global.tmpName);
        fprintf(dest, " (global ");
        write(state, dest, global.name);
        fprintf(dest, "))");
    }; break;

    case IRStmt::CONST_DEF: {
        ConstDef const cdef = stmt->constDef;
        fprintf(dest, "(let ");
        printName(state, dest, compiler, cdef.name);
        fputc(' ', dest);
        write(state, dest, cdef.v);
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
        if (closure->closes->count() > 0) { putc(' ', dest); }
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

    case IRStmt::FFI_CALL: {
        FFICall const& ffiCall = stmt->ffiCall;

        fputs("(let ", dest);
        printName(state, dest, compiler, ffiCall.name);
        fputs(" (call-foreign ", dest);

        if (ffiCall.codomain.box) {
            fputs("(-box ", dest);
            printName(state, dest, compiler, ffiCall.codomain.name);
            putc(')', dest);
        } else {
            printName(state, dest, compiler, ffiCall.codomain.name);
        }

        putc(' ', dest);
        printName(state, dest, compiler, ffiCall.callee);

        for (FFICall::Arg const& arg : ffiCall.args) {
            putc(' ', dest);

            if (arg.unbox) {
                fputs("(-unbox ", dest);
                printName(state, dest, compiler, arg.name);
                putc(')', dest);
            } else {
                printName(state, dest, compiler, arg.name);
            }
        }

        fputs("))", dest);
    }
    }
}

void printTransfer(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    size_t nesting, IRTransfer const* transfer
    ) {
    for (size_t i = 0; i < nesting + 1; ++i) { fprintf(dest, "  "); }

    switch (transfer->type) {
    case IRTransfer::CALL: {
        fprintf(dest, "(call ");
        printName(state, dest, compiler, transfer->call.callee);
        fprintf(dest, " (");
        transfer->call.retLabel.print(dest);
        if (transfer->call.closes.count() > 0) { fputc(' ', dest); }
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
        transfer->iff.conseq.print(dest);
        fputc(' ', dest);
        transfer->iff.alt.print(dest);
        fputc(')', dest);
    }; break;

    case IRTransfer::GOTO: {
        fprintf(dest, "(goto ");
        transfer->gotoo.dest.print(dest);
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
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting, IRBlock* block
    ) {
    for (size_t i = 0; i < nesting; ++i) { fprintf(dest, "  "); }
    fprintf(dest, "(label ");

    block->label.print(dest);

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
    size_t const paramCount = block->params.count();
    for (size_t i = 0; i < paramCount; ++i) {
        if (i > 0) { fputc(' ', dest); }
        if (i == paramCount - 1 && block == fn->blocks[0] && fn->hasVarArg) {
            fputs(". ", dest);
        }
        printName(state, dest, compiler, block->params[i]);
    }
    fputc(')', dest);

    size_t const callerCount = block->callers.count();
    if (callerCount > 0) {
        fprintf(dest, " callers (");

        for (size_t i = 0; i < callerCount; ++i) {
            if (i > 0) { fputc(' ', dest); }
            block->callers[i].print(dest);
        }

        fputc(')', dest);
    }

    fputc('\n', dest);

    size_t const stmtCount = block->stmts.count();
    for (size_t i = 0; i < stmtCount; ++i) {
        printStmt(state, dest, compiler, printName, nesting, &block->stmts[i]);
        fputc('\n', dest);
    }

    printTransfer(state, dest, compiler, printName, nesting, &block->transfer);

    fprintf(dest, ")");
}

void printNestedIRFn(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting
    ) {
    for (size_t i = 0; i < nesting; ++i) { fprintf(dest, "  "); }
    fprintf(dest, "(fn ");

    if (isHeaped(fn->maybeName)) {
        write(state, dest, fn->maybeName);
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

    size_t const blockCount = fn->blocks.count();
    for (size_t i = 0; i < blockCount; ++i) {
        if (i > 0) { fprintf(dest, "\n\n"); }
        printBlock(state, dest, compiler, printName, fn, nesting + 1, fn->blocks[i]);
    }

    putc(')', dest);
}

void printIRFn(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn
    ) {
    printNestedIRFn(state, dest, compiler, printName, fn, 0);
}

} // namespace
