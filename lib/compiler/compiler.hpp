#pragma once

#include "../util/arena.hpp"
#include "../util/avec.hpp"
#include "../util/bitset.hpp"
#include "../state.hpp"

namespace {

// TODO: Compiler linter a la GHC (in addition to bytecode verifier!)

typedef struct Compiler {
    Arena arena;
    AVec<ORef> nameSyms;

    Compiler() :
        arena{newArena(defaultArenaBlockSize)},
        nameSyms{&arena}
    {}

    ~Compiler() { freeArena(&arena); }

    // `nameSyms` and other `AVecs` have pointers to `arena` so this must not be copied or even
    // moved:
    Compiler(Compiler const&) = delete;
    Compiler& operator=(Compiler const&) = delete;
    Compiler(Compiler&&) = delete;
    Compiler& operator=(Compiler&&) = delete;
} Compiler;

typedef struct IRName { size_t index; } IRName;

typedef struct IRLabel { size_t blockIndex; } IRLabel;

typedef struct IRDomain {
    IRName* vals;
    size_t count;
    size_t cap;
} IRDomain;

struct IRBlock;

typedef struct IRFn {
    struct IRBlock** blocks; // OPTIMIZE: `IRBlock* blocks`
    size_t blockCount;
    size_t blockCap;

    ORef maybeName;
    IRDomain domain;
    bool hasVarArg;
} IRFn;

typedef struct Args {
    IRName* names;
    size_t count;
    size_t cap;
} Args;

typedef struct GlobalDef {
    HRef<Symbol> name;
    IRName val;
} GlobalDef;

typedef struct GlobalSet {
    HRef<Symbol> name;
    IRName val;
} GlobalSet;

typedef struct IRGlobal {
    IRName tmpName;
    HRef<Symbol> name;
} IRGlobal;

typedef struct ConstDef {
    IRName name;
    ORef v;
} ConstDef;

typedef struct Clover {
    IRName name;
    IRName closure;
    IRName origName;
    uint8_t idx;
} Clover;

struct MethodDef {
    IRName name;
    IRFn fn;
    Args* closes; // Shared with `IRClosure`
};

struct IRClosure {
    IRName name;
    IRName method;
    Args* closes; // Shared with `MethodDef`
};

struct MoveStmt {
    IRName dest;
    IRName src;
};

struct SwapStmt {
    IRName reg1;
    IRName reg2;
};

typedef struct KnotStmt {
    IRName name;
} KnotStmt;

typedef struct KnotInitStmt {
    IRName knot;
    IRName v;
} KnotInitStmt;

typedef struct KnotGetStmt {
    IRName name;
    IRName knot;
} KnotGetStmt;

typedef struct IRStmt {
    ORef maybeLoc;
    union {
        GlobalDef globalDef;
        GlobalSet globalSet;
        IRGlobal global;
        ConstDef constDef;
        Clover clover;
        MethodDef methodDef;
        IRClosure closure;
        MoveStmt mov;
        SwapStmt swap;
        KnotStmt knot;
        KnotInitStmt knotInit;
        KnotGetStmt knotGet;
    };
    enum IRStmtType {
        GLOBAL_DEF,
        GLOBAL_SET,
        GLOBAL,
        CONST_DEF,
        CLOVER,
        METHOD_DEF,
        CLOSURE,
        MOVE,
        SWAP,
        KNOT,
        KNOT_INIT,
        KNOT_GET
    } type;
} IRStmt;

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
    ORef maybeLoc;
    union {
        Call call;
        Tailcall tailcall;
        IRIf iff;
        IRGoto gotoo;
        IRReturn ret;
    };
    enum {
        CALL,
        TAILCALL,
        IF,
        GOTO,
        RETURN
    } type;
} IRTransfer;

typedef struct Callers {
    IRLabel* vals;
    size_t count;
    size_t cap;
} Callers;

typedef struct Stmts {
    IRStmt* vals;
    size_t count;
    size_t cap;
} Stmts;

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

constexpr IRName invalidIRName = {0};

inline bool irNameEq(IRName name1, IRName name2) { return name1.index == name2.index; }

inline bool irNameIsValid(IRName name) { return name.index != invalidIRName.index; }

IRName renameSymbol(Compiler* compiler, HRef<Symbol> sym);

IRName freshName(Compiler* compiler);

IRName renameIRName(Compiler* compiler, IRName name);

IRFn createIRFn(Compiler* compiler, ORef maybeName);

void setParamType(Compiler* compiler, IRDomain* domain, size_t idx, IRName typeName);

void completeIRDomain(Compiler* compiler, IRDomain* domain, size_t arity);

inline BitSet const* fnFreeVars(IRFn const* fn) { return &fn->blocks[0]->liveIns; }

inline IRBlock const* irLabelBlock(IRFn const* fn, IRLabel label) {
    assert(label.blockIndex < fn->blockCount);

    return fn->blocks[label.blockIndex];
}

IRBlock* createIRBlock(Compiler* compiler, IRFn* fn, size_t callerCap);

inline void pushCaller(IRBlock* block, IRLabel caller) {
    block->callers.vals[block->callers.count++] = caller;
}

void pushIRParam(Compiler* compiler, IRBlock* block, IRName param);

Stmts newStmtsWithCap(Compiler* compiler, size_t cap);

void pushIRStmt(Compiler* compiler, Stmts* stmts, IRStmt stmt);

inline IRStmt globalDefToStmt(GlobalDef globalDef, ORef maybeLoc) {
    return IRStmt{maybeLoc, {.globalDef = globalDef}, IRStmt::GLOBAL_DEF};
}

inline IRStmt globalSetToStmt(GlobalSet globalSet, ORef maybeLoc) {
    return IRStmt{maybeLoc, {.globalSet = globalSet}, IRStmt::GLOBAL_SET};
}

inline IRStmt globalToStmt(IRGlobal global, ORef maybeLoc) {
    return IRStmt{maybeLoc, {.global = global}, IRStmt::GLOBAL};
}

inline IRStmt constDefToStmt(ConstDef cdef, ORef maybeLoc) {
    return IRStmt{maybeLoc, {.constDef = cdef}, IRStmt::CONST_DEF};
}

inline IRStmt moveToStmt(MoveStmt mov, ORef maybeLoc) {
    return IRStmt{maybeLoc, {.mov = mov}, IRStmt::MOVE};
}

inline IRStmt swapToStmt(SwapStmt swap, ORef maybeLoc) {
    return IRStmt{maybeLoc, {.swap = swap}, IRStmt::SWAP};
}

inline void swapStmts(void* x, void* y) {
    IRStmt* const xStmt = (IRStmt*)x;
    IRStmt* const yStmt = (IRStmt*)y;

    IRStmt const tmp = *xStmt;
    *xStmt = *yStmt;
    *yStmt = tmp;
}

Args createArgs(Compiler* compiler);

void pushArg(Compiler* compiler, Args* args, IRName arg);

void createCall(IRBlock* block, IRName callee, IRLabel retLabel, Args closes, Args args,
                ORef maybeLoc);

void createTailcall(IRBlock* block, IRName callee, IRName retFrame, Args args, ORef maybeLoc);

IRIf* createIRIf(IRBlock* block, IRName cond, IRLabel conseqLabel, IRLabel altLabel, ORef maybeLoc);

void createIRGoto(Compiler* compiler, IRBlock* block, IRLabel destLabel, IRName arg, ORef maybeLoc);

void createIRReturn(IRBlock* block, IRName callee, IRName arg, ORef maybeLoc);

using CompilationRes = Res<SyntaxErrors, HRef<Method>>;

CompilationRes compile(State* state, ORef expr, HRef<Loc> loc, bool debug);

} // namespace
