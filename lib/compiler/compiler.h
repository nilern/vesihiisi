#pragma once

#include "../util/arena.h"
#include "../util/bitset.h"
#include "../object.h"

// TODO: Compiler linter a la GHC (in addition to bytecode verifier!)

typedef struct Compiler {
    Arena arena;
    ORef* nameSyms;
    size_t nameCount;
    size_t nameCap;
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
    SymbolRef name;
    IRName val;
} GlobalDef;

typedef struct IRGlobal {
    IRName tmpName;
    SymbolRef name;
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

typedef struct MethodDef {
    IRName name;
    IRFn fn;
    Args* closes; // Shared with `IRClosure`
} MethodDef;

typedef struct IRClosure {
    IRName name;
    IRName method;
    Args* closes; // Shared with `MethodDef`
} IRClosure;

typedef struct MoveStmt {
    IRName dest;
    IRName src;
} MoveStmt;

typedef struct SwapStmt {
    IRName reg1;
    IRName reg2;
} SwapStmt;

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
    union {
        GlobalDef globalDef;
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
        STMT_GLOBAL_DEF,
        STMT_GLOBAL,
        STMT_CONST_DEF,
        STMT_CLOVER,
        STMT_METHOD_DEF,
        STMT_CLOSURE,
        STMT_MOVE,
        STMT_SWAP,
        STMT_KNOT,
        STMT_KNOT_INIT,
        STMT_KNOT_GET
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

static const IRName invalidIRName;

inline static bool irNameEq(IRName name1, IRName name2) { return name1.index == name2.index; }

inline static bool irNameIsValid(IRName name) { return name.index != invalidIRName.index; }

static IRName renameSymbol(Compiler* compiler, SymbolRef sym);

static IRName freshName(Compiler* compiler);

static IRName renameIRName(Compiler* compiler, IRName name);

static IRFn createIRFn(Compiler* compiler, ORef maybeName);

static void setParamType(Compiler* compiler, IRDomain* domain, size_t idx, IRName typeName);

static void completeIRDomain(Compiler* compiler, IRDomain* domain, size_t arity);

inline static BitSet const* fnFreeVars(IRFn const* fn) { return &fn->blocks[0]->liveIns; }

inline static IRBlock const* irLabelBlock(IRFn const* fn, IRLabel label) {
    assert(label.blockIndex < fn->blockCount);

    return fn->blocks[label.blockIndex];
}

static IRBlock* createIRBlock(Compiler* compiler, IRFn* fn, size_t callerCap);

inline static void pushCaller(IRBlock* block, IRLabel caller) {
    block->callers.vals[block->callers.count++] = caller;
}

static void pushIRParam(Compiler* compiler, IRBlock* block, IRName param);

static Stmts newStmtsWithCap(Compiler* compiler, size_t cap);

static void pushIRStmt(Compiler* compiler, Stmts* stmts, IRStmt stmt);

inline static IRStmt globalDefToStmt(GlobalDef globalDef) {
    return (IRStmt){{.globalDef = globalDef}, STMT_GLOBAL_DEF};
}

inline static IRStmt globalToStmt(IRGlobal global) {
    return (IRStmt){{.global = global}, STMT_GLOBAL};
}

inline static IRStmt constDefToStmt(ConstDef cdef) {
    return (IRStmt){{.constDef = cdef}, STMT_CONST_DEF};
}

inline static IRStmt moveToStmt(MoveStmt mov) { return (IRStmt){{.mov = mov}, STMT_MOVE}; }

inline static IRStmt swapToStmt(SwapStmt swap) { return (IRStmt){{.swap = swap}, STMT_SWAP}; }

inline static void swapStmts(void* restrict x, void* restrict y) {
    IRStmt* const xStmt = x;
    IRStmt* const yStmt = y;

    IRStmt const tmp = *xStmt;
    *xStmt = *yStmt;
    *yStmt = tmp;
}

static Args createArgs(Compiler* compiler);

static void pushArg(Compiler* compiler, Args* args, IRName arg);

static void createCall(IRBlock* block, IRName callee, IRLabel retLabel, Args closes, Args args);

static void createTailcall(IRBlock* block, IRName callee, IRName retFrame, Args args);

static IRIf* createIRIf(IRBlock* block, IRName cond, IRLabel conseqLabel, IRLabel altLabel);

static void createIRGoto(Compiler* compiler, IRBlock* block, IRLabel destLabel, IRName arg);

static void createIRReturn(IRBlock* block, IRName callee, IRName arg);
