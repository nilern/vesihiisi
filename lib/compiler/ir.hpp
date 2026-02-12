#pragma once

#include "../util/arena.hpp"
#include "../util/avec.hpp"
#include "../util/bitset.hpp"
#include "../value.hpp"

namespace {

struct Compiler {
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
};

struct IRName {
    size_t index;

    static constexpr size_t invalidIndex = 0;

    bool operator==(IRName that) const { return index == that.index; }

    bool isValid() const { return index != invalidIndex; }

    void print(RT const* state, FILE* dest, Compiler const* compiler) const;

    void printAsReg(FILE* dest) const { fprintf(dest, "r%ld", index); }
};

constexpr IRName invalidIRName = {IRName::invalidIndex};

struct IRLabel {
    size_t blockIndex;

    void print(FILE* dest) const { fprintf(dest, ":%ld", blockIndex); }
};

struct IRBlock;

struct IRDomain {
    IRName* vals;
    size_t count;
    size_t cap;

    void setParamType(Compiler* compiler, size_t idx, IRName typeName);

    void complete(Compiler* compiler, size_t arity);
};

struct IRFn {
    AVec<IRBlock*> blocks; // OPTIMIZE: `AVec<IRBlock> blocks`
    ORef maybeName;
    IRDomain domain;
    bool hasVarArg;
    Arena* arena;

    IRFn(Arena* t_arena, ORef maybeName);

    IRBlock* createBlock(size_t callerCap);

    IRBlock const* labelBlock(IRLabel label) const { return blocks[label.blockIndex]; }

    BitSet const* freeVars() const;
};

struct Define {
    HRef<Symbol> name;
    IRName val;
};

struct GlobalSet {
    HRef<Symbol> name;
    IRName val;
};

struct IRGlobal {
    IRName tmpName;
    HRef<Symbol> name;
};

struct ConstDef {
    IRName name;
    ORef v;
};

struct Clover {
    IRName name;
    IRName closure;
    IRName origName;
    uint8_t idx;
};

struct MethodDef {
    IRName name;
    IRFn fn;
    AVec<IRName>* closes; // Shared with `IRClosure`
};

struct IRClosure {
    IRName name;
    IRName method;
    AVec<IRName>* closes; // Shared with `MethodDef`
};

struct MoveStmt {
    IRName dest;
    IRName src;
};

struct SwapStmt {
    IRName reg1;
    IRName reg2;
};

struct KnotStmt {
    IRName name;
};

struct KnotInitStmt {
    IRName knot;
    IRName v;
};

struct KnotGetStmt {
    IRName name;
    IRName knot;
};

struct IRStmt {
    ORef maybeLoc;
    union {
        Define define;
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

    IRStmt(Define t_define, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, define{t_define}, type{IRStmt::GLOBAL_DEF}
    {}

    IRStmt(GlobalSet t_globalSet, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, globalSet{t_globalSet}, type{IRStmt::GLOBAL_SET}
    {}

    IRStmt(IRGlobal t_global, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, global{t_global}, type{IRStmt::GLOBAL}
    {}

    IRStmt(ConstDef t_constDef, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, constDef{t_constDef}, type{IRStmt::CONST_DEF}
    {}

    IRStmt(Clover t_clover, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, clover{t_clover}, type{IRStmt::CLOVER}
    {}

    IRStmt(MethodDef&& t_methodDef, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, methodDef{std::move(t_methodDef)}, type{IRStmt::METHOD_DEF}
    {}

    IRStmt(IRClosure t_closure, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, closure{t_closure}, type{IRStmt::CLOSURE}
    {}

    IRStmt(MoveStmt t_mov, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, mov{t_mov}, type{IRStmt::MOVE}
    {}

    IRStmt(SwapStmt t_swap, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, swap{t_swap}, type{IRStmt::SWAP}
    {}

    IRStmt(KnotStmt t_knot, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, knot{t_knot}, type{IRStmt::KNOT}
    {}

    IRStmt(KnotInitStmt t_knotInit, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, knotInit{t_knotInit}, type{IRStmt::KNOT_INIT}
    {}

    IRStmt(KnotGetStmt t_knotGet, ORef t_maybeLoc) :
        maybeLoc{t_maybeLoc}, knotGet{t_knotGet}, type{IRStmt::KNOT_GET}
    {}
};

struct Call {
    IRName callee;
    IRLabel retLabel;
    AVec<IRName> closes;
    AVec<IRName> args;
};

struct Tailcall {
    IRName callee;
    IRName retFrame;
    AVec<IRName> args;
};

struct IRIf {
    IRName cond;
    IRLabel conseq;
    IRLabel alt;
};

struct IRGoto {
    IRLabel dest;
    AVec<IRName> args;
};

struct IRReturn {
    IRName callee;
    IRName arg;
};

struct IRTransfer {
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
};

struct IRBlock {
    IRLabel label;
    AVec<IRLabel> callers;
    BitSet liveIns;
    AVec<IRName> params;
    AVec<IRStmt> stmts;
    IRTransfer transfer;

    IRBlock(Arena* arena, IRLabel t_label, size_t callerCap);

    void createCall(IRName callee, IRLabel retLabel, AVec<IRName>&& closes, AVec<IRName>&& args,
                    ORef maybeLoc);

    void createTailcall(IRName callee, IRName retFrame, AVec<IRName>&& args, ORef maybeLoc);

    IRIf* createIf(IRName cond, IRLabel conseqLabel, IRLabel altLabel, ORef maybeLoc);

    void createGoto(Arena* arena, IRLabel destLabel, IRName arg, ORef maybeLoc);

    void createReturn(IRName callee, IRName arg, ORef maybeLoc);
};

IRName renameSymbol(Compiler* compiler, HRef<Symbol> sym);

IRName freshName(Compiler* compiler);

IRName renameIRName(Compiler* compiler, IRName name);

void setParamType(Compiler* compiler, IRDomain* domain, size_t idx, IRName typeName);

void completeIRDomain(Compiler *compiler, IRDomain *domain, size_t arity);

[[nodiscard]]
bool markIRFn(RT* state, struct IRFn* fn);
[[maybe_unused]]
void assertIRFnInTospace(RT const* state, struct IRFn const* fn);

typedef void (PrintIRNameFn)(RT const* state, FILE* dest, Compiler const* compiler, IRName name);

void printArgs(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    AVec<IRName> const* args);

void printNestedIRFn(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting);

void printStmt(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    size_t nesting, IRStmt const* stmt);

void printTransfer(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    size_t nesting, IRTransfer const* transfer);

void printBlock(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting, IRBlock* block);

void printNestedIRFn(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn, size_t nesting);

void printIRFn(
    RT const* state, FILE* dest, Compiler const* compiler, PrintIRNameFn printName,
    IRFn const* fn);

} // namespace
