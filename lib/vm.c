static ORef run(State* state, ClosureRef selfRef) {
    Closure const* const self = closureToPtr(selfRef);
    ORef const anyMethod = self->method;
    // TODO: Debug type check:
    Method const* const method = methodToPtr(uncheckedORefToMethod(anyMethod));
    state->code = byteArrayToPtr(method->code);
    state->pc = 0;
    state->consts = arrayToPtr(method->consts);
    state->regs[0] = closureToORef(selfRef);
    state->regs[1] = closureToORef(state->exit); // Return continuation

    for (;/*ever*/;) {
        switch ((Opcode)state->code[state->pc++]) {
        case OP_DEF: {
            uint8_t const constIdx = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            VarRef var;
            ORef const c = state->consts[constIdx];
            if (!isSymbol(state, c)) {
                assert(false); // TODO: Lazily linked Var
            } else {
                SymbolRef const name = uncheckedORefToSymbol(c);
                var = getVar(state, state->ns, name);
            }

            varToPtr(var)->val = state->regs[srcReg];
            break;
        }

        case OP_GLOBAL: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            VarRef var;
            ORef const c = state->consts[constIdx];
            if (!isSymbol(state, c)) {
                assert(false); // TODO: Lazily linked Var
                var = tagVar((Var*)nullptr); // HACK: Shuts up warning about maybe uninit `var`.
            } else {
                SymbolRef const name = uncheckedORefToSymbol(c);
                FindVarRes const findRes = findVar(state->ns, name);
                if (findRes.type != NS_FOUND_VAR) {
                    assert(false); // FIXME: Use of unbound name
                }

                var = findRes.var;
            }

            ORef const v = varToPtr(var)->val;
            if (eq(v, unboundToORef(state->unbound))) {
                assert(false); // FIXME: use of unbound var
            }
            state->regs[destReg] = v;
            break;
        }

        case OP_CONST: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            state->regs[destReg] = state->consts[constIdx];
            break;
        }

        case OP_RET: {
            uint8_t const retReg = state->code[state->pc++];
            uint8_t const vReg = state->code[state->pc++];

            assert(eq(typeToORef(typeOf(state, state->regs[retReg])),
                      typeToORef(state->closureType)));
            ClosureRef const retRef = uncheckedORefToClosure(state->regs[retReg]);
            Closure const* const ret = closureToPtr(retRef);
            ORef const anyMethod = ret->method;
            if (!eq(anyMethod, fixnumToORef(Zero))) {
                assert(false); // TODO
            } else { // Exit
                return state->regs[vReg];
            }
        }
        }
    }
}
