typedef struct VMRes {
    ORef val;
    bool success;
} VMRes;

static VMRes run(State* state, ClosureRef selfRef) {
    // TODO: Debug index & type checks & bytecode verifier

    Closure const* const self = closureToPtr(selfRef);
    ORef const method = self->method;
    Method const* const methodPtr = methodToPtr(uncheckedORefToMethod(method));
    assert(methodPtr->nativeCode == callBytecode);
    state->method = method;
    state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
    state->pc = 0;
    state->consts = arrayToPtr(uncheckedORefToArray(methodPtr->consts));
    state->regs[calleeReg] = closureToORef(selfRef);
    state->regs[retContReg] = closureToORef(state->exit); // Return continuation

    for (;/*ever*/;) {
        switch ((Opcode)state->code[state->pc++]) {
        case OP_MOVE: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const srcReg = state->code[state->pc++];

            state->regs[destReg] = state->regs[srcReg];
        }; break;

        case OP_SWAP: {
            uint8_t const reg1 = state->code[state->pc++];
            uint8_t const reg2 = state->code[state->pc++];

            ORef const tmp = state->regs[reg1];
            state->regs[reg1] = state->regs[reg2];
            state->regs[reg2] = tmp;
        }; break;

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
        }; break;

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
        }; break;

        case OP_CONST: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const constIdx = state->code[state->pc++];

            state->regs[destReg] = state->consts[constIdx];
        }; break;

        case OP_BR: {
            uint8_t const displacement = state->code[state->pc++];

            state->pc += displacement;
        }; break;

        case OP_BRF: {
            uint8_t const condReg = state->code[state->pc++];
            uint8_t const displacement = state->code[state->pc++];

            if (eq(state->regs[condReg], boolToORef(False))) {
                state->pc += displacement;
            }
        }; break;

        case OP_RET: {
            assert(eq(typeToORef(typeOf(state, state->regs[retContReg])),
                      typeToORef(state->continuationType)));
            ContinuationRef const retRef = uncheckedORefToContinuation(state->regs[retContReg]);
            Continuation const* const ret = continuationToPtr(retRef);
            ORef const method = ret->method;
            if (!eq(method, fixnumToORef(Zero))) {
                assert(isMethod(state, method));
                Method const* const methodPtr = methodToPtr(uncheckedORefToMethod(method));
                state->method = method;
                state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                state->pc = (size_t)fixnumToInt(ret->pc);
                state->consts = arrayToPtr(uncheckedORefToArray(methodPtr->consts));
            } else { // Exit
                return (VMRes){.success = true, .val = state->regs[retReg]};
            }
        }; break;

        case OP_CLOSURE: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const methodConstIdx = state->code[state->pc++];
            uint8_t const cloverSetByteCount = state->code[state->pc++];
            size_t cloverCount = 0;
            // OPTIMIZE:
            for (size_t i = 0; i < cloverSetByteCount; ++i) {
                cloverCount += stdc_count_ones(state->code[state->pc++]);
            }

            MethodRef const method = uncheckedORefToMethod(state->consts[methodConstIdx]);
            ClosureRef const closure = allocClosure(state, method, tagInt((intptr_t)cloverCount));
            // TODO: DRY wrt. OP_CALL:
            // OPTIMIZE:
            {
                size_t const end = state->pc;
                size_t const start = end - cloverSetByteCount;
                for (size_t byteIdx = 0, cloverIdx = 0; byteIdx < cloverSetByteCount; ++byteIdx) {
                    uint8_t const byte = state->code[start + byteIdx];
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> (UINT8_WIDTH - 1 - bitIdx)) & 1) {
                            ORef* const cloverPtr =
                                (ORef*)closureToPtr(closure)->clovers + cloverIdx++;
                            size_t const regIdx = byteIdx + bitIdx;
                            *cloverPtr = state->regs[regIdx];
                        }
                    }
                }
            }

            state->regs[destReg] = closureToORef(closure);
        }; break;

        case OP_CLOVER: {
            uint8_t const destReg = state->code[state->pc++];
            uint8_t const closureReg = state->code[state->pc++];
            uint8_t const cloverIdx = state->code[state->pc++];

            // OPTIMIZE: Separate OP_CONT_CLOVER:
            ORef const anyClosure = state->regs[closureReg];
            if (!isClosure(state, anyClosure)) {
                Continuation const* const cont =
                    continuationToPtr(uncheckedORefToContinuation(anyClosure));
                state->regs[destReg] = cont->saves[cloverIdx];
            } else {
                Closure const* const closure = closureToPtr(uncheckedORefToClosure(anyClosure));
                state->regs[destReg] = closure->clovers[cloverIdx];
            }
        }; break;

        case OP_CALL: {
            uint8_t regCount /*FIXME:*/ [[maybe_unused]] = state->code[state->pc++];
            uint8_t const cloverSetByteCount = state->code[state->pc++];
            size_t cloverCount = 0;
            // OPTIMIZE:
            for (size_t i = 0; i < cloverSetByteCount; ++i) {
                cloverCount += stdc_count_ones(state->code[state->pc++]);
            }

            MethodRef const callerMethod = uncheckedORefToMethod(state->method);
            ContinuationRef const cont = allocContinuation(
                state, callerMethod, tagInt((intptr_t)state->pc), tagInt((intptr_t)cloverCount)
            );
            // TODO: DRY wrt. OP_CLOSURE:
            // OPTIMIZE:
            {
                size_t const end = state->pc;
                size_t const start = end - cloverSetByteCount;
                for (size_t byteIdx = 0, cloverIdx = 0; byteIdx < cloverSetByteCount; ++byteIdx) {
                    uint8_t const byte = state->code[start + byteIdx];
                    for (size_t bitIdx = 0; bitIdx < UINT8_WIDTH; ++bitIdx) {
                        if ((byte >> (UINT8_WIDTH - 1 - bitIdx)) & 1) {
                            ORef* const cloverPtr =
                                (ORef*)continuationToPtr(cont)->saves + cloverIdx++;
                            size_t const regIdx = byteIdx + bitIdx;
                            *cloverPtr = state->regs[regIdx];
                        }
                    }
                }
            }

            state->regs[retContReg] = continuationToORef(cont);

            // TODO: DRY wrt. OP_TAILCALL:
            bool trampoline = true;
            while (trampoline) {
                ORef callee = state->regs[calleeReg];
                if (!isClosure(state, callee)) {
                    state->regs[calleeReg] = getErrorHandler(state);
                    state->regs[firstArgReg] =
                        typeErrorToORef(createTypeError(state, state->closureType, callee));
                    callee = state->regs[calleeReg];
                    regCount = 3;
                }
                Closure const* const closure = closureToPtr(uncheckedORefToClosure(callee));
                ORef const method = closure->method;
                assert(isMethod(state, method));
                Method const* const methodPtr = methodToPtr(uncheckedORefToMethod(method));
                if (methodPtr->nativeCode == callBytecode) {
                    state->method = method;
                    state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                    state->pc = 0;
                    state->consts = arrayToPtr(uncheckedORefToArray(methodPtr->consts));
                    trampoline = false;
                } else {
                    switch (methodPtr->nativeCode(state)) {
                    case PRIMOP_RES_CONTINUE: { // TODO: DRY wrt. OP_RET:
                        assert(eq(typeToORef(typeOf(state, state->regs[retContReg])),
                                typeToORef(state->continuationType)));
                        ContinuationRef const retRef =
                            uncheckedORefToContinuation(state->regs[retContReg]);
                        Continuation const* const ret = continuationToPtr(retRef);
                        ORef const method = ret->method;
                        if (!eq(method, fixnumToORef(Zero))) {
                            assert(isMethod(state, method));
                            Method const* const methodPtr =
                                methodToPtr(uncheckedORefToMethod(method));
                            state->method = method;
                            state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                            state->pc = (size_t)fixnumToInt(ret->pc);
                            state->consts = arrayToPtr(uncheckedORefToArray(methodPtr->consts));
                            trampoline = false;
                        } else { // Exit
                            return (VMRes){.val = state->regs[retReg], .success = true};
                        }
                    }; break;

                    case PRIMOP_RES_TAILCALL: break; // All is in place, just keep trampolining

                    case PRIMOP_RES_ABORT: return (VMRes){};
                    }
                }
            }
        }; break;

        case OP_TAILCALL: {
            uint8_t regCount /*FIXME:*/ [[maybe_unused]] = state->code[state->pc++];

            // TODO: DRY wrt. OP_CALL:
            bool trampoline = true;
            while (trampoline) {
                ORef callee = state->regs[calleeReg];
                if (!isClosure(state, callee)) {
                    state->regs[calleeReg] = getErrorHandler(state);
                    state->regs[firstArgReg] =
                        typeErrorToORef(createTypeError(state, state->closureType, callee));
                    callee = state->regs[calleeReg];
                    regCount = 3;
                }
                Closure const* const closure = closureToPtr(uncheckedORefToClosure(callee));
                ORef const method = closure->method;
                assert(isMethod(state, method));
                Method const* const methodPtr = methodToPtr(uncheckedORefToMethod(method));
                if (methodPtr->nativeCode == callBytecode) {
                    state->method = method;
                    state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                    state->pc = 0;
                    state->consts = arrayToPtr(uncheckedORefToArray(methodPtr->consts));
                    trampoline = false;
                } else {
                    switch (methodPtr->nativeCode(state)) {
                    case PRIMOP_RES_CONTINUE: { // TODO: DRY wrt. OP_RET:
                        assert(eq(typeToORef(typeOf(state, state->regs[retContReg])),
                                typeToORef(state->continuationType)));
                        ContinuationRef const retRef =
                            uncheckedORefToContinuation(state->regs[retContReg]);
                        Continuation const* const ret = continuationToPtr(retRef);
                        ORef const method = ret->method;
                        if (!eq(method, fixnumToORef(Zero))) {
                            assert(isMethod(state, method));
                            Method const* const methodPtr =
                                methodToPtr(uncheckedORefToMethod(method));
                            state->method = method;
                            state->code = byteArrayToPtr(uncheckedORefToByteArray(methodPtr->code));
                            state->pc = (size_t)fixnumToInt(ret->pc);
                            state->consts = arrayToPtr(uncheckedORefToArray(methodPtr->consts));
                            trampoline = false;
                        } else { // Exit
                            return (VMRes){.val = state->regs[retReg], .success = true};
                        }
                    }; break;

                    case PRIMOP_RES_TAILCALL: break; // All is in place, just keep trampolining

                    case PRIMOP_RES_ABORT: return (VMRes){};
                    }
                }
            }
        }; break;
        }
    }
}
