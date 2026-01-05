#include "vesihiisi.h"
#include "state.h"

void print(State const* state, FILE* dest, ORef v) {
    switch (getTag(v)) {
    case TYPE_FIXNUM:
        fprintf(dest, "%ld", uncheckedFixnumToInt(v));
        break;
        
    case TYPE_FLONUM:
        fprintf(dest, "%f", uncheckedFlonumToDouble(v));
        break;
        
    case TYPE_CHAR:
        fprintf(dest, "#\"%c\"", uncheckedORefToChar(v));
        break;
        
    case TYPE_BOOL:
        if (uncheckedORefToBool(v)) {
            fprintf(dest, "#t");
        } else {
            fprintf(dest, "#f");
        }
        break;
        
    case TYPE_HEAPED:
        if (isString(state, v)) {
            StringRef const s = uncheckedORefToString(v);
            
            Str const str = stringStr(s);
            fprintf(dest, "\"%.*s\"", (int)str.len, str.data);
        } else if (isSymbol(state, v)) {
            SymbolRef const s = uncheckedORefToSymbol(v);
            
            Str const name = symbolName(s);
            fprintf(dest, "%.*s", (int)name.len, name.data);
        } else if (isPair(state, v)) {
            Pair const* pair = pairToPtr(uncheckedORefToPair(v));
            
            fputc('(', dest);
            print(state, dest, pair->car);
            
            for (ORef tail = pair->cdr; true; tail = pair->cdr) {
                if (isPair(state, tail)) {
                    pair = pairToPtr(uncheckedORefToPair(tail));
                    fputc(' ', dest);
                    print(state, dest, pair->car);
                } else if (isEmptyList(state, tail)) {
                    break;
                } else {
                    printf(" . ");
                    print(state, dest, tail);
                    break;
                }
            }
            
            fputc(')', dest);
        } else if (isEmptyList(state, v)) {
            fprintf(dest, "()");
        } else if (isa(state, state->arrayMutType, v)) {
            ORef const* const vs = arrayMutToPtr(uncheckedORefToArrayMut(v));
            fprintf(dest, "#<array!");
            size_t const count = (uintptr_t)fixnumToInt(uncheckedFlexCount(v));
            for (size_t i = 0; i < count; ++i) {
                fputc(' ', dest);
                print(state, dest, vs[i]);
            }
            putc('>', dest);
        } else if (isa(state, state->methodType, v)) {
            Method const* const method = toPtr(uncheckedORefToMethod(v));

            fprintf(dest, "#<method");
            ORef const maybeName = method->maybeName;
            if (isHeaped(maybeName)) {
                putc(' ', dest);
                print(state, dest, maybeName);
            }
            putc('>', dest);
        } else if (isClosure(state, v)) {
            Closure const* const closure = toPtr(uncheckedORefToClosure(v));

            fprintf(dest, "#<fn");

            // TODO: DRY with #<method ... directly above:
            if (isMethod(state, closure->method)) {
                Method const* const method = toPtr(uncheckedORefToMethod(closure->method));
                ORef const maybeName = method->maybeName;
                if (isHeaped(maybeName)) {
                    putc(' ', dest);
                    print(state, dest, maybeName);
                }
            }

            putc('>', dest);
        } else if (isMultimethod(state, v)) {
            fprintf(dest, "#<multimethod>");
        } else if (isContinuation(state, v)) {
            fprintf(dest, "#<continuation>");
        } else if (isType(state, v)) {
            Type const* const type = toPtr(uncheckedORefToType(v));

            fprintf(dest, "#<type ");
            print(state, dest, toORef(type->name));
            putc('>', dest);
        } else if (isa(state, state->unboundErrorType, v)) {
            UnboundError const* const err = toPtr(uncheckedORefToUnboundError(v));

            fputs("#<unbound-error ", dest);
            print(state, dest, toORef(err->name));
            putc('>', dest);
        } else if (isTypeError(state, v)) {
            TypeError const* const err = typeErrorToPtr(uncheckedORefToTypeError(v));

            fputs("#<type-error ", dest);
            print(state, dest, toORef(err->type));
            putc(' ', dest);
            print(state, dest, err->val);
            putc('>', dest);
        } else if (isa(state, state->arityErrorType, v)) {
            ArityError const* const err = arityErrorToPtr(uncheckedORefToArityError(v));

            fputs("#<arity-error ", dest);
            print(state, dest, toORef(err->callee));
            putc(' ', dest);
            print(state, dest, toORef(err->callArgc));
            putc('>', dest);
        } else if (isa(state, state->inapplicableErrorType, v)) {
            InapplicableError const* const err = toPtr(uncheckedORefToInapplicableError(v));

            fputs("#<inapplicable-error ", dest);
            print(state, dest, toORef(err->callee));
            putc('>', dest);
        } else  {
            assert(false); // FIXME
        }
        break;
    }
}

