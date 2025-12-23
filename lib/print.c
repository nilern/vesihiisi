#include "print.h"

static void print(State const* state, FILE* dest, ORef v) {
    switch (getTag(v)) {
    case TAG_FIXNUM:
        fprintf(dest, "%ld", uncheckedFixnumToInt(v));
        break;
        
    case TAG_FLONUM:
        assert(false); // FIXME
        break;
        
    case TAG_CHAR:
        fprintf(dest, "#\"%c\"", uncheckedORefToChar(v));
        break;
        
    case TAG_BOOL:
        if (uncheckedORefToBool(v)) {
            fprintf(dest, "#t");
        } else {
            fprintf(dest, "#f");
        }
        break;
        
    case TAG_HEAPED:
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
        } else if (isa(state, state->arrayType, v)) {
            ORef const* const vs = arrayToPtr(uncheckedORefToArray(v));
            fprintf(dest, "#<array!");
            size_t const count = (uintptr_t)fixnumToInt(flexLength(v));
            for (size_t i = 0; i < count; ++i) {
                fputc(' ', dest);
                print(state, dest, vs[i]);
            }
            putc('>', dest);
        } else if (isClosure(state, v)) {
            fprintf(dest, "#<fn>");
        } else if (isContinuation(state, v)) {
            fprintf(dest, "#<continuation>");
        } else if (isType(state, v)) {
            fprintf(dest, "#<type>");
        } else if (isTypeError(state, v)) {
            TypeError const* const err = typeErrorToPtr(uncheckedORefToTypeError(v));
            fputs("#<type-error ", dest);
            print(state, dest, typeToORef(err->type));
            putc(' ', dest);
            print(state, dest, err->val);
            putc('>', dest);
        } else if (isa(state, state->arityErrorType, v)) {
            ArityError const* const err = arityErrorToPtr(uncheckedORefToArityError(v));
            fputs("#<arity-error ", dest);
            print(state, dest, closureToORef(err->callee));
            putc(' ', dest);
            print(state, dest, fixnumToORef(err->callArgc));
            putc('>', dest);
        } else  {
            assert(false); // FIXME
        }
        break;

    case TAG_BROKEN_HEART: assert(false); break; // Unreachable
    }
}

