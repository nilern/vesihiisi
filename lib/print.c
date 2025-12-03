static void print(State const* state, FILE* dest, ORef v) {
    if (isFixnum(v)) {
        fprintf(dest, "%ld", uncheckedFixnumToInt(v));
    } else if (isChar(v)) {
        fprintf(dest, "#\"%c\"", uncheckedORefToChar(v));
    } else if (isBool(v)) {
        if (uncheckedORefToBool(v)) {
            fprintf(dest, "#t");
        } else {
            fprintf(dest, "#f");
        }
    } else if (isString(state, v)) {
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
    } else  {
        assert(false); // FIXME
    }
}

