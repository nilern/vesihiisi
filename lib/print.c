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
    } else {
        assert(false); // FIXME
    }
}

