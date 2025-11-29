static void print(FILE* dest, Type const* stringType, ORef v) {
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
    } else if (isString(stringType, v)) {
        StringRef const s = uncheckedORefToString(v);
        Str const str = stringStr(s);
        fprintf(dest, "\"%.*s\"", (int)str.len, str.data);
    } else {
        assert(false); // FIXME
    }
}

