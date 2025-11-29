static void print(FILE* dest, ORef v) {
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
    }
    
    assert(false); // FIXME
}

