static void print(FILE* dest, ORef v) {
    if (isFixnum(v)) {
        fprintf(dest, "%d", uncheckedFixnumToInt32(v));
    }
}

