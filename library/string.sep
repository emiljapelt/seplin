
external compress(str: char[]) {
    for(i ::= 0; i < |str|; i +:= 1) {
        if (str[i] != null) for(j ::= 0; j < i; j +:= 1) {
            if (str[j] != null) if (str[i] = str[j]) {
                str[i] := str[j];
                break;
            }
        }
    }
}

external out(str: const char[]) {
    for(i ::= 0; i < |str|; i +:= 1) if (str[i] != null) print str[i]; else print '_';
}