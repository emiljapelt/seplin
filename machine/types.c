#include <string.h>
#include <stdlib.h>

#include "defs.h"
#include "types.h"

char* type_name(char index) {
    switch (index) {
        case BOOL: return "bool";
        case INT: return "int";
        default: return "?";
    }
}

byte type_size(char index) {
    switch (index) {
        case ADDR: return 8;
        case BOOL: return 1;
        case INT: return 8;
        default: return 0;
    }
}

word parse_int(char* str) {
    return atoi(str);
}

byte parse_bool(char* str) {
    if (strcmp(str, "true") == 0) {
        return 1;
    }
    else if (strcmp(str, "false") == 0) {
        return 0;
    }
    else return -1;
}

byte* payload(word* addr) {
    return ((byte*)addr)+1;
}