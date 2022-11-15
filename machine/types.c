#include <string.h>
#include <stdlib.h>

#include "defs.h"
#include "memory.h"
#include "types.h"

char* type_name(char index) {
    switch (index) {
        case BOOL: return "bool";
        case INT: return "int";
        default: return "?";
    }
}

byte* allocate_type(byte type) {
    return allocate(TYPE_SIZE(type));
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