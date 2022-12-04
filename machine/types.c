#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "memory.h"
#include "types.h"

char* type_name(char index) {
    switch (index) {
        case BOOL: return "bool";
        case INT: return "int";
        case CHAR: return "char";
        default: return "?";
    }
}

ufull_t parse_int(char* str) {
    return atoi(str);
}

byte_t parse_bool(char* str) {
    if (strcmp(str, "true") == 0) {
        return 1;
    }
    else if (strcmp(str, "false") == 0) {
        return 0;
    }
    else return -1;
}

byte_t parse_char(char* str) {
    if (str[0] == '\\') {
        if (str[2] != 0) return -1;
        switch (str[1]) {
            case 'n': return '\n';
            default: return -1;
        }
    }
    else {
        if (str[1] != 0) return -1;
        return str[0];
    }
}