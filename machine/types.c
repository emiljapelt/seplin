#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "memory.h"
#include "types.h"

char* type_name(byte_t index) {
    switch (index) {
        case BOOL: return "bool";
        case INT: return "int";
        case CHAR: return "char";
        default: return "?";
    }
}
