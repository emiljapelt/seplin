#include <string.h>

#include "ISA.h"

char* instruction_to_string(int inst) {
    switch (inst) {  
        case HALT: return "HALT";
        case STOP: return "STOP";
        case CALL: return "CALL";
        case GOTO: return "GOTO";
        case IF_TRUE: return "IF_TRUE";
        case PLACE_BYTE: return "PLACE_BYTE";
        case PLACE_FULL: return "PLACE_FULL";
        case CLONE_FULL: return "CLONE_FULL";
        case CLONE_HALF: return "CLONE_HALF";
        case CLONE_SHORT: return "CLONE_SHORT";
        case CLONE_BYTE: return "CLONE_BYTE";
        case FETCH_FULL: return "FETCH_FULL";
        case FETCH_HALF: return "FETCH_HALF";
        case FETCH_SHORT: return "FETCH_SHORT";
        case FETCH_BYTE: return "FETCH_BYTE";
        case FIELD_FETCH: return "FIELD_FETCH";
        case DECLARE_FULL: return "DECLARE_FULL";
        case DECLARE_HALF: return "DECLARE_HALF";
        case DECLARE_SHORT: return "DECLARE_SHORT";
        case DECLARE_BYTE: return "DECLARE_BYTE";
        case DECLARE_STRUCT: return "DECLARE_STRUCT";
        case ASSIGN_FULL: return "ASSIGN_FULL";
        case ASSIGN_HALF: return "ASSIGN_HALF";
        case ASSIGN_SHORT: return "ASSIGN_SHORT";
        case ASSIGN_BYTE: return "ASSIGN_BYTE";
        case REF_ASSIGN: return "REF_ASSIGN";
        case FIELD_ASSIGN: return "FIELD_ASSIGN";
        case INT_ADD: return "INT_ADD";
        case INT_MUL: return "INT_MUL";
        case INT_SUB: return "INT_SUB";
        case FULL_EQ: return "FULL_EQ";
        case INT_LT: return "INT_LT";
        case BOOL_EQ: return "BOOL_EQ";
        case BOOL_NOT: return "BOOL_NOT";
        case BOOL_AND: return "BOOL_AND";
        case BOOL_OR: return "BOOL_OR";
        case GETSP: return "GETSP";
        case GETBP: return "GETBP";
        case MODSP: return "MODSP";
        case FREE_VAR: return "FREE_VAR";
        case FREE_VARS: return "FREE_VARS";
        case PRINT_INT: return "PRINT_INT";
        case PRINT_BOOL: return "PRINT_BOOL";
        case STACK_FETCH: return "STACK_FETCH";
        case BP_FETCH: return "BP_FETCH";
        case SIZE_OF: return "SIZE_OF";
        case START: return "START";
        case REF_FETCH: return "REF_FETCH";
        case INCR_REF: return "INCR_REF";
        case PRINT_CHAR: return "PRINT_CHAR";
        case GET_INPUT: return "GET_INPUT";
        case HALF_EQ: return "HALF_EQ";
        case SHORT_EQ: return "SHORT_EQ";
        case BYTE_EQ: return "BYTE_EQ";
        default: return "?";
    }
}

int string_to_instruction(char* str) {
    if (strcmp(str, "HALT") == 0) return HALT;
    if (strcmp(str, "STOP") == 0) return STOP;
    if (strcmp(str, "CALL") == 0) return CALL;
    if (strcmp(str, "GOTO") == 0) return GOTO;
    if (strcmp(str, "IF_TRUE") == 0) return IF_TRUE;
    if (strcmp(str, "PLACE_BYTE") == 0) return PLACE_BYTE;
    if (strcmp(str, "PLACE_FULL") == 0) return PLACE_FULL;
    if (strcmp(str, "CLONE_FULL") == 0) return CLONE_FULL;
    if (strcmp(str, "CLONE_HALF") == 0) return CLONE_HALF;
    if (strcmp(str, "CLONE_SHORT") == 0) return CLONE_SHORT;
    if (strcmp(str, "CLONE_BYTE") == 0) return CLONE_BYTE;
    if (strcmp(str, "FETCH_FULL") == 0) return FETCH_FULL;
    if (strcmp(str, "FETCH_HALF") == 0) return FETCH_HALF;
    if (strcmp(str, "FETCH_SHORT") == 0) return FETCH_SHORT;
    if (strcmp(str, "FETCH_BYTE") == 0) return FETCH_BYTE;
    if (strcmp(str, "FIELD_FETCH") == 0) return FIELD_FETCH;
    if (strcmp(str, "DECLARE_FULL") == 0) return DECLARE_FULL;
    if (strcmp(str, "DECLARE_HALF") == 0) return DECLARE_HALF;
    if (strcmp(str, "DECLARE_SHORT") == 0) return DECLARE_SHORT;
    if (strcmp(str, "DECLARE_BYTE") == 0) return DECLARE_BYTE;
    if (strcmp(str, "DECLARE_STRUCT") == 0) return DECLARE_STRUCT;
    if (strcmp(str, "ASSIGN_FULL") == 0) return ASSIGN_FULL;
    if (strcmp(str, "ASSIGN_HALF") == 0) return ASSIGN_HALF;
    if (strcmp(str, "ASSIGN_SHORT") == 0) return ASSIGN_SHORT;
    if (strcmp(str, "ASSIGN_BYTE") == 0) return ASSIGN_BYTE;
    if (strcmp(str, "FIELD_ASSIGN") == 0) return FIELD_ASSIGN;
    if (strcmp(str, "INT_ADD") == 0) return INT_ADD;
    if (strcmp(str, "INT_MUL") == 0) return INT_MUL;
    if (strcmp(str, "INT_SUB") == 0) return INT_SUB;
    if (strcmp(str, "FULL_EQ") == 0) return FULL_EQ;
    if (strcmp(str, "INT_LT") == 0) return INT_LT;
    if (strcmp(str, "BOOL_EQ") == 0) return BOOL_EQ;
    if (strcmp(str, "BOOL_NOT") == 0) return BOOL_NOT;
    if (strcmp(str, "BOOL_AND") == 0) return BOOL_AND;
    if (strcmp(str, "BOOL_OR") == 0) return BOOL_OR;
    if (strcmp(str, "GETSP") == 0) return GETSP;
    if (strcmp(str, "GETBP") == 0) return GETBP;
    if (strcmp(str, "MODSP") == 0) return MODSP;
    if (strcmp(str, "FREE_VAR") == 0) return FREE_VAR;
    if (strcmp(str, "FREE_VARS") == 0) return FREE_VARS;
    if (strcmp(str, "PRINT_INT") == 0) return PRINT_INT;
    if (strcmp(str, "PRINT_BOOL") == 0) return PRINT_BOOL;
    if (strcmp(str, "STACK_FETCH") == 0) return STACK_FETCH;
    if (strcmp(str, "BP_FETCH") == 0) return BP_FETCH;
    if (strcmp(str, "SIZE_OF") == 0) return SIZE_OF;
    if (strcmp(str, "START") == 0) return START;
    if (strcmp(str, "REF_FETCH") == 0) return REF_FETCH;
    if (strcmp(str, "INCR_REF") == 0) return INCR_REF;
    if (strcmp(str, "PRINT_CHAR") == 0) return PRINT_CHAR;
    if (strcmp(str, "HALF_EQ") == 0) return HALF_EQ;
    if (strcmp(str, "SHORT_EQ") == 0) return SHORT_EQ;
    if (strcmp(str, "BYTE_EQ") == 0) return BYTE_EQ;
    return -1;
}
