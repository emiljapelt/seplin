#include <string.h>

#include "ISA.h"

char* instruction_to_string(int inst) {
    switch (inst) {  
        case HALT: return "HALT";
        case STOP: return "STOP";
        case CALL: return "CALL";
        case GOTO: return "GOTO";
        case IF_TRUE: return "IF_TRUE";
        case PLACE_BOOL: return "PLACE_BOOL";
        case PLACE_INT: return "PLACE_INT";
        case CLONE_FULL: return "CLONE_FULL";
        case CLONE_HALF: return "CLONE_HALF";
        case CLONE_SHORT: return "CLONE_SHORT";
        case CLONE_BYTE: return "CLONE_BYTE";
        case FETCH_BOOL: return "FETCH_BOOL";
        case FETCH_INT: return "FETCH_INT";
        case DECLARE_BOOL: return "DECLARE_BOOL";
        case DECLARE_INT: return "DECLARE_INT";
        case ASSIGN_BOOL: return "ASSIGN_BOOL";
        case ASSIGN_INT: return "ASSIGN_INT";
        case INT_ADD: return "INT_ADD";
        case INT_MUL: return "INT_MUL";
        case INT_SUB: return "INT_SUB";
        case INT_EQ: return "INT_EQ";
        case INT_LT: return "INT_LT";
        case BOOL_EQ: return "BOOL_EQ";
        case BOOL_NOT: return "BOOL_NOT";
        case BOOL_AND: return "BOOL_AND";
        case BOOL_OR: return "BOOL_OR";
        case GETSP: return "GETSP";
        case GETBP: return "GETBP";
        case MODSP: return "MODSP";
        case CLONE_FRAME: return "CLONE_FRAME";
        case FETCH_ADDR: return "FETCH_ADDR";
        case FREE_VAR: return "FREE_VAR";
        case FREE_VARS: return "FREE_VARS";
        case PRINT_VAR: return "PRINT_VAR";
        case PRINT_INT: return "PRINT_INT";
        case PRINT_BOOL: return "PRINT_BOOL";
        case STACK_FETCH: return "STACK_FETCH";
        case BP_FETCH: return "BP_FETCH";
        default: return "?";
    }
}

int string_to_instruction(char* str) {
    if (strcmp(str, "HALT") == 0) return HALT;
    if (strcmp(str, "STOP") == 0) return STOP;
    if (strcmp(str, "CALL") == 0) return CALL;
    if (strcmp(str, "GOTO") == 0) return GOTO;
    if (strcmp(str, "IF_TRUE") == 0) return IF_TRUE;
    if (strcmp(str, "PLACE_BOOL") == 0) return PLACE_BOOL;
    if (strcmp(str, "PLACE_INT") == 0) return PLACE_INT;
    if (strcmp(str, "CLONE_FULL") == 0) return CLONE_FULL;
    if (strcmp(str, "CLONE_HALF") == 0) return CLONE_HALF;
    if (strcmp(str, "CLONE_SHORT") == 0) return CLONE_SHORT;
    if (strcmp(str, "CLONE_BYTE") == 0) return CLONE_BYTE;
    if (strcmp(str, "FETCH_BOOL") == 0) return FETCH_BOOL;
    if (strcmp(str, "FETCH_INT") == 0) return FETCH_INT;
    if (strcmp(str, "DECLARE_BOOL") == 0) return DECLARE_BOOL;
    if (strcmp(str, "DECLARE_INT") == 0) return DECLARE_INT;
    if (strcmp(str, "ASSIGN_BOOL") == 0) return ASSIGN_BOOL;
    if (strcmp(str, "ASSIGN_INT") == 0) return ASSIGN_INT;
    if (strcmp(str, "INT_ADD") == 0) return INT_ADD;
    if (strcmp(str, "INT_MUL") == 0) return INT_MUL;
    if (strcmp(str, "INT_SUB") == 0) return INT_SUB;
    if (strcmp(str, "INT_EQ") == 0) return INT_EQ;
    if (strcmp(str, "INT_LT") == 0) return INT_LT;
    if (strcmp(str, "BOOL_EQ") == 0) return BOOL_EQ;
    if (strcmp(str, "BOOL_NOT") == 0) return BOOL_NOT;
    if (strcmp(str, "BOOL_AND") == 0) return BOOL_AND;
    if (strcmp(str, "BOOL_OR") == 0) return BOOL_OR;
    if (strcmp(str, "GETSP") == 0) return GETSP;
    if (strcmp(str, "GETBP") == 0) return GETBP;
    if (strcmp(str, "MODSP") == 0) return MODSP;
    if (strcmp(str, "CLONE_FRAME") == 0) return CLONE_FRAME;
    if (strcmp(str, "FETCH_ADDR") == 0) return FETCH_ADDR;
    if (strcmp(str, "FREE_VAR") == 0) return FREE_VAR;
    if (strcmp(str, "FREE_VARS") == 0) return FREE_VARS;
    if (strcmp(str, "PRINT_VAR") == 0) return PRINT_VAR;
    if (strcmp(str, "PRINT_INT") == 0) return PRINT_INT;
    if (strcmp(str, "PRINT_BOOL") == 0) return PRINT_BOOL;
    if (strcmp(str, "STACK_FETCH") == 0) return STACK_FETCH;
    if (strcmp(str, "BP_FETCH") == 0) return BP_FETCH;
    return -1;
}
