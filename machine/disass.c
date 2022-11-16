#include <stdio.h>

#include "disass.h"
#include "file_analysis.h"
#include "ISA.h"
#include "defs.h"
#include "types.h"

void dissas(byte* p, word file_len) {
    byte* temp = p;
    int diff = 0;
    printf("    ENTRY POINTS:\n");
    print_entry_points(p);
    find_global_vars_start(temp, &temp);
    diff = temp-p;
    p += temp-p;
    file_len -= diff;
    int offset = 0;
    word globvars = p[offset];
    offset += 8;
    printf("\n    GLOBAL VARIABLES:\n");
    for(int i = 0; i < globvars; i++) {
        byte type = ((byte*)p)[offset++];
        switch (type) {
            case BOOL:;
                char* b_value = (((byte*)p)[offset++]) ? "true" : "false";
                printf("%s: %s\n", type_name(type), b_value);
                break;
            case INT:;
                word i_value = p[offset];
                offset += 8;
                printf("%s: %lli\n", type_name(type), i_value);
                break;
            default: 
                printf("Global variable of unknown type");
                return;
        }
    }
    find_instruction_start(temp, &temp);
    diff = temp-p;
    p += diff;
    file_len -= diff;
    printf("\n    INSTRUCTIONS:\n");
    for(int i = 0; i < file_len; i++) {
        switch (p[i]) {
            case HALT:
                printf("%i: HALT\n", i);
                break;
            case STOP:
                printf("%i: STOP\n", i);
                break;
            case CALL:
                printf("%i: CALL %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            case GOTO:
                printf("%i: GOTO %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            case IF_TRUE:
                printf("%i: IF_TRUE %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            case PLACE_BOOL:
                printf("%i: PLACE_BOOL %x\n", i, *((byte*)(p+i+1)));
                i+=1;
                break;
            case PLACE_INT:
                printf("%i: PLACE_INT %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            case CLONE_FULL:
                printf("%i: CLONE_FULL\n", i);
                break;
            case CLONE_HALF:
                printf("%i: CLONE_HALF\n", i);
                break;
            case CLONE_SHORT:
                printf("%i: CLONE_SHORT\n", i);
                break;
            case CLONE_BYTE:
                printf("%i: CLONE_BYTE\n", i);
                break;
            case FETCH_FULL:
                printf("%i: FETCH_FULL\n", i);
                break;
            case FETCH_HALF:
                printf("%i: FETCH_HALF\n", i);
                break;
            case FETCH_SHORT:
                printf("%i: FETCH_SHORT\n", i);
                break;
            case FETCH_BYTE:
                printf("%i: FETCH_BYTE\n", i);
                break;
            case FIELD_FETCH:
                printf("%i: FIELD_FETCH\n", i);
                break;
            case DECLARE_FULL:
                printf("%i: DECLARE_FULL\n", i);
                break;
            case DECLARE_HALF:
                printf("%i: DECLARE_HALF\n", i);
                break;
            case DECLARE_SHORT:
                printf("%i: DECLARE_SHORT\n", i);
                break;
            case DECLARE_BYTE:
                printf("%i: DECLARE_BYTE\n", i);
                break;
            case DECLARE_STRUCT:
                printf("%i: DECLARE_STRUCT %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            case ASSIGN_FULL:
                printf("%i: ASSIGN_FULL\n", i);
                break;
            case ASSIGN_HALF:
                printf("%i: ASSIGN_HALF\n", i);
                break;
            case ASSIGN_SHORT:
                printf("%i: ASSIGN_SHORT\n", i);
                break;
            case ASSIGN_BYTE:
                printf("%i: ASSIGN_BYTE\n", i);
                break;
            case FIELD_ASSIGN:
                printf("%i: FIELD_ASSIGN\n", i);
                break;
            case INT_ADD:
                printf("%i: INT_ADD\n", i);
                break;
            case INT_MUL:
                printf("%i: INT_MUL\n", i);
                break;
            case INT_SUB:
                printf("%i: INT_SUB\n", i);
                break;
            case INT_EQ:
                printf("%i: INT_EQ\n", i);
                break;
            case INT_LT:
                printf("%i: INT_LT\n", i);
                break;
            case BOOL_EQ:
                printf("%i: BOOL_EQ\n", i);
                break;
            case BOOL_NOT:
                printf("%i: BOOL_NOT\n", i);
                break;
            case BOOL_AND:
                printf("%i: BOOL_AND\n", i);
                break;
            case BOOL_OR:
                printf("%i: BOOL_OR\n", i);
                break;
            case GETSP:
                printf("%i: GETSP\n", i);
                break;
            case GETBP:
                printf("%i: GETBP\n", i);
                break;
            case MODSP:
                printf("%i: MODSP %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            case FREE_VAR:
                printf("%i: FREE_VAR\n", i);
                break;
            case FREE_VARS:
                printf("%i: FREE_VARS %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            case PRINT_VAR:
                printf("%i: PRINT_VAR\n", i);
                break;
            case PRINT_INT:
                printf("%i: PRINT_INT\n", i);
                break;
            case PRINT_BOOL:
                printf("%i: PRINT_BOOL\n", i);
                break;
            case STACK_FETCH:
                printf("%i: STACK_FETCH %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            case BP_FETCH:
                printf("%i: BP_FETCH %lld\n", i, *((word*)(p+i+1)));
                i+=8;
                break;
            default: return;
        }
    }
}