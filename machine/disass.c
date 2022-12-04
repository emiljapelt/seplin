#include <stdio.h>

#include "disass.h"
#include "file_analysis.h"
#include "ISA.h"
#include "types.h"

void dissas(byte_t* p, full_t file_len) {
    byte_t* temp = p;
    int diff = 0;
    printf("    ENTRY POINTS:\n");
    print_entry_points(p);
    find_global_vars_start(temp, &temp);
    diff = temp-p;
    p += temp-p;
    file_len -= diff;
    int offset = 0;
    full_t globvars = p[offset];
    offset += 8;
    printf("\n    GLOBAL VARIABLES:\n");
    for(int i = 0; i < globvars; i++) {
        byte_t type = ((byte_t*)p)[offset++];
        switch (type) {
            case BOOL:;
                char* b_value = (((byte_t*)p)[offset++]) ? "true" : "false";
                printf("%s: %s\n", type_name(type), b_value);
                break;
            case INT:;
                full_t i_value = p[offset];
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
        char* name_string = instruction_to_string(p[i]);
        switch (p[i]) {
            case PLACE_CHAR:
            case PLACE_BOOL:
                printf("%i: %s %x\n", i, name_string, *((byte_t*)(p+i+1)));
                i+=1;
                break;
            case CALL:
            case GOTO:
            case IF_TRUE:
            case PLACE_INT:
            case MODSP:
            case FREE_VARS:
            case STACK_FETCH:
            case BP_FETCH:
                printf("%i: %s %lld\n", i, name_string, *((full_t*)(p+i+1)));
                i+=8;
                break;
            default:
                printf("%i: %s\n", i, name_string);
                break;
        }
    }
}