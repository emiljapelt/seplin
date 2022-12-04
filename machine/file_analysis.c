#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "types.h"

void find_global_vars_start(byte_t* ptr, byte_t** progresser) {
    full_t entry_points = ((full_t*)ptr)[0];
    ptr += 8;

    int i = 0;
    while (i < entry_points) {
        while(1) {
            if (*ptr == 0b00000000) {ptr++; break;}
            else ptr++;
        }
        ptr += 8;
        char argc = *ptr;
        ptr += argc+1;
        i++;
    }
    *progresser = ptr;
}

void find_instruction_start(byte_t* ptr, byte_t** progresser) {
    full_t variables = ((full_t*)ptr)[0];
    ptr += 8;

    int i = 0;
    while (i < variables) {
        byte_t type = *ptr;
        switch (type){
            case BOOL:
                ptr += 2;
                break;
            case INT:
                ptr += 9;
                break;
            default:
                break;
        }
        i++;
    }
    *progresser = ptr;
}

full_t* find_entry_point(byte_t* ptr, char* routine) {
    full_t entry_points = ((full_t*)ptr)[0];
    ptr += 8;

    int i = 0;
    while (i < entry_points) {
        if (strcmp(routine, ptr) == 0) {
            // Return entry point address
            while(true) {
                if (*ptr == 0b00000000) {ptr++; break;}
                else ptr++;
            }
            return ((full_t*)ptr);
        }
        else {
            // Skip to next entry point
            while(true) {
                if (*ptr == 0b00000000) {ptr++; break;}
                else ptr++;
            }
            ptr += 8;
            char argc = *ptr;
            ptr += argc+1;
        }
        i++;
    }

    return 0;
}

void print_entry_points(char* ptr) {
    full_t entry_points = ((full_t*)ptr)[0];
    ptr += 8;

    int i = 0;
    while (i < entry_points) {
        while (true) {
            printf("%c", *ptr);
            ptr++;
            if (*ptr == 0b00000000) {ptr++; break;}
        }
        printf("( ");

        full_t addr = ((full_t*)ptr)[0];
        ptr += 8;

        char argc = *ptr;
        ptr++;
        int o = 0;
        while(o < argc) {
            printf("%s ", type_name(*ptr));
            ptr++;
            o++;
        }

        printf(") @ %lld\n", addr);
        i++;
    }
}