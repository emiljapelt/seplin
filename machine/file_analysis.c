#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "types.h"

void find_global_vars_start(byte* ptr, byte** progresser) {
    word entry_points = ((word*)ptr)[0];
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

void find_instruction_start(byte* ptr, byte** progresser) {
    word variables = ((word*)ptr)[0];
    ptr += 8;

    int i = 0;
    while (i < variables) {
        byte type = *ptr;
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

word* find_entry_point(byte* ptr, char* routine) {
    word entry_points = ((word*)ptr)[0];
    ptr += 8;

    int i = 0;
    while (i < entry_points) {
        if (strcmp(routine, ptr) == 0) {
            // Return entry point address
            while(true) {
                if (*ptr == 0b00000000) {ptr++; break;}
                else ptr++;
            }
            return ((word*)ptr);
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
    word entry_points = ((word*)ptr)[0];
    ptr += 8;

    int i = 0;
    while (i < entry_points) {
        while (true) {
            printf("%c", *ptr);
            ptr++;
            if (*ptr == 0b00000000) {ptr++; break;}
        }
        printf("( ");

        word addr = ((word*)ptr)[0];
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