#include <stdio.h>
#include <string.h>

#include "file_analysis.h"
#include "memory.h"
#include "types.h"
#include "ISA.h"

void skip_string(byte_t** file) {
    while(**file != 0) *file += 1;
    *file += 1;
}

int type_rep_size(byte_t* file) {
    int size = 1;
    switch (*(file+1)) {
        case 0: // simple
            size += 2;
            break;
        case 1: // array
            size += 1 + type_rep_size(file+2);
            break;
        case 2: // struct
            size += 9;
            break;
        case 3:
            size += 2;
            break;
    }
    return size;
}

void skip_type(byte_t** file) {
    *file += type_rep_size(*file);
}

struct file_segments find_segments(byte_t* file) {
    struct file_segments segments;
    segments.struct_segment = file;

    // Find globvar_segment / skip over struct_segment

    full_t i = *(full_t*)file;
    file += 8;
    while(i > 0) {
        skip_string(&file);
        byte_t f = *(byte_t*)file;
        file += 1;
        while(f > 0) {
            skip_type(&file);
            f--;
        }
        i--;
    }
    segments.global_var_segment = file;

    // Find entry_point_segment / skip over globvar_segment

    i = *(full_t*)file;
    file += 8;
    while (i > 0) {
        skip_string(&file);
        skip_type(&file);
        i--;
    }
    segments.entry_point_segment = file;
    
    // Find instructions_segment / skip over entry_point_segment

    i = *(full_t*)file;
    file += 8;
    while (i > 0) {
        skip_string(&file);
        file += 8;
        byte_t ac = *(byte_t*)file;
        file += 1;
        while (ac > 0) {
            skip_type(&file);
            ac--;
        }
        i--;
    }
    segments.instructions_segment = file;
    

    return segments;
}

struct entry_point_info find_entry_point(byte_t* ptr, char* name) {
    full_t count = *(full_t*)ptr;
    ptr += 8;
    while (count > 0) {
        if (strcmp(name, ptr) == 0) {
            skip_string(&ptr);
            // Return entry point address
            struct entry_point_info info;
            info.found = true;
            info.instruction_addr = *(full_t*)ptr;
            ptr += 8;
            info.argument_count = *(byte_t*)ptr;
            ptr += 1;
            info.argument_types = ptr;
            return info;
        }
        else {
            // Skip to next entry point
            skip_string(&ptr);
            ptr += 8;
            int argc = *(byte_t*)ptr;
            ptr += 1;
            while (argc > 0) {
                skip_type(&ptr);
                argc--;
            }
        }
        count--;
    }

    struct entry_point_info info = {false, 0,0,0};
    return info;
}

void print_type(byte_t* ptr, byte_t* struct_seg) {
    if (*ptr == 1) printf("locked ");
    ptr += 1;
    switch (*ptr) {
        case 0: // simple
            ptr += 1;
            switch (*ptr) {
                case 0: // int
                    printf("int");
                    break;
                case 1: // bool
                    printf("bool");
                    break;
                case 2: // char
                    printf("char");
                    break;
            }
            break;
        case 1: // array
            ptr += 1;
            print_type(ptr, struct_seg);
            printf("[]");
            break;
        case 2: // struct
            ptr += 1;
            full_t idx = *(full_t*)ptr;
            struct_seg += 8;
            while (idx > 0) {
                skip_string(&struct_seg);
                int f = *(byte_t*)struct_seg;
                struct_seg += 1;
                while (f > 0) {
                    skip_type(&struct_seg);
                    f--;
                }
                idx--;
            }
            printf("%s", struct_seg);
            break;
        case 3: // generic
            ptr += 1;
            printf("%c", *ptr);
            ptr += 1;
            break;
    }
}

void print_structs(byte_t* structs) {
    byte_t* ptr = structs;
    full_t struct_count = *(full_t*)ptr;
    if (!struct_count) { printf("   None\n"); return; }
    ptr += 8;

    while (struct_count > 0) {
        printf("    %s(", ptr);
        skip_string(&ptr);
        byte_t fields = *(byte_t*)ptr;
        ptr += 1;
        while (fields > 0) {
            print_type(ptr, structs);
            if (fields > 1) printf(", ");
            skip_type(&ptr);
            fields--;
        }
        
        printf(")\n");
        struct_count--;
    }
}

void print_global_vars(byte_t* ptr, byte_t* structs) {
    full_t globvar_count = *(full_t*)ptr;
    if (!globvar_count) { printf("   None\n"); return; }
    ptr += 8;

    while (globvar_count > 0) {
        printf("    %s: ", ptr);
        skip_string(&ptr);
        print_type(ptr, structs);
        skip_type(&ptr);
        printf("\n");
        globvar_count--;
    }
    
}

void print_entry_points(byte_t* ptr, byte_t* struct_seg) {
    full_t entry_point_count = *(full_t*)ptr;
    if (!entry_point_count) { printf("   None\n"); return; }
    ptr += 8;

    while (entry_point_count > 0) {
        printf("    %s(", ptr);
        skip_string(&ptr);

        full_t addr = *(full_t*)ptr;
        ptr += 8;

        byte_t argc = *ptr;
        ptr += 1;

        while(argc > 0) {
            print_type(ptr, struct_seg);
            skip_type(&ptr);
            if (argc > 1) printf(", ");
            argc--;
        }

        printf(") @ %lld\n", addr);
        entry_point_count--;
    }
}

void dissas(byte_t* origin, byte_t* instr, full_t file_len) {
    full_t instr_segment_length = file_len - ((full_t)(instr - origin));
    for(int i = 0; i < instr_segment_length; i++) {
        char* name_string = instruction_to_string(instr[i]);
        switch (instr[i]) {
            case PLACE_CHAR:
            case PLACE_BOOL:
                printf("    %i: %s %x\n", i, name_string, *((byte_t*)(instr+i+1)));
                i+=1;
                break;
            case GET_INPUT:
            case CALL:
            case GOTO:
            case IF_TRUE:
            case PLACE_INT:
            case MODSP:
            case FREE_VARS:
            case STACK_FETCH:
            case BP_FETCH:
                printf("    %i: %s %lld\n", i, name_string, *((full_t*)(instr+i+1)));
                i+=8;
                break;
            default:
                printf("    %i: %s\n", i, name_string);
                break;
        }
    }
}