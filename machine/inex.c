#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ISA.h"
#include "types.h"
#include "defs.h"
#include "disass.h"
#include "file_analysis.h"
#include "commands.h"

#define STACKSIZE 8000
#define HEAPSIZE 1000


void load_file(char* file_name, byte** out_file, word* out_len) {
    FILE* fp;
    char* buffer;
    long numbytes;
    
    fp = fopen(file_name, "r");
    
    if (fp == NULL) { out_file = 0; return; }
    
    fseek(fp, 0L, SEEK_END);
    numbytes = ftell(fp);
    fseek(fp, 0L, SEEK_SET);	
    buffer = (byte*)calloc(numbytes, sizeof(char));	
    
    if(buffer == NULL) { out_file = 0; return; }
    
    fread(buffer, sizeof(char), numbytes, fp);
    fclose(fp);

    *out_file = buffer;
    *out_len = numbytes;
}

void print_stack(byte* stack, uword bp, uword sp) {
    byte done = false;
    printf("[");
    uword i = 0;
    word to_top = sp - (sp % 8);
    while(i < to_top) {
        if (i == bp) printf("bp>");
        printf("0x%llx, ", *(word*)(stack + i));
        i+=8;
    }
    if (to_top != sp) {
        printf("| ");
        byte x = 0;
        while(x < sp % 8) {
            printf("0x%x, ", *(byte*)(stack + i + x));
            x++;
        }
    }
    printf("_ ]\n");
}

byte* allocate(byte type, byte constant) {
    int size;
    switch (type) {
        case BOOL:
            size = 2;
            break;
        case INT:
            size = 9;
            break;
        default:
            return NULL;
    }
    byte* alloc = (byte*)malloc(size);
    byte header = type;
    if (constant) header = header | 0b10000000;
    *alloc = header;
    return alloc;
}

void print_var(word* target) {
    switch (type_index(target)) {
        case BOOL:
            if (*payload(target)) printf("true\n");
            else printf("false\n");
            break;
        case INT:
            printf("%lld\n", *((word*)payload(target)));
            break;
        default:
            printf("?\n");
            break;
    }
}

char is_on_stack(word* addr, byte* stack, uword sp) {
    if (stack <= (byte*)addr && (byte*)addr <= (stack + sp)) return true;
    else return false;
}

int move(char type, int steps) {
    return type_size(type) * steps;
}

void follow_trail(word** target, byte* stack, uword sp) {
    word* temp = *target;
    while(is_on_stack(temp, stack, sp)) temp = (word*)*temp;
    *target = temp;
}

void try_free(word* addr, byte* stack, uword sp) {
    if (addr == 0) return;
    if (is_on_stack(addr, stack, sp)) return;
    free(addr);
}

int run(byte* p, word entry_point, byte stack[], int glob_var_count, int argument_count, byte debug) {
    uword ip = entry_point;
    uword sp = move(ADDR, glob_var_count+argument_count);
    uword bp = move(ADDR, glob_var_count);

    while(true) {
        byte i = p[ip];

        if (debug) print_stack(stack, bp, sp);
        if (debug) printf("instruction #%llu: 0x%x %s\n", ip, i, instruction_to_string(i));

        if (i == HALT) {
            if (debug) printf("Halting...\n");
            return 0;
        }
        else if (i == STOP) {
            if (bp == move(ADDR, glob_var_count)) {
                if (debug) printf("Halting...\n");
                return 0;
            }
            uword old_bp = *(word*)(stack + bp + move(ADDR, -1));
            uword next_ip = *(word*)(stack + bp + move(ADDR, -2));;
            uword i = bp;
            // while(i <= sp) {
            //    if (!is_on_stack((word*)(stack+i), stack, sp)) free((word*)(stack+i));
            //    i += move(ADDR, 1);
            // }
            sp = bp - move(ADDR, 3);
            sp -= move(ADDR, *((word*)(stack + sp)));
            bp = old_bp;
            ip = next_ip;
        }
        else if (i == CALL) {
            word arg_count = *(word*)(stack + sp + move(INT, -1));
            uword arg_start = sp - move(ADDR, arg_count+1);
            *(word*)(stack + sp) = ip+9;
            *(word*)(stack + sp + move(ADDR, 1)) = (word)bp;
            for(int i = 0; i < arg_count; i++) {
                word* arg = *(word**)(stack + arg_start + move(ADDR, i));
                if (is_on_stack(arg, stack, sp)) {
                    *(word*)(stack + sp + move(ADDR, 2+i)) = (word)(arg);
                }
                else {
                    *(word*)(stack + sp + move(ADDR, 2+i)) = (word)(stack + arg_start + move(ADDR, i));
                }
            }
            bp = sp + move(ADDR, 2);
            sp += move(ADDR, 2+arg_count);
            ip = *(word*)(p+ip+1);
        }
        else if (i == GOTO) {
            word target = *(word*)(p + ip + 1);
            ip = target;
        }
        else if (i == IF_TRUE) {
            if (*(byte*)(stack + sp + move(BOOL, -1))) {
                sp -= move(BOOL, 1);
                ip = *(word*)(p + ip + 1);
            }
            else {
                sp -= move(BOOL, 1);
                ip += 1 + move(ADDR, 1);
            }
        }
        else if (i == PLACE_INT) {
            word value = *(word*)(p + ip + 1);
            *(word*)(stack + sp) = value;
            sp += move(INT, 1);
            ip += move(INT, 1) + 1;
        }
        else if (i == INT_ADD) {
            word value = (*(word*)(stack + sp + move(INT, -1))) + (*(word*)(stack + sp + move(INT, -2)));
            *(word*)(stack + sp + move(INT, -2)) = value;
            sp -= move(INT, 1);
            ip++;
        }
        else if (i == INT_MUL) {
            word value = (*(word*)(stack + sp + move(INT, -1))) * (*(word*)(stack + sp + move(INT, -2)));
            *(word*)(stack + sp + move(INT, -2)) = value;
            sp -= move(INT, 1);
            ip++;
        }
        else if (i == INT_SUB) {
            word value = (*(word*)(stack + sp + move(INT, -1))) - (*(word*)(stack + sp + move(INT, -2)));
            *(word*)(stack + sp + move(INT, -2)) = value;
            sp -= move(INT, 1);
            ip++;
        }
        else if (i == INT_EQ) {
            byte eq = (*(word*)(stack + sp + move(INT, -1))) == (*(word*)(stack + sp + move(INT, -2)));
            sp -= move(INT, 2);
            *(byte*)(stack + sp) = eq; 
            sp += move(BOOL, 1);
            ip++;
        }
        else if (i == INT_LT) {
            byte lt = (*(word*)(stack + sp + move(INT, -1))) < (*(word*)(stack + sp + move(INT, -2)));
            sp -= move(INT, 2);
            *(byte*)(stack + sp) = lt; 
            sp += move(BOOL, 1);
            ip++;
        }
        else if (i == CLONE_FULL) {
            word value = *(word*)(stack + sp + move(INT, -1));
            *(word*)(stack + sp) = value;
            sp += move(INT, 1);
            ip++;
        }
        else if (i == CLONE_HALF) {
            printf("CLONE_HALF not implemented!");
            return -1;
        }
        else if (i == CLONE_SHORT) {
            printf("CLONE_SHORT not implemented!");
            return -1;
        }
        else if (i == CLONE_BYTE) {
            byte value = *(byte*)(stack + sp + move(BOOL, -1));
            *(byte*)(stack + sp) = value;
            sp += move(BOOL, 1);
            ip++;
        }
        else if (i == FETCH_INT) {
            word* target = *(word**)(stack + sp + move(INT, -1));
            follow_trail(&target, stack, sp);
            if (type_index(target) != INT) { printf("Failure: Type mismatch\n"); return -1; }

            *(word*)(stack + sp + move(INT, -1)) = *((word*)payload(target));
            ip++;
        }
        else if (i == DECLARE_INT) {
            byte* alloc = malloc(9);
            *alloc = INT;
            *(word*)(stack + sp) = (word)((word*)alloc);
            sp += move(ADDR, 1);
            ip++;
        }
        else if (i == ASSIGN_INT) {
            word* target = *(word**)(stack + sp + move(ADDR, -1) + move(INT, -1));
            follow_trail(&target, stack, sp);
            if (is_locked(target) == true) { printf("Failure: Can't assign to a locked variable!\n"); return -1; }
            if (type_index(target) != INT) { printf("Failure: Type mismatch!\n"); return -1; }
            word* pl = (word*)payload(target); 

            word value = *(word*)(stack + sp + move(INT, -1));
            *pl = value;
            sp -= move(ADDR, 1) + move (INT, 1);
            ip++;
        }
        else if (i == PLACE_BOOL) {
            word value = *(byte*)(p + ip + 1);
            *(byte*)(stack + sp) = value;
            sp += move(BOOL, 1);
            ip += move(BOOL, 1) + 1;
        }
        else if (i == DECLARE_BOOL) {
            byte* alloc = malloc(2);
            *alloc = BOOL;
            *(word*)(stack + sp) = (word)((word*)alloc);
            sp += move(ADDR, 1);
            ip++;
        }
        else if (i == FETCH_BOOL) {
            word* target = *(word**)(stack + sp + move(ADDR, -1));
            follow_trail(&target, stack, sp);
            if (type_index(target) != BOOL) { printf("Failure: Type mismatch\n"); return -1; }

            *(byte*)(stack + sp + move(ADDR, -1)) = *((byte*)payload(target));
            sp -= move(BOOL, 7);
            ip++;
        }
        else if (i == ASSIGN_BOOL) {
            word* target = *(word**)(stack + sp + move(ADDR, -1) + move(BOOL, -1));
            follow_trail(&target, stack, sp);
            if (is_locked(target) == true) { printf("Failure: Can't assign to a locked variable!\n"); return -1; }
            if (type_index(target) != BOOL) { printf("Failure: Type mismatch!\n"); return -1; }
            byte* pl = (byte*)payload(target); 

            byte value = *(byte*)(stack + sp + move(BOOL, -1));
            *pl = value;
            sp -= move(ADDR, 1) + move (BOOL, 1);
            ip++;
        }
        else if (i == BOOL_EQ) {
            byte eq = !!(*(stack + sp + move(BOOL, -1))) == !!(*(stack + sp + move(BOOL, -2)));
            *(byte*)(stack + sp + move(BOOL, -2)) = eq;
            sp -= move(BOOL, 1);
            ip++;
        }
        else if (i == BOOL_NOT) {
            *(byte*)(stack + sp + move(BOOL, -1)) = !(*(stack + sp + move(BOOL, -1)));
            ip++;
        }
        else if (i == BOOL_AND) {
            byte res = (*(stack + sp + move(BOOL, -1))) && (*(stack + sp + move(BOOL, -2)));
            *(byte*)(stack + sp + move(BOOL, -2)) = res;
            sp -= move(BOOL, 1);
            ip++;
        }
        else if (i == BOOL_OR) {
            byte res = (*(stack + sp + move(BOOL, -1))) || (*(stack + sp + move(BOOL, -2)));
            *(byte*)(stack + sp + move(BOOL, -2)) = res;
            sp -= move(BOOL, 1);
            ip++;
        }
        else if (i == GETBP) {
            *(word*)(stack + sp) = (word)bp;
            sp += move(INT, 1);
            ip++;
        }
        else if (i == GETSP) {
            *(word*)(stack + sp) = (word)sp;
            sp += move(INT, 1);
            ip++;
        }
        else if (i == MODSP) {
            word amount = *(word*)(p+ip+1);
            sp += amount;
            ip += move(INT, 1) + 1;
        }
        else if (i == FETCH_ADDR) {
            word* target = *(word**)(stack + sp + move(ADDR, -1));
            follow_trail(&target, stack, sp);
            *(word*)(stack + sp + move(ADDR, -1)) = (word)target;
            ip++;
        }
        else if (i == FREE_VAR) {
            word* target = *(word**)(stack + sp + move(ADDR, -1));
            try_free(target, stack, sp);
            sp -= move(ADDR, 1);
            ip++;
        }
        else if (i == FREE_VARS) {
            word count = *(word*)(p+ip+1);
            while (count > 0) {
                word* target = *(word**)(stack + sp + move(ADDR, -1));
                try_free(target, stack, sp);
                sp -= move(ADDR, 1);
                count--;
            }
            ip += move(INT, 1) + 1;
        }
        else if (i == PRINT_VAR) {
            word* target = *(word**)(stack + sp + move(ADDR, -1));
            follow_trail(&target, stack, sp);
            print_var(target);
            sp -= move(ADDR, 1);
            ip++;
        }
        else if (i == PRINT_INT) {
            word value = *(word*)(stack + sp + move(INT, -1));
            printf("%lld\n", value);
            sp -= move(INT, 1);
            ip++;
        }
        else if (i == PRINT_BOOL) {
            word value = *(byte*)(stack + sp + move(BOOL, -1));
            if (value) printf("true\n");
            else printf("false\n");
            sp -= move(BOOL, 1);
            ip++;
        }
        else if (i == STACK_FETCH) {
            word offset = *(word*)(p + ip + 1);
            word* value = *(word**)(stack + move(ADDR, offset));
            if (!is_on_stack(value, stack, sp)) value = (word*)(stack + move(ADDR, offset));
            *(word*)(stack + sp) = (word)value;
            sp += move(ADDR, 1);
            ip += move(INT, 1) + 1;
        }
        else if (i == BP_FETCH) {
            word offset = *(word*)(p + ip + 1);
            word* value = *(word**)(stack + bp + move(ADDR, offset));
            if (!is_on_stack(value, stack, sp)) value = (word*)(stack + bp + move(ADDR, offset));
            *(word*)(stack + sp) = (word)value;
            sp += move(ADDR, 1);
            ip += move(INT, 1) + 1;
        }
        else if (i == LOCK) {
            word* target = *(word**)(stack + sp + move(ADDR, -1));
            follow_trail(&target, stack, sp);
            byte* header = (byte*)target;
            *header = *header | 0b10000000;
            sp -= move(ADDR, 1);
            ip++;
        }
        else if (i == UNLOCK) {
            word* target = *(word**)(stack + sp + move(ADDR, -1));
            follow_trail(&target, stack, sp);
            byte* header = (byte*)target;
            *header = *header & 0b01111111;
            sp -= move(ADDR, 1);
            ip++;
        }
        else {
            printf("Failure: Unknown instruction: %x\n", p[ip]);
            return -1;
        }

        if (debug) { print_stack(stack, bp, sp); printf("\n\n"); }
    }
}

// Call should be: 'inex <command> <file>.ixc arg1? arg2? ...'
int main(int argc, char** argv) {
    char* command = argv[1];
    byte* file;
    word file_len;
    load_file(argv[2], &file, &file_len);
    if (file == NULL) { printf("Failure: No such file: %s\n", argv[2]); return -1;} 

    byte debug = false;

    switch (command_index(command)) {
        case I:
            print_entry_points(file);
            return 0;
        case DISASS:
            dissas(file, file_len);
            return 0;
        case DEBUG:
            debug = true;
        case RUN: {
            if (argc < 4) { printf("Failure: Too few arguments were given.\n"); return -1; }


            //Find the requested entry point
            word* entry_point_info = find_entry_point(file, argv[3]);
            if (entry_point_info == 0) { printf("Failure: No such entry point: %s\n", argv[3]); return -1; }


            // Gather entry point information
            word entry_addr = entry_point_info[0];
            char* argument_info = (char*)(entry_point_info+1);
            char argument_count = argument_info[0];
            if (argument_count+4 != argc) { printf("Failure: Argument mismatch.\n"); return -1; }

            byte stack[STACKSIZE];

            // Load global variables
            byte* progresser = file;
            find_global_vars_start(progresser, &progresser);

            byte* glob_var_ptr = progresser;
            word glob_var_count = *((word*)glob_var_ptr);
            glob_var_ptr += 8;
            int i = 0;
            while (i < glob_var_count) {
                byte type = type_index((word*)glob_var_ptr);
                byte* alloc = allocate(type, is_locked((word*)glob_var_ptr));
                glob_var_ptr += 1;
                switch (type){
                    case BOOL:
                        *(alloc+1) = *glob_var_ptr;
                        glob_var_ptr += move(BOOL, 1);
                        break;
                    case INT:
                        *((word*)(alloc+1)) = *(word*)glob_var_ptr;
                        glob_var_ptr += move(INT, 1);
                        break;
                    default:
                        break;
                }
                *(word*)(stack + move(ADDR, i)) = (word)alloc;
                i++;
            }

            find_instruction_start(progresser, &progresser);

            // Load given arguments
            word* arguments[argument_count];
            i = 0;
            while (i < argument_count) {
                switch (argument_info[i+1]) {
                    case BOOL: ;
                        byte bool_v = parse_bool(argv[4+i]);
                        if (bool_v == -1) { printf("Failure: expected a bool, but got: %s\n", argv[4+i]); return -1; }
                        byte* bool_alloc = allocate(BOOL, false);
                        *(bool_alloc+1) = bool_v;
                        *(word*)(stack + move(ADDR, glob_var_count) + move(ADDR, i)) = (word)bool_alloc;
                        break;
                    case INT: ;
                        word int_v = parse_int(argv[4+i]);
                        if (int_v == 0 && !(strcmp(argv[4+i], "0") == 0)) { printf("Failure: expected an int, but got: %s\n", argv[4+i]); return -1; }
                        byte* int_alloc = allocate(INT, false);
                        *((word*)(int_alloc+1)) = int_v;
                        *(word*)(stack + move(ADDR, glob_var_count) + move(ADDR, i)) = (word)int_alloc;
                        break;
                    default: ;
                        printf("Failure: Unknown type");
                        return -1;
                }
                i++;
            }

            if (debug) 
                printf("Running %s @ instruction #%lld...\n\n", argv[3], entry_addr);

            return run(progresser, entry_addr, stack, glob_var_count, argument_count, debug);
        }
        default:
            break;
    }
}