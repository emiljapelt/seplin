#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "ISA.h"
#include "types.h"
#include "defs.h"
#include "disass.h"
#include "file_analysis.h"
#include "commands.h"

#define MOVE(unit, steps) (TYPE_SIZE(unit) * (steps))

#define STACKSIZE 80000
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

byte* allocate(byte type) {
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
    *alloc = header;
    return alloc;
}

void print_var(word* target) {
    switch (TYPE(target)) {
        case BOOL:
            if (*((byte*)PAYLOAD(target))) printf("true\n");
            else printf("false\n");
            break;
        case INT:
            printf("%lld\n", *((word*)PAYLOAD(target)));
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

int run(byte* p, word entry_point, byte stack[], int glob_var_count, int argument_count, byte trace, byte time) {
    uword ip = entry_point;
    uword sp = MOVE(ADDR, glob_var_count+argument_count);
    uword bp = MOVE(ADDR, glob_var_count);

    if (trace) { print_stack(stack, bp, sp); printf("\n"); }

    while(true) {
        byte i = p[ip];

        if (sp < bp) { printf("Failure: stack underflow!\n"); return -1; }
        if (sp > STACKSIZE) { printf("Failure: stack overflow!\n"); return -1; }

        if (trace) printf("instruction #%llu: 0x%x %s\n", ip, i, instruction_to_string(i));

        switch (i)
        {
            case HALT: {
                if (trace) printf("Halting...\n");
                return 0;
            }
            case STOP: {
                if (bp == MOVE(ADDR, glob_var_count)) {
                    if (trace) printf("Halting...\n");
                    return 0;
                }

                uword i = sp - MOVE(ADDR, 1);
                while (i >= bp) {
                    try_free(*((word**)(stack+i)), stack, sp);
                    i -= MOVE(ADDR, 1);
                }

                uword old_bp = *(word*)(stack + bp + MOVE(ADDR, -1));
                uword next_ip = *(word*)(stack + bp + MOVE(ADDR, -2));;
                sp = bp - MOVE(ADDR, 2);

                bp = old_bp;
                ip = next_ip;
                break;
            }
            case CALL: {
                word arg_count = *(word*)(stack + sp + MOVE(INT, -1));
                sp += MOVE(INT, 1);

                for(int i = 0; i < arg_count; i++) {
                    word* arg = *(word**)(stack + sp + MOVE(ADDR, -3-i));
                    *(word*)(stack + sp + MOVE(ADDR, -1-i)) = (word)(arg);
                }

                *(word*)((stack + sp) - MOVE(ADDR, arg_count+2)) = ip+9;
                *(word*)((stack + sp) - MOVE(ADDR, arg_count+1)) = (word)bp;

                bp = sp - MOVE(ADDR, arg_count);
                ip = *(word*)(p+ip+1);
                break;
            }
            case GOTO: {
                word target = *(word*)(p + ip + 1);
                ip = target;
                break;
            }
            case IF_TRUE: {
                if (*(byte*)(stack + sp + MOVE(BOOL, -1))) {
                    sp -= MOVE(BOOL, 1);
                    ip = *(word*)(p + ip + 1);
                }
                else {
                    sp -= MOVE(BOOL, 1);
                    ip += 1 + MOVE(ADDR, 1);
                }
                break;
            }
            case PLACE_INT: {
                word value = *(word*)(p + ip + 1);
                *(word*)(stack + sp) = value;
                sp += MOVE(INT, 1);
                ip += MOVE(INT, 1) + 1;
                break;
            }
            case INT_ADD: {
                word value = (*(word*)(stack + sp + MOVE(INT, -1))) + (*(word*)(stack + sp + MOVE(INT, -2)));
                *(word*)(stack + sp + MOVE(INT, -2)) = value;
                sp -= MOVE(INT, 1);
                ip++;
                break;
            }
            case INT_MUL: {
                word value = (*(word*)(stack + sp + MOVE(INT, -1))) * (*(word*)(stack + sp + MOVE(INT, -2)));
                *(word*)(stack + sp + MOVE(INT, -2)) = value;
                sp -= MOVE(INT, 1);
                ip++;
                break;
            }
            case INT_SUB: {
                word value = (*(word*)(stack + sp + MOVE(INT, -1))) - (*(word*)(stack + sp + MOVE(INT, -2)));
                *(word*)(stack + sp + MOVE(INT, -2)) = value;
                sp -= MOVE(INT, 1);
                ip++;
                break;
            }
            case INT_EQ: {
                byte eq = (*(word*)(stack + sp + MOVE(INT, -1))) == (*(word*)(stack + sp + MOVE(INT, -2)));
                sp -= MOVE(INT, 2);
                *(byte*)(stack + sp) = eq; 
                sp += MOVE(BOOL, 1);
                ip++;
                break;
            }
            case INT_LT: {
                byte lt = (*(word*)(stack + sp + MOVE(INT, -1))) < (*(word*)(stack + sp + MOVE(INT, -2)));
                sp -= MOVE(INT, 2);
                *(byte*)(stack + sp) = lt; 
                sp += MOVE(BOOL, 1);
                ip++;
                break;
            }
            case CLONE_FULL: {
                word value = *(word*)(stack + sp + MOVE(INT, -1));
                *(word*)(stack + sp) = value;
                sp += MOVE(INT, 1);
                ip++;
                break;
            }
            case CLONE_HALF: {
                printf("CLONE_HALF not implemented!");
                return -1;
            }
            case CLONE_SHORT: {
                printf("CLONE_SHORT not implemented!");
                return -1;
            }
            case CLONE_BYTE: {
                byte value = *(byte*)(stack + sp + MOVE(BOOL, -1));
                *(byte*)(stack + sp) = value;
                sp += MOVE(BOOL, 1);
                ip++;
                break;
            }
            case FETCH_INT: {
                word* target = *(word**)(stack + sp + MOVE(INT, -1));
                follow_trail(&target, stack, sp);
                if (TYPE(target) != INT) { printf("Failure: Type mismatch\n"); return -1; }

                *(word*)(stack + sp + MOVE(INT, -1)) = *((word*)PAYLOAD(target));
                ip++;
                break;
            }
            case DECLARE_INT: {
                byte* alloc = malloc(9);
                *alloc = INT;
                *(word*)(stack + sp) = (word)((word*)alloc);
                sp += MOVE(ADDR, 1);
                ip++;
                break;
            }
            case ASSIGN_INT: {
                word* target = *(word**)(stack + sp + MOVE(ADDR, -1) + MOVE(INT, -1));
                follow_trail(&target, stack, sp);
                if (TYPE(target) != INT) { printf("Failure: Type mismatch!\n"); return -1; }
                word* pl = (word*)PAYLOAD(target); 

                word value = *(word*)(stack + sp + MOVE(INT, -1));
                *pl = value;
                sp -= MOVE(ADDR, 1) + MOVE (INT, 1);
                ip++;
                break;
            }
            case PLACE_BOOL: {
                word value = *(byte*)(p + ip + 1);
                *(byte*)(stack + sp) = value;
                sp += MOVE(BOOL, 1);
                ip += MOVE(BOOL, 1) + 1;
                break;
            }
            case DECLARE_BOOL: {
                byte* alloc = malloc(2);
                *alloc = BOOL;
                *(word*)(stack + sp) = (word)((word*)alloc);
                sp += MOVE(ADDR, 1);
                ip++;
                break;
            }
            case FETCH_BOOL: {
                word* target = *(word**)(stack + sp + MOVE(ADDR, -1));
                follow_trail(&target, stack, sp);
                if (TYPE(target) != BOOL) { printf("Failure: Type mismatch\n"); return -1; }

                *(byte*)(stack + sp + MOVE(ADDR, -1)) = *((byte*)PAYLOAD(target));
                sp -= MOVE(BOOL, 7);
                ip++;
                break;
            }
            case ASSIGN_BOOL: {
                word* target = *(word**)(stack + sp + MOVE(ADDR, -1) + MOVE(BOOL, -1));
                follow_trail(&target, stack, sp);
                if (TYPE(target) != BOOL) { printf("Failure: Type mismatch!\n"); return -1; }
                byte* pl = (byte*)PAYLOAD(target); 

                byte value = *(byte*)(stack + sp + MOVE(BOOL, -1));
                *pl = value;
                sp -= MOVE(ADDR, 1) + MOVE (BOOL, 1);
                ip++;
                break;
            }
            case BOOL_EQ: {
                byte eq = !!(*(stack + sp + MOVE(BOOL, -1))) == !!(*(stack + sp + MOVE(BOOL, -2)));
                *(byte*)(stack + sp + MOVE(BOOL, -2)) = eq;
                sp -= MOVE(BOOL, 1);
                ip++;
                break;
            }
            case BOOL_NOT: {
                *(byte*)(stack + sp + MOVE(BOOL, -1)) = !(*(stack + sp + MOVE(BOOL, -1)));
                ip++;
                break;
            }
            case BOOL_AND: {
                byte res = (*(stack + sp + MOVE(BOOL, -1))) && (*(stack + sp + MOVE(BOOL, -2)));
                *(byte*)(stack + sp + MOVE(BOOL, -2)) = res;
                sp -= MOVE(BOOL, 1);
                ip++;
                break;
            }
            case BOOL_OR: {
                byte res = (*(stack + sp + MOVE(BOOL, -1))) || (*(stack + sp + MOVE(BOOL, -2)));
                *(byte*)(stack + sp + MOVE(BOOL, -2)) = res;
                sp -= MOVE(BOOL, 1);
                ip++;
                break;
            }
            case GETBP: {
                *(word*)(stack + sp) = (word)bp;
                sp += MOVE(INT, 1);
                ip++;
                break;
            }
            case GETSP: {
                *(word*)(stack + sp) = (word)sp;
                sp += MOVE(INT, 1);
                ip++;
                break;
            }
            case MODSP: {
                word amount = *(word*)(p+ip+1);
                sp += amount;
                ip += MOVE(INT, 1) + 1;
                break;
            }
            case FREE_VAR: {
                word* target = *(word**)(stack + sp + MOVE(ADDR, -1));
                try_free(target, stack, sp);
                sp -= MOVE(ADDR, 1);
                ip++;
                break;
            }
            case FREE_VARS: {
                word count = *(word*)(p+ip+1);
                while (count > 0) {
                    word* target = *(word**)(stack + sp + MOVE(ADDR, -1));
                    try_free(target, stack, sp);
                    sp -= MOVE(ADDR, 1);
                    count--;
                }
                ip += MOVE(INT, 1) + 1;
                break;
            }
            case PRINT_VAR: {
                word* target = *(word**)(stack + sp + MOVE(ADDR, -1));
                follow_trail(&target, stack, sp);
                print_var(target);
                sp -= MOVE(ADDR, 1);
                ip++;
                break;
            }
            case PRINT_INT: {
                word value = *(word*)(stack + sp + MOVE(INT, -1));
                printf("%lld\n", value);
                sp -= MOVE(INT, 1);
                ip++;
                break;
            }
            case PRINT_BOOL: {
                word value = *(byte*)(stack + sp + MOVE(BOOL, -1));
                if (value) printf("true\n");
                else printf("false\n");
                sp -= MOVE(BOOL, 1);
                ip++;
                break;
            }
            case STACK_FETCH: {
                word offset = *(word*)(p + ip + 1);
                word* value = *(word**)(stack + MOVE(ADDR, offset));
                if (!is_on_stack(value, stack, sp)) value = (word*)(stack + MOVE(ADDR, offset));
                *(word*)(stack + sp) = (word)value;
                sp += MOVE(ADDR, 1);
                ip += MOVE(INT, 1) + 1;
                break;
            }
            case BP_FETCH: {
                word offset = *(word*)(p + ip + 1);
                word* value = *(word**)(stack + bp + MOVE(ADDR, offset));
                if (!is_on_stack(value, stack, sp)) value = (word*)(stack + bp + MOVE(ADDR, offset));
                *(word*)(stack + sp) = (word)value;
                sp += MOVE(ADDR, 1);
                ip += MOVE(INT, 1) + 1;
                break;
            }
            default: {
                printf("Failure: Unknown instruction: %x\n", p[ip]);
                return -1;
            }
        }

        if (trace) { print_stack(stack, bp, sp); printf("\n"); }
    }
}

// Call should be: 'inex <command> <file>.ixc arg1? arg2? ...'
int main(int argc, char** argv) {
    char* command = argv[1];
    byte* file;
    word file_len;
    load_file(argv[2], &file, &file_len);
    if (file == NULL) { printf("Failure: No such file: %s\n", argv[2]); return -1;} 

    switch (command_index(command)) {
        case I:
            print_entry_points(file);
            return 0;
        case DISASS:
            dissas(file, file_len);
            return 0;
        case RUN: {

            byte trace = false;
            byte time = false;
            int flags = 0;
            for(int i = 3; i < argc; i++) {
                switch (flag_index(argv[i])) {
                    case TRACE:
                        trace = true;
                        flags++;
                        break;
                    case TIME:
                        time = true;
                        flags++;
                        break;
                    case UNKNOWN_FLAG:
                        flags++;
                        break;
                    default: break;
                }
            }

            //Find the requested entry point
            word* entry_point_info = find_entry_point(file, argv[3+flags]);
            if (entry_point_info == 0) { printf("Failure: No such entry point: %s\n", argv[3+flags]); return -1; }


            // Gather entry point information
            word entry_addr = entry_point_info[0];
            char* argument_info = (char*)(entry_point_info+1);
            char argument_count = argument_info[0];
            if (argument_count+4 != argc-flags) { printf("Failure: Argument mismatch.\n"); return -1; }

            byte stack[STACKSIZE];

            // Load global variables
            byte* progresser = file;
            find_global_vars_start(progresser, &progresser);

            byte* glob_var_ptr = progresser;
            word glob_var_count = *((word*)glob_var_ptr);
            glob_var_ptr += 8;
            int i = 0;
            while (i < glob_var_count) {
                byte type = *glob_var_ptr;
                byte* alloc = allocate(type);
                glob_var_ptr += 1;
                switch (type){
                    case BOOL:
                        *(alloc+1) = *glob_var_ptr;
                        glob_var_ptr += MOVE(BOOL, 1);
                        break;
                    case INT:
                        *((word*)(alloc+1)) = *(word*)glob_var_ptr;
                        glob_var_ptr += MOVE(INT, 1);
                        break;
                    default:
                        break;
                }
                *(word*)(stack + MOVE(ADDR, i)) = (word)alloc;
                i++;
            }

            find_instruction_start(progresser, &progresser);

            // Load given arguments
            word* arguments[argument_count];
            i = 0;
            while (i < argument_count) {
                switch (argument_info[i+1]) {
                    case BOOL: ;
                        byte bool_v = parse_bool(argv[4+flags+i]);
                        if (bool_v == -1) { printf("Failure: expected a bool, but got: %s\n", argv[4+flags+i]); return -1; }
                        byte* bool_alloc = allocate(BOOL);
                        *(bool_alloc+1) = bool_v;
                        *(word*)(stack + MOVE(ADDR, glob_var_count) + MOVE(ADDR, i)) = (word)bool_alloc;
                        break;
                    case INT: ;
                        word int_v = parse_int(argv[4+flags+i]);
                        if (int_v == 0 && !(strcmp(argv[4+flags+i], "0") == 0)) { printf("Failure: expected an int, but got: %s\n", argv[4+flags+i]); return -1; }
                        byte* int_alloc = allocate(INT);
                        *((word*)(int_alloc+1)) = int_v;
                        *(word*)(stack + MOVE(ADDR, glob_var_count) + MOVE(ADDR, i)) = (word)int_alloc;
                        break;
                    default: ;
                        printf("Failure: Unknown type");
                        return -1;
                }
                i++;
            }

            clock_t ticks;
            if (time) ticks = clock();
            if (trace) 
                printf("Running %s @ instruction #%lld...\n\n", argv[3+flags], entry_addr);

            int return_code = run(progresser, entry_addr, stack, glob_var_count, argument_count, trace, time);

            if (time) 
                printf("Total execution time: %fs\n", (((double)(clock() - ticks))/CLOCKS_PER_SEC));
                
            return return_code;
        }
        default:
            break;
    }
}