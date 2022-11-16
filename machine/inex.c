#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "ISA.h"
#include "types.h"
#include "defs.h"
#include "disass.h"
#include "memory.h"
#include "file_analysis.h"
#include "commands.h"

#define MOVE(unit, steps) (SIZE(unit) * (steps))

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

// void print_var(word* target) {
//     switch (TYPE(target)) {
//         case BYTE:
//             if (*((byte*)PAYLOAD(target))) printf("true\n");
//             else printf("false\n");
//             break;
//         case FULL:
//             printf("%lld\n", *((word*)PAYLOAD(target)));
//             break;
//         default:
//             printf("?\n");
//             break;
//     }
// }

// void follow_trail(word** target, byte* stack, uword sp) {
//     word* temp = *target;
//     while(on_stack(temp, sp)) temp = (word*)*temp;
//     *target = temp;
// }

int run(byte* p, word entry_point, byte stack[], int glob_var_count, int argument_count, byte trace, byte time) {
    uword ip = entry_point;
    uword sp = MOVE(FULL, glob_var_count+argument_count);
    uword bp = MOVE(FULL, glob_var_count);

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
                if (bp == MOVE(FULL, glob_var_count)) {
                    if (trace) printf("Halting...\n");
                    return 0;
                }

                uword i = sp - MOVE(FULL, 1);
                while (i >= bp) {
                    try_free(*((word**)(stack+i)));
                    i -= MOVE(FULL, 1);
                }

                uword old_bp = *(word*)(stack + bp + MOVE(FULL, -1));
                uword next_ip = *(word*)(stack + bp + MOVE(FULL, -2));;
                sp = bp - MOVE(FULL, 2);

                bp = old_bp;
                ip = next_ip;
                break;
            }
            case CALL: {
                word arg_count = *(word*)(stack + sp + MOVE(FULL, -1));
                sp += MOVE(FULL, 1);

                for(int i = 0; i < arg_count; i++) {
                    word* arg = *(word**)(stack + sp + MOVE(FULL, -3-i));
                    INCR_REF_COUNT(arg);
                    *(word*)(stack + sp + MOVE(FULL, -1-i)) = (word)(arg);
                }

                *(word*)((stack + sp) - MOVE(FULL, arg_count+2)) = ip+9;
                *(word*)((stack + sp) - MOVE(FULL, arg_count+1)) = (word)bp;

                bp = sp - MOVE(FULL, arg_count);
                ip = *(word*)(p+ip+1);
                break;
            }
            case GOTO: {
                word target = *(word*)(p + ip + 1);
                ip = target;
                break;
            }
            case IF_TRUE: {
                if (*(byte*)(stack + sp + MOVE(BYTE, -1))) {
                    sp -= MOVE(BYTE, 1);
                    ip = *(word*)(p + ip + 1);
                }
                else {
                    sp -= MOVE(BYTE, 1);
                    ip += 1 + MOVE(FULL, 1);
                }
                break;
            }
            case PLACE_BOOL: {
                word value = *(byte*)(p + ip + 1);
                *(byte*)(stack + sp) = value;
                sp += MOVE(BYTE, 1);
                ip += MOVE(BYTE, 1) + 1;
                break;
            }
            case PLACE_INT: {
                word value = *(word*)(p + ip + 1);
                *(word*)(stack + sp) = value;
                sp += MOVE(FULL, 1);
                ip += MOVE(FULL, 1) + 1;
                break;
            }
            case CLONE_FULL: {
                word value = *(word*)(stack + sp + MOVE(FULL, -1));
                *(word*)(stack + sp) = value;
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case CLONE_HALF: {
                byte value = *(byte*)(stack + sp + MOVE(HALF, -1));
                *(byte*)(stack + sp) = value;
                sp += MOVE(HALF, 1);
                ip++;
                break;
            }
            case CLONE_SHORT: {
                byte value = *(byte*)(stack + sp + MOVE(SHORT, -1));
                *(byte*)(stack + sp) = value;
                sp += MOVE(SHORT, 1);
                ip++;
                break;
            }
            case CLONE_BYTE: {
                byte value = *(byte*)(stack + sp + MOVE(BYTE, -1));
                *(byte*)(stack + sp) = value;
                sp += MOVE(BYTE, 1);
                ip++;
                break;
            }
            case FETCH_FULL: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1));
                *(word*)(stack + sp + MOVE(FULL, -1)) = *((word*)target);
                ip++;
                break;
            }
            case FETCH_HALF: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1));
                *(byte*)(stack + sp + MOVE(FULL, -1)) = *((short*)target);
                sp -= MOVE(HALF, 1);
                ip++;
                break;
            }
            case FETCH_SHORT: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1));
                *(byte*)(stack + sp + MOVE(FULL, -1)) = *((short*)target);
                sp -= MOVE(SHORT, 3);
                ip++;
                break;
            }
            case FETCH_BYTE: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1));
                *(byte*)(stack + sp + MOVE(FULL, -1)) = *((byte*)target);
                sp -= MOVE(BYTE, 7);
                ip++;
                break;
            }
            case FIELD_FETCH: {
                uword offset = *(uword*)(stack + sp + MOVE(FULL, -1));
                word** target = *((word***)(stack + sp + MOVE(FULL, -2)));

                if (!IS_STRUCT(target)) { printf("Failure: Field fetch from non-struct data\n"); return -1; } 
                if (ALLOC_SIZE(target) <= offset) { printf("Failure: Field fetch out of struct bounds\n"); return -1; } 

                *(word**)(stack + sp + MOVE(FULL, -2)) = *(target + offset);
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_FULL: {
                byte* alloc = allocate_simple(FULL);
                *(word*)(stack + sp) = (word)((word*)alloc);
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_HALF: {
                byte* alloc = allocate_simple(HALF);
                *(word*)(stack + sp) = (word)((word*)alloc);
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_SHORT: {
                byte* alloc = allocate_simple(SHORT);
                *(word*)(stack + sp) = (word)((word*)alloc);
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_BYTE: {
                byte* alloc = allocate_simple(BYTE);
                *(word*)(stack + sp) = (word)((word*)alloc);
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_STRUCT: {
                uword fields = *(uword*)(stack + sp + MOVE(FULL, -1));
                byte* alloc = allocate_struct(fields);
                *(word*)(stack + sp + MOVE(FULL, -1)) = (word)((word*)alloc);
                ip++;
                break;
            }
            case ASSIGN_FULL: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1) + MOVE(FULL, -1));
                word value = *(word*)(stack + sp + MOVE(FULL, -1));
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (FULL, 1);
                ip++;
                break;
            }
            case ASSIGN_HALF: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1) + MOVE(HALF, -1));
                byte value = *(byte*)(stack + sp + MOVE(HALF, -1));
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (HALF, 1);
                ip++;
                break;
            }
            case ASSIGN_SHORT: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1) + MOVE(SHORT, -1));
                byte value = *(byte*)(stack + sp + MOVE(SHORT, -1));
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (SHORT, 1);
                ip++;
                break;
            }
            case ASSIGN_BYTE: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1) + MOVE(BYTE, -1));
                byte value = *(byte*)(stack + sp + MOVE(BYTE, -1));
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (BYTE, 1);
                ip++;
                break;
            }
            case FIELD_ASSIGN: {
                uword** target = *(uword***)(stack + sp + MOVE(FULL, -3));
                uword offset = *(uword*)(stack + sp + MOVE(FULL, -2));
                word* value = *(word**)(stack + sp + MOVE(FULL, -1));

                if (!IS_STRUCT(target)) { printf("Failure: Field assignment to non-struct data\n"); return -1; } 
                if (ALLOC_SIZE(target) <= offset) { printf("Failure: Field assignment out of struct bounds\n"); return -1; } 

                try_free(*(target + offset));

                INCR_REF_COUNT(value);
                *(target + offset) = value;
                sp -= MOVE(FULL, 3);
                ip++;
                break;
            }
            case INT_ADD: {
                word value = (*(word*)(stack + sp + MOVE(FULL, -1))) + (*(word*)(stack + sp + MOVE(FULL, -2)));
                *(word*)(stack + sp + MOVE(FULL, -2)) = value;
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case INT_MUL: {
                word value = (*(word*)(stack + sp + MOVE(FULL, -1))) * (*(word*)(stack + sp + MOVE(FULL, -2)));
                *(word*)(stack + sp + MOVE(FULL, -2)) = value;
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case INT_SUB: {
                word value = (*(word*)(stack + sp + MOVE(FULL, -1))) - (*(word*)(stack + sp + MOVE(FULL, -2)));
                *(word*)(stack + sp + MOVE(FULL, -2)) = value;
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case INT_EQ: {
                byte eq = (*(word*)(stack + sp + MOVE(FULL, -1))) == (*(word*)(stack + sp + MOVE(FULL, -2)));
                sp -= MOVE(FULL, 2);
                *(byte*)(stack + sp) = eq; 
                sp += MOVE(BYTE, 1);
                ip++;
                break;
            }
            case INT_LT: {
                byte lt = (*(word*)(stack + sp + MOVE(FULL, -1))) < (*(word*)(stack + sp + MOVE(FULL, -2)));
                sp -= MOVE(FULL, 2);
                *(byte*)(stack + sp) = lt; 
                sp += MOVE(BYTE, 1);
                ip++;
                break;
            }
            case BOOL_EQ: {
                byte eq = !!(*(stack + sp + MOVE(BYTE, -1))) == !!(*(stack + sp + MOVE(BYTE, -2)));
                *(byte*)(stack + sp + MOVE(BYTE, -2)) = eq;
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case BOOL_NOT: {
                *(byte*)(stack + sp + MOVE(BYTE, -1)) = !(*(stack + sp + MOVE(BYTE, -1)));
                ip++;
                break;
            }
            case BOOL_AND: {
                byte res = (*(stack + sp + MOVE(BYTE, -1))) && (*(stack + sp + MOVE(BYTE, -2)));
                *(byte*)(stack + sp + MOVE(BYTE, -2)) = res;
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case BOOL_OR: {
                byte res = (*(stack + sp + MOVE(BYTE, -1))) || (*(stack + sp + MOVE(BYTE, -2)));
                *(byte*)(stack + sp + MOVE(BYTE, -2)) = res;
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case GETBP: {
                *(word*)(stack + sp) = (word)bp;
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case GETSP: {
                *(word*)(stack + sp) = (word)sp;
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case MODSP: {
                word amount = *(word*)(p+ip+1);
                sp += amount;
                ip += MOVE(FULL, 1) + 1;
                break;
            }
            case FREE_VAR: {
                word* target = *(word**)(stack + sp + MOVE(FULL, -1));
                try_free(target);
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case FREE_VARS: {
                word count = *(word*)(p+ip+1);
                while (count > 0) {
                    word* target = *(word**)(stack + sp + MOVE(FULL, -1));
                    try_free(target);
                    sp -= MOVE(FULL, 1);
                    count--;
                }
                ip += MOVE(FULL, 1) + 1;
                break;
            }
            // case PRINT_VAR: {
            //     word* target = *(word**)(stack + sp + MOVE(FULL, -1));
            //     print_var(target);
            //     sp -= MOVE(FULL, 1);
            //     ip++;
            //     break;
            // }
            case PRINT_INT: {
                word value = *(word*)(stack + sp + MOVE(FULL, -1));
                printf("%lld\n", value);
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case PRINT_BOOL: {
                word value = *(byte*)(stack + sp + MOVE(BYTE, -1));
                if (value) printf("true\n");
                else printf("false\n");
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case STACK_FETCH: {
                word offset = *(word*)(p + ip + 1);
                word* value = *(word**)(stack + MOVE(FULL, offset));
                *(word*)(stack + sp) = (word)value;
                sp += MOVE(FULL, 1);
                ip += MOVE(FULL, 1) + 1;
                break;
            }
            case BP_FETCH: {
                word offset = *(word*)(p + ip + 1);
                word* value = *(word**)(stack + bp + MOVE(FULL, offset));
                *(word*)(stack + sp) = (word)value;
                sp += MOVE(FULL, 1);
                ip += MOVE(FULL, 1) + 1;
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
                glob_var_ptr += 1;
                switch (type){
                    case BOOL: {
                        byte* alloc = allocate_simple(BYTE);
                        *(alloc+1) = *glob_var_ptr;
                        glob_var_ptr += MOVE(BYTE, 1);
                        *(word*)(stack + MOVE(FULL, i)) = (word)alloc;
                        break;
                    }
                    case INT: {
                        byte* alloc = allocate_simple(FULL);
                        *((word*)(alloc+1)) = *(word*)glob_var_ptr;
                        glob_var_ptr += MOVE(FULL, 1);
                        *(word*)(stack + MOVE(FULL, i)) = (word)alloc;
                        break;
                    }
                    default:
                        break;
                }
                i++;
            }

            find_instruction_start(progresser, &progresser);

            // Load given arguments
            word* arguments[argument_count];
            i = 0;
            while (i < argument_count) {
                switch (argument_info[i+1]) {
                    case BOOL: {
                        byte bool_v = parse_bool(argv[4+flags+i]);
                        if (bool_v == -1) { printf("Failure: expected a bool, but got: %s\n", argv[4+flags+i]); return -1; }
                        byte* bool_alloc = allocate_simple(BYTE);
                        *(bool_alloc) = bool_v;
                        *(word*)(stack + MOVE(FULL, glob_var_count) + MOVE(FULL, i)) = (word)bool_alloc;
                        break;
                    }
                    case INT: {
                        word int_v = parse_int(argv[4+flags+i]);
                        if (int_v == 0 && !(strcmp(argv[4+flags+i], "0") == 0)) { printf("Failure: expected an int, but got: %s\n", argv[4+flags+i]); return -1; }
                        byte* int_alloc = allocate_simple(FULL);
                        *((word*)(int_alloc)) = int_v;
                        *(word*)(stack + MOVE(FULL, glob_var_count) + MOVE(FULL, i)) = (word)int_alloc;
                        break;
                    }
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