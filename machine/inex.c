#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "ISA.h"
#include "types.h"
#include "disass.h"
#include "memory.h"
#include "file_analysis.h"
#include "commands.h"

#define MOVE(unit, steps) (SIZE(unit) * (steps))

#define STACKSIZE 80000

void load_file(char* file_name, byte_t** out_file, full_t* out_len) {
    FILE* fp;
    char* buffer;
    long numbytes;
    
    fp = fopen(file_name, "r");
    
    if (fp == NULL) { out_file = 0; return; }
    
    fseek(fp, 0L, SEEK_END);
    numbytes = ftell(fp);
    fseek(fp, 0L, SEEK_SET);	
    buffer = (byte_t*)calloc(numbytes, sizeof(char));	
    
    if(buffer == NULL) { out_file = 0; return; }
    
    fread(buffer, sizeof(char), numbytes, fp);
    fclose(fp);

    *out_file = buffer;
    *out_len = numbytes;
}

void print_stack(byte_t* stack, ufull_t bp, ufull_t sp) {
    printf("V %llx\n", (full_t)stack);
    printf("[");
    ufull_t i = 0;
    full_t to_top = sp - (sp % 8);
    while(i < to_top) {
        if (i == bp) printf("bp>");
        printf("0x%llx, ", *(full_t*)(stack + i));
        i+=8;
    }
    if (to_top != sp) {
        printf("| ");
        byte_t x = 0;
        while(x < sp % 8) {
            printf("0x%x, ", *(byte_t*)(stack + i + x));
            x++;
        }
    }
    printf("_ ]\n");
}

void print_help() {
    char message[1000];

    strcat(message, "Welcome to INEX!\n");
    strcat(message, "Here are the available commands:\n");
    strcat(message, "   help:\n");
    strcat(message, "       Prints this message.\n");
    strcat(message, "   i <file_path>:\n");
    strcat(message, "       Prints the interface of a compiled INEX file.\n");
    strcat(message, "   disass <file_path>:\n");
    strcat(message, "       Prints the instructions of a compiled INEX file, using their names.\n");
    strcat(message, "   run <file_path> <entry_point> <args>*:\n");
    strcat(message, "       Executes a compiled INEX file, starting from the given entry point, using the given arguments.\n");
    strcat(message, "       --time: Measure and print the execution time, in seconds.\n");
    strcat(message, "       --trace: Print information about the execution.\n");
    strcat(message, "           This includes which instruction is being executed, the current stack and memory management.\n");

    printf("%s", message);
}

void read_input(unsigned int max_size, char** ret) {
    char buffer[max_size + 1];
    char ch = 0;
    unsigned int count = 0;
    while(ch != '\n' && count < max_size) {
        ch = getchar();
        buffer[count++] = ch;
    } 
    buffer[count-1] = '\0';
    *ret = buffer;
}

int run(byte_t* p, full_t entry_point, byte_t stack[], byte_t* arguments[], int argument_count, byte_t trace, byte_t time) {    
    ufull_t ip = 0;
    ufull_t sp = 0;
    ufull_t bp = 0;
    ufull_t depth = 0;

    if (trace) { print_stack(stack, bp, sp); printf("\n"); }

    while(true) {
        byte_t i = p[ip];

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
                if (depth == 0) {
                    if (trace) printf("Halting...\n");
                    return 0;
                }

                ufull_t i = sp - MOVE(FULL, 1);
                while (i >= bp) {
                    try_free(*((full_t**)(stack+i)), sp, 0, trace);
                    i -= MOVE(FULL, 1);
                }

                depth--;
                ufull_t old_bp = *(full_t*)(stack + bp + MOVE(FULL, -1));
                ufull_t next_ip = *(full_t*)(stack + bp + MOVE(FULL, -2));;
                sp = bp - MOVE(FULL, 2);
                bp = old_bp;
                ip = next_ip;
                break;
            }
            case CALL: {
                full_t arg_count = *(full_t*)(stack + sp + MOVE(FULL, -1));
                sp += MOVE(FULL, 1);

                for(int i = 0; i < arg_count; i++) {
                    full_t* arg = *(full_t**)(stack + sp + MOVE(FULL, -3-i));
                    // to_origin(&arg, sp);
                    // if (*arg) INCR_REF_COUNT(*arg);
                    *(full_t*)(stack + sp + MOVE(FULL, -1-i)) = (full_t)(arg);
                }

                *(full_t*)((stack + sp) - MOVE(FULL, arg_count+2)) = ip+9;
                *(full_t*)((stack + sp) - MOVE(FULL, arg_count+1)) = (full_t)bp;

                depth++;
                bp = sp - MOVE(FULL, arg_count);
                ip = *(full_t*)(p+ip+1);
                break;
            }
            case GOTO: {
                full_t target = *(full_t*)(p + ip + 1);
                ip = target;
                break;
            }
            case IF_TRUE: {
                if (*(byte_t*)(stack + sp + MOVE(BYTE, -1))) {
                    sp -= MOVE(BYTE, 1);
                    ip = *(full_t*)(p + ip + 1);
                }
                else {
                    sp -= MOVE(BYTE, 1);
                    ip += 1 + MOVE(FULL, 1);
                }
                break;
            }
            case PLACE_BOOL: {
                full_t value = *(byte_t*)(p + ip + 1);
                *(byte_t*)(stack + sp) = value;
                sp += MOVE(BYTE, 1);
                ip += MOVE(BYTE, 1) + 1;
                break;
            }
            case PLACE_INT: {
                full_t value = *(full_t*)(p + ip + 1);
                *(full_t*)(stack + sp) = value;
                sp += MOVE(FULL, 1);
                ip += MOVE(FULL, 1) + 1;
                break;
            }
            case PLACE_CHAR: {
                full_t value = *(byte_t*)(p + ip + 1);
                *(byte_t*)(stack + sp) = value;
                sp += MOVE(BYTE, 1);
                ip += MOVE(BYTE, 1) + 1;
                break;
            }
            case CLONE_FULL: {
                full_t value = *(full_t*)(stack + sp + MOVE(FULL, -1));
                *(full_t*)(stack + sp) = value;
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case CLONE_HALF: {
                byte_t value = *(byte_t*)(stack + sp + MOVE(HALF, -1));
                *(byte_t*)(stack + sp) = value;
                sp += MOVE(HALF, 1);
                ip++;
                break;
            }
            case CLONE_SHORT: {
                byte_t value = *(byte_t*)(stack + sp + MOVE(SHORT, -1));
                *(byte_t*)(stack + sp) = value;
                sp += MOVE(SHORT, 1);
                ip++;
                break;
            }
            case CLONE_BYTE: {
                byte_t value = *(byte_t*)(stack + sp + MOVE(BYTE, -1));
                *(byte_t*)(stack + sp) = value;
                sp += MOVE(BYTE, 1);
                ip++;
                break;
            }
            case FETCH_FULL: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                *(full_t*)(stack + sp + MOVE(FULL, -1)) = *((full_t*)target);
                ip++;
                break;
            }
            case FETCH_HALF: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                *(half_t*)(stack + sp + MOVE(FULL, -1)) = *((half_t*)target);
                sp -= MOVE(HALF, 1);
                ip++;
                break;
            }
            case FETCH_SHORT: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                *(short_t*)(stack + sp + MOVE(FULL, -1)) = *((short_t*)target);
                sp -= MOVE(SHORT, 3);
                ip++;
                break;
            }
            case FETCH_BYTE: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                *(byte_t*)(stack + sp + MOVE(FULL, -1)) = *((byte_t*)target);
                sp -= MOVE(BYTE, 7);
                ip++;
                break;
            }
            case FIELD_FETCH: {
                ufull_t offset = *(ufull_t*)(stack + sp + MOVE(FULL, -1));
                full_t** target = *((full_t***)(stack + sp + MOVE(FULL, -2)));

                if (!IS_STRUCT(target)) { printf("Failure: Field fetch from non-struct data\n"); return -1; } 
                if (ALLOC_SIZE(target) <= offset) { printf("Failure: Field fetch out of struct bounds\n"); return -1; } 

                *(full_t***)(stack + sp + MOVE(FULL, -2)) = target + offset;
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case REF_FETCH: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                if (ON_STACK((byte_t*)target, sp)) to_origin(&target, sp);
                *(full_t**)(stack + sp + MOVE(FULL, -1)) = target;
                ip++;
                break;
            }
            case DECLARE_FULL: {
                byte_t* alloc = allocate_simple(FULL);
                *(full_t*)(stack + sp) = (full_t)((full_t*)alloc);
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_HALF: {
                byte_t* alloc = allocate_simple(HALF);
                *(full_t*)(stack + sp) = (full_t)((full_t*)alloc);
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_SHORT: {
                byte_t* alloc = allocate_simple(SHORT);
                *(full_t*)(stack + sp) = (full_t)((full_t*)alloc);
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_BYTE: {
                byte_t* alloc = allocate_simple(BYTE);
                *(full_t*)(stack + sp) = (full_t)((full_t*)alloc);
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            case DECLARE_STRUCT: {
                ufull_t fields = *(ufull_t*)(stack + sp + MOVE(FULL, -1));
                byte_t* alloc = allocate_struct(fields);
                *(full_t*)(stack + sp + MOVE(FULL, -1)) = (full_t)((full_t*)alloc);
                ip++;
                break;
            }
            case ASSIGN_FULL: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1) + MOVE(FULL, -1));
                full_t value = *(full_t*)(stack + sp + MOVE(FULL, -1));
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (FULL, 1);
                ip++;
                break;
            }
            case ASSIGN_HALF: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1) + MOVE(HALF, -1));
                half_t value = *(half_t*)(stack + sp + MOVE(HALF, -1));
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (HALF, 1);
                ip++;
                break;
            }
            case ASSIGN_SHORT: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1) + MOVE(SHORT, -1));
                short_t value = *(short_t*)(stack + sp + MOVE(SHORT, -1));
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (SHORT, 1);
                ip++;
                break;
            }
            case ASSIGN_BYTE: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1) + MOVE(BYTE, -1));
                byte_t value = *(byte_t*)(stack + sp + MOVE(BYTE, -1));
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (BYTE, 1);
                ip++;
                break;
            }
            case REF_ASSIGN: {
                full_t** target = *(full_t***)(stack + sp + MOVE(FULL, -2));
                full_t* value = *(full_t**)(stack + sp + MOVE(FULL, -1));

                try_free(*target, sp, 0, trace);

                // if (value) INCR_REF_COUNT(value);
                *target = value;
                sp -= MOVE(FULL, 1) + MOVE (FULL, 1);
                ip++;
                break;
            }
            case FIELD_ASSIGN: {
                ufull_t** target = *(ufull_t***)(stack + sp + MOVE(FULL, -3));
                ufull_t offset = *(ufull_t*)(stack + sp + MOVE(FULL, -2));
                full_t* value = *(ufull_t**)(stack + sp + MOVE(FULL, -1));

                try_free(*(target + offset), sp, 0, trace);

                // if (value) INCR_REF_COUNT(value);
                *(target + offset) = value;
                sp -= MOVE(FULL, 3);
                ip++;
                break;
            }
            case INT_ADD: {
                full_t value = (*(full_t*)(stack + sp + MOVE(FULL, -1))) + (*(full_t*)(stack + sp + MOVE(FULL, -2)));
                *(full_t*)(stack + sp + MOVE(FULL, -2)) = value;
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case INT_MUL: {
                full_t value = (*(full_t*)(stack + sp + MOVE(FULL, -1))) * (*(full_t*)(stack + sp + MOVE(FULL, -2)));
                *(full_t*)(stack + sp + MOVE(FULL, -2)) = value;
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case INT_SUB: {
                full_t value = (*(full_t*)(stack + sp + MOVE(FULL, -1))) - (*(full_t*)(stack + sp + MOVE(FULL, -2)));
                *(full_t*)(stack + sp + MOVE(FULL, -2)) = value;
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case INT_EQ: {
                byte_t eq = (*(full_t*)(stack + sp + MOVE(FULL, -1))) == (*(full_t*)(stack + sp + MOVE(FULL, -2)));
                sp -= MOVE(FULL, 2);
                *(byte_t*)(stack + sp) = eq; 
                sp += MOVE(BYTE, 1);
                ip++;
                break;
            }
            case INT_LT: {
                byte_t lt = (*(full_t*)(stack + sp + MOVE(FULL, -1))) < (*(full_t*)(stack + sp + MOVE(FULL, -2)));
                sp -= MOVE(FULL, 2);
                *(byte_t*)(stack + sp) = lt; 
                sp += MOVE(BYTE, 1);
                ip++;
                break;
            }
            case BOOL_EQ: {
                byte_t eq = !!(*(stack + sp + MOVE(BYTE, -1))) == !!(*(stack + sp + MOVE(BYTE, -2)));
                *(byte_t*)(stack + sp + MOVE(BYTE, -2)) = eq;
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case BOOL_NOT: {
                *(byte_t*)(stack + sp + MOVE(BYTE, -1)) = !(*(stack + sp + MOVE(BYTE, -1)));
                ip++;
                break;
            }
            case BOOL_AND: {
                byte_t res = (*(stack + sp + MOVE(BYTE, -1))) && (*(stack + sp + MOVE(BYTE, -2)));
                *(byte_t*)(stack + sp + MOVE(BYTE, -2)) = res;
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case BOOL_OR: {
                byte_t res = (*(stack + sp + MOVE(BYTE, -1))) || (*(stack + sp + MOVE(BYTE, -2)));
                *(byte_t*)(stack + sp + MOVE(BYTE, -2)) = res;
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case GETBP: {
                *(full_t*)(stack + sp) = (full_t)bp;
                sp += MOVE(FULL, 1);
                ip++;
                break;
            }
            // case GETSP: {
            //     *(full_t*)(stack + sp) = (full_t)sp;
            //     sp += MOVE(FULL, 1);
            //     ip++;
            //     break;
            // }
            // case MODSP: {
            //     full_t amount = *(full_t*)(p+ip+1);
            //     sp += amount;
            //     ip += MOVE(FULL, 1) + 1;
            //     break;
            // }
            case FREE_VAR: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                try_free(target, sp, 0, trace);
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case FREE_VARS: {
                full_t count = *(full_t*)(p+ip+1);
                while (count > 0) {
                    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                    try_free(target, sp, 0, trace);
                    sp -= MOVE(FULL, 1);
                    count--;
                }
                ip += MOVE(FULL, 1) + 1;
                break;
            }
            // case PRINT_VAR: {
            //     full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
            //     print_var(target);
            //     sp -= MOVE(FULL, 1);
            //     ip++;
            //     break;
            // }
            case PRINT_INT: {
                full_t value = *(full_t*)(stack + sp + MOVE(FULL, -1));
                printf("%lld", value);
                sp -= MOVE(FULL, 1);
                ip++;
                break;
            }
            case PRINT_BOOL: {
                byte_t value = *(byte_t*)(stack + sp + MOVE(BYTE, -1));
                if (value) printf("true");
                else printf("false");
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case PRINT_CHAR: {
                byte_t value = *(byte_t*)(stack + sp + MOVE(BYTE, -1));
                printf("%c", value);
                sp -= MOVE(BYTE, 1);
                ip++;
                break;
            }
            case STACK_FETCH: {
                full_t offset = *(full_t*)(p + ip + 1);
                full_t* value = (full_t*)(stack + MOVE(FULL, offset));
                *(full_t*)(stack + sp) = (full_t)value;
                sp += MOVE(FULL, 1);
                ip += MOVE(FULL, 1) + 1;
                break;
            }
            case BP_FETCH: {
                full_t offset = *(full_t*)(p + ip + 1);
                full_t* value = (full_t*)(stack + bp + MOVE(FULL, offset));
                *(full_t*)(stack + sp) = (full_t)value;
                sp += MOVE(FULL, 1);
                ip += MOVE(FULL, 1) + 1;
                break;
            }
            case SIZE_OF: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                *(full_t*)(stack + sp + MOVE(FULL, -1)) = ALLOC_SIZE(target);
                ip++;
                break;
            }
            case TO_START: {
                for(short i = 0; i < argument_count; i++) {
                    *(byte_t**)(stack + sp + MOVE(FULL, i)) = arguments[i];
                }
                bp = sp;
                sp += MOVE(FULL, argument_count);
                ip = entry_point;
                break;
            }
            case INCR_REF: {
                full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
                if (target) {
                    if (ON_HEAP(target)) {
                        INCR_REF_COUNT(target);
                    }
                    else if (ON_STACK(target, sp)) {
                        to_origin(&target, sp);
                        if (*target) {
                            INCR_REF_COUNT(*target);
                        }
                    }
                }
                ip++;
                break;
            }
            case GET_INPUT: {
                ufull_t typ = *(ufull_t*)(p + ip + 1);

                switch (typ) {
                    case BOOL: {
                        char* bool_buffer;
                        read_input(10, &bool_buffer);
                        byte_t bool_value = parse_bool(bool_buffer);
                        if (bool_value == -1) { printf("Failure: Expected a 'bool' but got: %s\n", bool_buffer); return -1; }
                        *(byte_t*)(stack + sp) = bool_value;
                        sp += MOVE(BYTE, 1);
                        break;
                    }
                    case CHAR: {
                        char* char_buffer;
                        read_input(3, &char_buffer);
                        byte_t char_value = parse_char(char_buffer);
                        if (char_value == -1) { printf("Failure: Expected a 'char' but got: %s\n", char_buffer); return -1; }
                        *(byte_t*)(stack + sp) = char_value;
                        sp += MOVE(BYTE, 1);
                        break;
                    }
                    case INT: {
                        char* int_buffer;
                        read_input(20, &int_buffer);
                        ufull_t int_value = parse_int(int_buffer);
                        // Needs a check
                        *(ufull_t*)(stack + sp) = int_value;
                        sp += MOVE(FULL, 1);
                        break;
                    }
                    default: 
                        printf("Failure: Unsupported GET_INPUT type");
                        return -1;
                }

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

    byte_t trace = false;
    byte_t time = false;

    short cmd_argument_count = 0;
    short flags = 0;

    for(int i = 1; i < argc; i++) {
        char* cmd_info = argv[i];
        if (is_flag(cmd_info)) flags++;
        else cmd_argument_count++;
    }

    if (!cmd_argument_count) { print_help(); return -1;}
    char* cmd_arguments[cmd_argument_count];

    short fls = 0;
    for(int i = 1; i < argc; i++) {
        char* cmd_info = argv[i];
        if (is_flag(cmd_info)) {
            switch (flag_index(cmd_info)) {
                case TRACE:
                    trace = true;
                    fls++;
                    break;
                case TIME:
                    time = true;
                    fls++;
                    break;
                default:
                    printf("Failure: Unknown flag: %s\n", cmd_info);
                    return -1;
            }
        }
        else cmd_arguments[i-(fls+1)] = cmd_info;
    }


    switch (command_index(cmd_arguments[0])) {
        case I: {
            if (cmd_argument_count != 2) { printf("Failure: Command 'i' requires 1 argument\n"); return -1; }

            byte_t* file;
            full_t file_len;
            load_file(cmd_arguments[1], &file, &file_len);
            if (file == NULL) { printf("Failure: No such file: %s\n", cmd_arguments[1]); return -1;} 

            print_entry_points(file);
            return 0;
        }
        case DISASS: {
            if (cmd_argument_count != 2) { printf("Failure: Command 'disass' requires 1 argument\n"); return -1; }

            byte_t* file;
            full_t file_len;
            load_file(cmd_arguments[1], &file, &file_len);
            if (file == NULL) { printf("Failure: No such file: %s\n", cmd_arguments[1]); return -1;}

            dissas(file, file_len);
            return 0;
        }
        case RUN: {
            if (cmd_argument_count < 3) { printf("Failure: Command 'run' requires 2 or more arguments\n"); return -1; }

            byte_t* file;
            full_t file_len;
            load_file(cmd_arguments[1], &file, &file_len);
            if (file == NULL) { printf("Failure: No such file: %s\n", cmd_arguments[1]); return -1;}

            //Find the requested entry point
            full_t* entry_point_info = find_entry_point(file, cmd_arguments[2]);
            if (entry_point_info == 0) { printf("Failure: No such entry point: %s\n", cmd_arguments[2]); return -1; }

            // Gather entry point information
            full_t entry_addr = entry_point_info[0];
            char* argument_info = (char*)(entry_point_info+1);
            char argument_count = argument_info[0];
            if (argument_count != cmd_argument_count-3) { printf("Failure: Argument mismatch\n"); return -1; }

            byte_t stack[STACKSIZE];
            memory_init(stack);

            // Load global variables
            byte_t* progresser = file;
            find_global_vars_start(progresser, &progresser);

            // byte_t* glob_var_ptr = progresser;
            // full_t glob_var_count = *((full_t*)glob_var_ptr);
            // glob_var_ptr += 8;
            int i = 0;
            // while (i < glob_var_count) {
            //     byte_t type = *glob_var_ptr;
            //     glob_var_ptr += 1;
            //     switch (type){
            //         case BOOL: {
            //             byte_t* alloc = allocate_simple(BYTE);
            //             *(alloc+1) = *glob_var_ptr;
            //             glob_var_ptr += MOVE(BYTE, 1);
            //             *(full_t*)(stack + MOVE(FULL, i)) = (full_t)alloc;
            //             break;
            //         }
            //         case INT: {
            //             byte_t* alloc = allocate_simple(FULL);
            //             *((full_t*)(alloc+1)) = *(full_t*)glob_var_ptr;
            //             glob_var_ptr += MOVE(FULL, 1);
            //             *(full_t*)(stack + MOVE(FULL, i)) = (full_t)alloc;
            //             break;
            //         }
            //         default:
            //             break;
            //     }
            //     i++;
            // }

            find_instruction_start(progresser, &progresser);

            // Load given arguments
            byte_t* arguments[argument_count];
            i = 0;
            while (i < argument_count) {
                switch (argument_info[i+1]) {
                    case CHAR: {
                        byte_t char_v = parse_char(cmd_arguments[i+3]);
                        if (char_v == -1) { printf("Failure: expected a char, but got: %s\n", cmd_arguments[i+3]); return -1; }
                        byte_t* char_alloc = allocate_simple(BYTE);
                        *(char_alloc) = char_v;
                        arguments[i] = char_alloc;
                        INCR_REF_COUNT(char_alloc);
                        break;
                    }
                    case BOOL: {
                        byte_t bool_v = parse_bool(cmd_arguments[i+3]);
                        if (bool_v == -1) { printf("Failure: expected a bool, but got: %s\n", cmd_arguments[i+3]); return -1; }
                        byte_t* bool_alloc = allocate_simple(BYTE);
                        *(bool_alloc) = bool_v;
                        arguments[i] = bool_alloc;
                        INCR_REF_COUNT(bool_alloc);
                        break;
                    }
                    case INT: {
                        full_t int_v = parse_int(cmd_arguments[i+3]);
                        if (int_v == 0 && !(strcmp(cmd_arguments[i+3], "0") == 0)) { printf("Failure: expected an int, but got: %s\n", cmd_arguments[i+3]); return -1; }
                        byte_t* int_alloc = allocate_simple(FULL);
                        *((full_t*)(int_alloc)) = int_v;
                        arguments[i] = int_alloc;
                        INCR_REF_COUNT(int_alloc);
                        break;
                    }
                    default: ;
                        printf("Failure: Unknown type\n");
                        return -1;
                }
                i++;
            }

            clock_t ticks;
            if (time) ticks = clock();
            if (trace) 
                printf("Running %s @ instruction #%lld...\n\n", cmd_arguments[2], entry_addr);

            int return_code = run(progresser, entry_addr, stack, arguments, argument_count, trace, time);

            if (time) 
                printf("Total execution time: %fs\n", (((double)(clock() - ticks))/CLOCKS_PER_SEC));
                
            return return_code;
        }
        case HELP: {
            print_help();
            break;
        }
        default:
            printf("Failure: Unknown command: %s", cmd_arguments[0]);
            print_help();
            return -1;
    }
}