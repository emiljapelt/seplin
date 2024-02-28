#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "ISA.h"
#include "instructions.h"
#include "types.h"
#include "memory.h"
#include "file_analysis.h"
#include "commands.h"

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
    printf(
        "Welcome to Seplin!\n\n"
        "Here are the available commands:\n"
        "   help:\n"
        "       Prints this message.\n"
        "   i <file_path>:\n"
        "       Prints the interface of a compiled Seplin file.\n"
        "   disass <file_path>:\n"
        "       Prints the instructions of a compiled Seplin file, using their names.\n"
        "   run <file_path> <entry_point> <args>*:\n"
        "       Executes a Seplin file, starting from the given entry point, using the given arguments.\n"
        "       --time: Measure and print the execution time, in seconds.\n"
        "       --trace: Print information about the execution.\n"
        "           This includes which instruction is being executed, the current stack and memory management.\n"
    );
}

byte_t* stack;
ufull_t depth = 0;
ufull_t sp = 0;
ufull_t bp = 0;

byte_t* program;
ufull_t ip = 0;

// machine flags
byte_t tracing = false;
byte_t timeing = false;
clock_t ticks;

void run(full_t entry_point, byte_t* arguments[], int argument_count) {    

    if (tracing) { print_stack(stack, bp, sp); printf("\n"); }

    while(true) {

        if (sp < bp) { printf("Failure: stack underflow!\n"); exit(-1); }
        if (sp > STACKSIZE) { printf("Failure: stack overflow!\n"); exit(-1); }

        if (tracing) printf("instruction #%llu: ", ip);
        byte_t i = program[ip];
        if (tracing) printf("0x%x %s\n", i, instruction_to_string(i) );

        switch (i)
        {
            case HALT: halt();
            case STOP: stop(); break;
            case CALL: call(); break;
            case GOTO: _goto(); break;
            case IF_TRUE: if_true(); break;
            case PLACE_BYTE: place_byte(); break;
            case PLACE_FULL: place_full(); break;
            case CLONE_FULL: clone_full(); break;
            case CLONE_HALF: clone_half(); break;
            case CLONE_SHORT: clone_short(); break;
            case CLONE_BYTE: clone_byte(); break;
            case FETCH_FULL: fetch_full(); break;
            case FETCH_HALF: fetch_half(); break;
            case FETCH_SHORT: fetch_short(); break;
            case FETCH_BYTE: fetch_byte(); break;
            case FIELD_FETCH: field_fetch(); break;
            case REF_FETCH: ref_fetch(); break;
            case DECLARE_FULL: declare_full(); break;
            case DECLARE_HALF: declare_half(); break;
            case DECLARE_SHORT: declare_short(); break;
            case DECLARE_BYTE: declare_byte(); break;
            case DECLARE_STRUCT: declare_struct(); break;
            case ASSIGN_FULL: assign_full(); break;
            case ASSIGN_HALF: assign_half(); break;
            case ASSIGN_SHORT: assign_short(); break;
            case ASSIGN_BYTE: assign_byte(); break;
            case REF_ASSIGN: ref_assign(); break;
            case FIELD_ASSIGN: field_assign(); break;
            case INT_ADD: int_add(); break;
            case INT_MUL: int_mul(); break; 
            case INT_SUB: int_sub(); break;
            case FULL_EQ: full_eq(); break;
            case HALF_EQ: half_eq(); break;
            case SHORT_EQ: short_eq(); break;
            case BYTE_EQ: byte_eq(); break;
            case INT_LT: int_lt(); break;
            case BOOL_EQ: bool_eq(); break;
            case BOOL_NOT: bool_not(); break;
            case BOOL_AND: bool_and(); break;
            case BOOL_OR: bool_or(); break;
            case GETBP: getbp(); break;
            case FREE_VAR: free_var(); break;
            case FREE_VARS: free_vars(); break;
            case PRINT_INT: print_int(); break;
            case PRINT_BOOL: print_bool(); break;
            case PRINT_CHAR: print_char(); break;
            case STACK_FETCH: stack_fetch(); break;
            case BP_FETCH: bp_fetch(); break;
            case SIZE_OF: size_of(); break;
            case INCR_REF: incr_ref(); break;
            case GET_INPUT: get_input(); break;
            case START: { 
                for(short i = 0; i < argument_count; i++) {
                    *(byte_t**)(stack + sp + MOVE(FULL, i)) = arguments[i];
                }
                bp = sp;
                sp += MOVE(FULL, argument_count);
                ip = **(((ufull_t**)stack) + entry_point);
                break;
            }
            default: {
                printf("Failure: Unknown instruction: %x\n", program[ip]);
                exit(-1);
            }
        }

        if (tracing) { print_stack(stack, bp, sp); printf("\n"); }
    }
}

char compile(char** path) {
    unsigned long path_length = strlen(*path);
    if (string_ends_with(4, ".sec", path_length, *path)) { return 1;}
    else if (string_ends_with(4, ".sep", path_length, *path)) {
        int command_length = path_length+11;
        char command[command_length+1];
        memset(command, 0, command_length);
        
        strcat(command, "seplinc ");
        strcat(command, *path);
        int result = system(command);
        if (result != 0) { printf("Failure: Compilation failed.\n"); return 0;}
        char* compiled_file_name = (char*)malloc(sizeof(char)*path_length);
        memset(compiled_file_name, 0, path_length);

        strcat(compiled_file_name, *path);
        compiled_file_name[path_length-1] = 'c';
        *path = compiled_file_name;
        return 1;
    }
    else { printf("Failure: Given a file without Seplin related suffix.\n"); return 0; }
}

// Call should be: 'seplin <command> <file>.ixc arg1? arg2? ...'
int main(int argc, char** argv) {

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
                    tracing = true;
                    fls++;
                    break;
                case TIME:
                    timeing = true;
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
            if (!compile(cmd_arguments+1)) { return -1; }

            byte_t* file;
            full_t file_len;
            load_file(cmd_arguments[1], &file, &file_len);
            if (file == NULL) { printf("Failure: No such file: %s\n", cmd_arguments[1]); return -1;}
            struct file_segments segments = find_segments(file);
            printf("debug\n");

            printf("Structs:\n");
            print_structs(segments.struct_segment);
            printf("Global variables:\n");
            print_global_vars(segments.global_var_segment, segments.struct_segment);
            printf("Entry points:\n");
            print_entry_points(segments.entry_point_segment, segments.struct_segment);
            return 0;
        }
        case DISASS: {
            if (cmd_argument_count != 2) { printf("Failure: Command 'disass' requires 1 argument\n"); return -1; }
            if (!compile(cmd_arguments+1)) { return -1; }

            byte_t* file;
            full_t file_len;
            load_file(cmd_arguments[1], &file, &file_len);
            if (file == NULL) { printf("Failure: No such file: %s\n", cmd_arguments[1]); return -1;}
            file_segments segments = find_segments(file);

            printf("Structs:\n");
            print_structs(segments.struct_segment);
            printf("Global variables:\n");
            print_global_vars(segments.global_var_segment, segments.struct_segment);
            printf("Entry points:\n");
            print_entry_points(segments.entry_point_segment, segments.struct_segment);
            printf("Instructions:\n");
            dissas(file, segments.instructions_segment, file_len);
            return 0;
        }
        case COMPILE: {
            if (cmd_argument_count != 2) { printf("Failure: Command 'comp' requires 1 argument\n"); return -1; }
            if (!compile(cmd_arguments+1)) { return -1; }
            return 0;
        }
        case RUN: {
            if (cmd_argument_count < 3) { printf("Failure: Command 'run' requires 2 or more arguments\n"); return -1; }
            if (access(cmd_arguments[1], F_OK) != 0) { printf("Failure: File not accesible: %s\n", cmd_arguments[1]); return -1; }
            if (!compile(cmd_arguments+1)) { return -1; }

            file_segments segments;

            {
                byte_t* file;
                full_t file_len;
                load_file(cmd_arguments[1], &file, &file_len);
                if (file == NULL) { printf("Failure: No such file: %s\n", cmd_arguments[1]); return -1;}

                segments = find_segments(file);
            }

            //Find the requested entry point
            entry_point_info entry_point = find_entry_point(segments.entry_point_segment, cmd_arguments[2]);
            if (entry_point.found == false) { printf("Failure: No such entry point: %s\n", cmd_arguments[2]); return -1; }
            if (entry_point.argument_count != cmd_argument_count-3) { printf("Failure: Argument mismatch\n"); return -1; }

            memory_init(&stack);

            // Load given arguments
            int i = 0;
            byte_t* arguments[entry_point.argument_count];
            while (i < entry_point.argument_count) {
                byte_t locked_var = *entry_point.argument_types;
                entry_point.argument_types += 1;
                switch (*entry_point.argument_types) {
                    case INT: // simple
                    case BOOL:
                    case CHAR: {
                        arguments[i] = load_simple_argument(*entry_point.argument_types, cmd_arguments[i+3]);
                        entry_point.argument_types += 1;
                        break;
                    }
                    case ARRAY: // array
                    case STRUCT: // struct
                    case GENERIC: // generic
                        printf("Seplin does not support array and struct as commandline arguments\n");
                        return -1;
                    default:
                        printf("Unknown type type\n");
                        return -1;
                }
                i++;
            }

            if (timeing) ticks = clock();
            if (tracing) 
                printf("Running %s @ instruction #%lld...\n\n", cmd_arguments[2], entry_point.instruction_addr);

            program = segments.instructions_segment;
            run(
                entry_point.instruction_addr, 
                arguments, 
                entry_point.argument_count 
            );
                
            return 0;
        }
        case HELP: {
            print_help();
            return 0;
        }
        default:
            printf("Failure: Unknown command: %s", cmd_arguments[0]);
            print_help();
            return -1;
    }
}