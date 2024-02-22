#ifndef INSTRUCTIONS_H
#define INSTRUCTIONS_H

#include "memory.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "input.h"

#define MOVE(unit, steps) (SIZE(unit) * (steps))

extern byte_t* stack;
extern ufull_t depth;
extern ufull_t sp;
extern ufull_t bp;

extern byte_t* program;
extern ufull_t ip;

extern byte_t tracing;
extern byte_t timeing;
extern clock_t ticks;

static inline void halt() {
    if (tracing) printf("Halting...\n");
    if (timeing) printf("Total cpu time: %fs\n", (((double)(clock() - ticks))/CLOCKS_PER_SEC));
    exit(0);
}

static inline void stop() {
    if (depth == 0) halt();

    ufull_t i = sp - MOVE(FULL, 1);
    while (i >= bp) {
        try_free(*((full_t**)(stack+i)), sp, 0, tracing);
        i -= MOVE(FULL, 1);
    }

    depth--;
    ufull_t old_bp = *(full_t*)(stack + bp + MOVE(FULL, -1));
    ufull_t next_ip = *(full_t*)(stack + bp + MOVE(FULL, -2));
    sp = bp - MOVE(FULL, 2);
    bp = old_bp;
    ip = next_ip;
}

static inline void call() {
    full_t arg_count = *(full_t*)(stack + sp + MOVE(FULL, -2));
    full_t target = *(full_t*)(stack + sp + MOVE(FULL, -1));

    for(int i = 0; i < arg_count; i++) {
        full_t* arg = *(full_t**)(stack + sp - MOVE(FULL, (i+3)));
        *(full_t*)(stack + sp - MOVE(FULL, i+1)) = (full_t)(arg);
    }

    *(full_t*)((stack + sp) - MOVE(FULL, arg_count+2)) = ip+1;
    *(full_t*)((stack + sp) - MOVE(FULL, arg_count+1)) = (full_t)bp;

    depth++;
    bp = sp - MOVE(FULL, arg_count);
    ip = target;
}

static inline void _goto() {
    full_t target = *(full_t*)(program + ip + 1);
    ip = target;
}

static inline void if_true() {
    if (*(byte_t*)(stack + sp + MOVE(BYTE, -1))) {
        sp -= MOVE(BYTE, 1);
        ip = *(full_t*)(program + ip + 1);
    }
    else {
        sp -= MOVE(BYTE, 1);
        ip += 1 + MOVE(FULL, 1);
    }
}

static inline void place_byte() {
    full_t value = *(byte_t*)(program + ip + 1);
    *(byte_t*)(stack + sp) = value;
    sp += MOVE(BYTE, 1);
    ip += MOVE(BYTE, 1) + 1;
}

static inline void place_full() {
    full_t value = *(full_t*)(program + ip + 1);
    *(full_t*)(stack + sp) = value;
    sp += MOVE(FULL, 1);
    ip += MOVE(FULL, 1) + 1;
}

static inline void clone_full() {
    full_t value = *(full_t*)(stack + sp + MOVE(FULL, -1));
    *(full_t*)(stack + sp) = value;
    sp += MOVE(FULL, 1);
    ip++;
}

static inline void clone_half() {
    byte_t value = *(byte_t*)(stack + sp + MOVE(HALF, -1));
    *(byte_t*)(stack + sp) = value;
    sp += MOVE(HALF, 1);
    ip++;
}

static inline void clone_short() {
    byte_t value = *(byte_t*)(stack + sp + MOVE(SHORT, -1));
    *(byte_t*)(stack + sp) = value;
    sp += MOVE(SHORT, 1);
    ip++;   
}

static inline void clone_byte() {
    byte_t value = *(byte_t*)(stack + sp + MOVE(BYTE, -1));
    *(byte_t*)(stack + sp) = value;
    sp += MOVE(BYTE, 1);
    ip++;
}

static inline void fetch_full() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
    *(full_t*)(stack + sp + MOVE(FULL, -1)) = *((full_t*)target);
    ip++;   
}

static inline void fetch_half() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
    *(half_t*)(stack + sp + MOVE(FULL, -1)) = *((half_t*)target);
    sp -= MOVE(HALF, 1);
    ip++;
}

static inline void fetch_short() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
    *(short_t*)(stack + sp + MOVE(FULL, -1)) = *((short_t*)target);
    sp -= MOVE(SHORT, 3);
    ip++;
}

static inline void fetch_byte() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
    *(byte_t*)(stack + sp + MOVE(FULL, -1)) = *((byte_t*)target);
    sp -= MOVE(BYTE, 7);
    ip++;
}

static inline void field_fetch() {
    ufull_t offset = *(ufull_t*)(stack + sp + MOVE(FULL, -1));
    full_t** target = *((full_t***)(stack + sp + MOVE(FULL, -2)));

    if (!IS_STRUCT(target)) { printf("Failure: Field fetch from non-struct data\n"); exit(-1); } 
    if (ALLOC_SIZE(target) <= offset) { printf("Failure: Field fetch out of struct bounds\n"); exit(-1); } 

    *(full_t***)(stack + sp + MOVE(FULL, -2)) = target + offset;
    sp -= MOVE(FULL, 1);
    ip++;
}

static inline void ref_fetch() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
    if (ON_STACK((byte_t*)target, sp)) to_origin(&target, sp);
    *(full_t**)(stack + sp + MOVE(FULL, -1)) = target;
    ip++;
}

static inline void declare_full() {
    byte_t* alloc = allocate_simple(FULL);
    *(full_t*)(stack + sp) = (full_t)((full_t*)alloc);
    sp += MOVE(FULL, 1);
    ip++;
}

static inline void declare_half() {
    byte_t* alloc = allocate_simple(HALF);
    *(full_t*)(stack + sp) = (full_t)((full_t*)alloc);
    sp += MOVE(FULL, 1);
    ip++;
}

static inline void declare_short() {
    byte_t* alloc = allocate_simple(SHORT);
    *(full_t*)(stack + sp) = (full_t)((full_t*)alloc);
    sp += MOVE(FULL, 1);
    ip++;
}

static inline void declare_byte() {
    byte_t* alloc = allocate_simple(BYTE);
    *(full_t*)(stack + sp) = (full_t)((full_t*)alloc);
    sp += MOVE(FULL, 1);
    ip++;
}

static inline void declare_struct() {
    ufull_t fields = *(ufull_t*)(stack + sp + MOVE(FULL, -1));
    byte_t* alloc = allocate_struct(fields);
    *(full_t*)(stack + sp + MOVE(FULL, -1)) = (full_t)((full_t*)alloc);
    ip++;
}

static inline void assign_full() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1) + MOVE(FULL, -1));
    full_t value = *(full_t*)(stack + sp + MOVE(FULL, -1));
    *target = value;
    sp -= MOVE(FULL, 1) + MOVE (FULL, 1);
    ip++;
}

static inline void assign_half() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1) + MOVE(HALF, -1));
    half_t value = *(half_t*)(stack + sp + MOVE(HALF, -1));
    *target = value;
    sp -= MOVE(FULL, 1) + MOVE (HALF, 1);
    ip++;
}

static inline void assign_short() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1) + MOVE(SHORT, -1));
    short_t value = *(short_t*)(stack + sp + MOVE(SHORT, -1));
    *target = value;
    sp -= MOVE(FULL, 1) + MOVE (SHORT, 1);
    ip++;
}

static inline void assign_byte() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1) + MOVE(BYTE, -1));
    byte_t value = *(byte_t*)(stack + sp + MOVE(BYTE, -1));
    *target = value;
    sp -= MOVE(FULL, 1) + MOVE (BYTE, 1);
    ip++;
}

static inline void ref_assign() {
    full_t** target = *(full_t***)(stack + sp + MOVE(FULL, -2));
    full_t* value = *(full_t**)(stack + sp + MOVE(FULL, -1));

    try_free(*target, sp, 0, tracing);

    *target = value;
    sp -= MOVE(FULL, 1) + MOVE (FULL, 1);
    ip++;
}

static inline void field_assign() {
    full_t** target = *(full_t***)(stack + sp + MOVE(FULL, -3));
    full_t offset = *(full_t*)(stack + sp + MOVE(FULL, -2));
    full_t* value = *(full_t**)(stack + sp + MOVE(FULL, -1));

    try_free(*(target + offset), sp, 0, tracing);

    *(target + offset) = value;
    sp -= MOVE(FULL, 3);
    ip++;
}

static inline void int_add() {
    full_t value = (*(full_t*)(stack + sp + MOVE(FULL, -1))) + (*(full_t*)(stack + sp + MOVE(FULL, -2)));
    *(full_t*)(stack + sp + MOVE(FULL, -2)) = value;
    sp -= MOVE(FULL, 1);
    ip++;
}

static inline void int_mul() {
    full_t value = (*(full_t*)(stack + sp + MOVE(FULL, -1))) * (*(full_t*)(stack + sp + MOVE(FULL, -2)));
    *(full_t*)(stack + sp + MOVE(FULL, -2)) = value;
    sp -= MOVE(FULL, 1);
    ip++;
}

static inline void int_sub() {
    full_t value = (*(full_t*)(stack + sp + MOVE(FULL, -1))) - (*(full_t*)(stack + sp + MOVE(FULL, -2)));
    *(full_t*)(stack + sp + MOVE(FULL, -2)) = value;
    sp -= MOVE(FULL, 1);
    ip++;
}

static inline void full_eq() {
    byte_t eq = (*(full_t*)(stack + sp + MOVE(FULL, -1))) == (*(full_t*)(stack + sp + MOVE(FULL, -2)));
    sp -= MOVE(FULL, 2);
    *(byte_t*)(stack + sp) = eq; 
    sp += MOVE(BYTE, 1);
    ip++;
}

static inline void half_eq() {
    byte_t eq = (*(half_t*)(stack + sp + MOVE(HALF, -1))) == (*(half_t*)(stack + sp + MOVE(HALF, -2)));
    sp -= MOVE(HALF, 2);
    *(byte_t*)(stack + sp) = eq; 
    sp += MOVE(BYTE, 1);
    ip++;
}

static inline void short_eq() {
    byte_t eq = (*(short_t*)(stack + sp + MOVE(SHORT, -1))) == (*(short_t*)(stack + sp + MOVE(SHORT, -2)));
    sp -= MOVE(SHORT, 2);
    *(byte_t*)(stack + sp) = eq; 
    sp += MOVE(BYTE, 1);
    ip++;
}

static inline void byte_eq() {
    byte_t eq = (*(byte_t*)(stack + sp + MOVE(BYTE, -1))) == (*(byte_t*)(stack + sp + MOVE(BYTE, -2)));
    sp -= MOVE(BYTE, 2);
    *(byte_t*)(stack + sp) = eq; 
    sp += MOVE(BYTE, 1);
    ip++;
}

static inline void int_lt() {
    byte_t lt = (*(full_t*)(stack + sp + MOVE(FULL, -1))) < (*(full_t*)(stack + sp + MOVE(FULL, -2)));
    sp -= MOVE(FULL, 2);
    *(byte_t*)(stack + sp) = lt; 
    sp += MOVE(BYTE, 1);
    ip++;
}

static inline void bool_eq() {
    byte_t eq = !!(*(stack + sp + MOVE(BYTE, -1))) == !!(*(stack + sp + MOVE(BYTE, -2)));
    *(byte_t*)(stack + sp + MOVE(BYTE, -2)) = eq;
    sp -= MOVE(BYTE, 1);
    ip++;
}

static inline void bool_not() {
    *(byte_t*)(stack + sp + MOVE(BYTE, -1)) = !(*(stack + sp + MOVE(BYTE, -1)));
    ip++;
}

static inline void bool_and() {
    byte_t res = (*(stack + sp + MOVE(BYTE, -1))) && (*(stack + sp + MOVE(BYTE, -2)));
    *(byte_t*)(stack + sp + MOVE(BYTE, -2)) = res;
    sp -= MOVE(BYTE, 1);
    ip++;
}

static inline void bool_or() {
    byte_t res = (*(stack + sp + MOVE(BYTE, -1))) || (*(stack + sp + MOVE(BYTE, -2)));
    *(byte_t*)(stack + sp + MOVE(BYTE, -2)) = res;
    sp -= MOVE(BYTE, 1);
    ip++;
}

static inline void getbp() {
    *(full_t*)(stack + sp) = (full_t)bp;
    sp += MOVE(FULL, 1);
    ip++;
}

static inline void free_var() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
    try_free(target, sp, 0, tracing);
    sp -= MOVE(FULL, 1);
    ip++;
}

static inline void free_vars() {
    full_t count = *(full_t*)(program+ip+1);
    while (count > 0) {
        full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
        try_free(target, sp, 0, tracing);
        sp -= MOVE(FULL, 1);
        count--;
    }
    ip += MOVE(FULL, 1) + 1;
}

static inline void print_int() {
    full_t value = *(full_t*)(stack + sp + MOVE(FULL, -1));
    printf("%lld", value);
    sp -= MOVE(FULL, 1);
    ip++;
}

static inline void print_bool() {
    byte_t value = *(byte_t*)(stack + sp + MOVE(BYTE, -1));
    if (value) printf("true");
    else printf("false");
    sp -= MOVE(BYTE, 1);
    ip++;
}

static inline void print_char() {
    byte_t value = *(byte_t*)(stack + sp + MOVE(BYTE, -1));
    printf("%c", value);
    sp -= MOVE(BYTE, 1);
    ip++;
}

static inline void stack_fetch() {
    full_t offset = *(full_t*)(program + ip + 1);
    full_t* value = (full_t*)(stack + MOVE(FULL, offset));
    *(full_t*)(stack + sp) = (full_t)value;
    sp += MOVE(FULL, 1);
    ip += MOVE(FULL, 1) + 1;
}

static inline void bp_fetch() {
    full_t offset = *(full_t*)(program + ip + 1);
    full_t* value = (full_t*)(stack + bp + MOVE(FULL, offset));
    *(full_t*)(stack + sp) = (full_t)value;
    sp += MOVE(FULL, 1);
    ip += MOVE(FULL, 1) + 1;
}

static inline void size_of() {
    full_t* target = *(full_t**)(stack + sp + MOVE(FULL, -1));
    *(full_t*)(stack + sp + MOVE(FULL, -1)) = ALLOC_SIZE(target);
    ip++;
}

static inline void incr_ref() {
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
}

static inline void get_input() {
    ufull_t typ = *(ufull_t*)(program + ip + 1);

    switch (typ) {
        case 0: { // Int
            char* int_buffer;
            read_input(20, &int_buffer);
            ufull_t int_value = parse_int(int_buffer);
            // Needs a check
            *(ufull_t*)(stack + sp) = int_value;
            sp += MOVE(FULL, 1);
            break;
        }
        case 1: { // Bool
            char* bool_buffer;
            read_input(10, &bool_buffer);
            byte_t bool_value = parse_bool(bool_buffer);
            if (bool_value == -1) { printf("Failure: Expected a 'bool' but got: %s\n", bool_buffer); exit(-1); }
            *(byte_t*)(stack + sp) = bool_value;
            sp += MOVE(BYTE, 1);
            break;
        }
        case 2: { // Char
            char* char_buffer;
            read_input(3, &char_buffer);
            byte_t char_value = parse_char(char_buffer);
            if (char_value == -1) { printf("Failure: Expected a 'char' but got: %s\n", char_buffer); exit(-1); }
            *(byte_t*)(stack + sp) = char_value;
            sp += MOVE(BYTE, 1);
            break;
        }
        case 3: { // String aka. Char[]
            char* char_buffer;
            read_input(100, &char_buffer);
            int string_len = strlen(char_buffer);
            byte_t** array_alloc = (byte_t**)allocate_struct(string_len);
            for(int i = 0; i < string_len; i++) {
                byte_t* char_alloc = allocate_simple(BYTE);
                *char_alloc = char_buffer[i];
                array_alloc[i] = char_alloc;
            }
            *(byte_t***)(stack + sp) = array_alloc;
            sp += MOVE(FULL, 1);
            break;
        }
        default: 
            printf("Failure: Unsupported GET_INPUT type\n");
            exit(-1);
    }
    ip += MOVE(FULL, 1) + 1;
}

#endif