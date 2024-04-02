#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "types.h"

byte_t* heap_min;
byte_t* heap_max;
byte_t* stack_base;

void memory_init(byte_t** stack) {
    byte_t* stack_alloc = malloc(STACKSIZE);
    stack_base = stack_alloc;
    *stack = stack_alloc;
    heap_max = (byte_t*)0;
    heap_min = (byte_t*)ULONG_MAX;
}

byte_t* allocate_simple(byte_t type) {
    unsigned int total_size = 8+SIZE(type);
    byte_t* alloc = (byte_t*)malloc(total_size);
    if (alloc+total_size > heap_max) heap_max = alloc+total_size;
    if (alloc < heap_min) heap_min = alloc;

    memset(alloc+8, 0, SIZE(type));

    #if defined(ENV64)
        *((uhalf_t*)alloc) = 0; // referecne count
        *(((uhalf_t*)alloc)+1) = ((uhalf_t)SIZE(type) << 1); // byte size and non-struct mark
    #elif defined(ENV32)
        // ufull_t instead of uhalf_t
        #error "Error: The Seplin virtual machine does not yet support 32bit architectures."
    #else
        #error "Error: Could not resolve architecture size (32/64bit)."
    #endif

    return alloc+8;
}

byte_t* allocate_struct(unsigned int fields) {
    unsigned int total_size = 8+(fields*SIZE(FULL));
    byte_t* alloc = (byte_t*)malloc(total_size);
    if (alloc+total_size > heap_max) heap_max = alloc+total_size;
    if (alloc < heap_min) heap_min = alloc;

    memset(((ufull_t*)alloc)+1, 0, (fields*SIZE(FULL)));

    #if defined(ENV64)
        *((uhalf_t*)alloc) = 0; // reference count
        *(((uhalf_t*)alloc)+1) = ((((uhalf_t)fields) << 1) | 1); // field count and struct mark
    #elif defined(ENV32)
        // ufull_t instead of uhalf_t
        #error "Error: The Seplin virtual machine does not yet support 32bit architectures."
    #else
        #error "Error: Could not resolve architecture size (32/64bit)."
    #endif

    return alloc+8;
}

void try_free(full_t* addr, ufull_t sp, unsigned int depth, byte_t trace) {
    if (!addr) return;
    if (!ON_HEAP(addr)) {
        addr = *(full_t**)addr;
        if (!addr) return;
    }
    
    if (trace) {
        for(unsigned int i = 0; i < depth; i++) printf("  ");
        printf("Trying to free: 0x%llx, refs: %i\n", (ufull_t)addr, REF_COUNT(addr));
    }

    DECR_REF_COUNT(addr);
    if (REF_COUNT(addr)) return;
    if (IS_STRUCT(addr)) {
        unsigned int fields = ALLOC_SIZE(addr);
        full_t** point = (full_t**)addr;
        for(int i = 0; i < fields; i++) try_free(*(point + i), sp, depth+1, trace);
    }
    if (trace) {
        for(unsigned int i = 0; i < depth; i++) printf("  ");
        printf("Freeing: 0x%llx\n", (ufull_t)addr);
    }
    #if defined(ENV64)
        free(addr-1);
    #elif defined(ENV32)
        // -2 instead of -1
        #error "Error: The Seplin virtual machine does not yet support 32bit architectures."
    #else
        #error "Error: Could not resolve architecture size (32/64bit)."
    #endif
}