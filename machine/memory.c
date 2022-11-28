#include <limits.h>
#include <stdlib.h>
#include <stdio.h>

#include "memory.h"
#include "defs.h"
#include "types.h"

byte* heap_min;
byte* heap_max;
byte* stack_base;

void memory_init(byte* sb) {
    stack_base = sb;
    heap_max = (byte*)0;
    heap_min = (byte*)ULONG_MAX;
}

byte* allocate_simple(byte type) {
    unsigned int total_size = 8+SIZE(type);
    byte* alloc = (byte*)malloc(total_size);
    if (alloc+total_size > heap_max) heap_max = alloc+total_size;
    if (alloc < heap_min) heap_min = alloc;

    byte* b = alloc+8;
    for(unsigned int i = 0; i < total_size-8; i++) b[i] = 0;

    uword header = 1ull << 32;  // reference count
    header = header | ((uword)SIZE(type) << 1); // payload size in bytes, and 0 for non struct

    *((uword*)alloc) = header;

    return alloc+8;
}

byte* allocate_struct(unsigned int fields) {
    unsigned int total_size = 8+(fields*8);
    byte* alloc = (byte*)malloc(total_size);
    if (alloc+total_size > heap_max) heap_max = alloc+total_size;
    if (alloc < heap_min) heap_min = alloc;

    uword* field = ((uword*)alloc)+1;
    for(unsigned int i = 0; i < fields; i++) field[i] = 0;

    uword header = 1ull << 32;  // reference count
    header = header | ((((uword)fields) << 1) | 1); // amount of fields, and 1 for struct marking

    *((uword*)alloc) = header;

    return alloc+8;
}

void try_free(word* addr, uword sp, unsigned int depth, byte trace) {
    if (addr == 0) return;
    to_origin((word**)addr, sp);
    addr = (word*)*addr;
    if (trace) {
        for(unsigned int i = 0; i < depth; i++) printf("  ");
        printf("Trying to free: 0x%llx\n", (uword)addr);
    }

    DECR_REF_COUNT(addr);
    if (REF_COUNT(addr)) return;
    if (IS_STRUCT(addr)) {
        unsigned int fields = ALLOC_SIZE(addr);
        word** point = (word**)addr;
        for(int i = 0; i < fields; i++) try_free(*(point + i), sp, depth+1, trace);
    }
    if (trace) {
        for(unsigned int i = 0; i < depth; i++) printf("  ");
        printf("Freeing: 0x%llx\n", (uword)addr);
    }
    free(addr-1);
}

byte on_heap(byte* addr) {
    return heap_min <= addr && addr <= heap_max;
}

byte on_stack(byte* addr, uword sp) {
    return stack_base <= addr && addr <= (stack_base + sp);
}

byte to_origin(word** target, uword sp) {
    if (target == 0) return true;
    if (!on_heap((byte*)target) && !on_stack((byte*)target, sp)) return false;
    if (on_heap((byte*)target)) return true;
    if (on_stack((byte*)target, sp)) {
        word* temp = *target;
        while(true) {
            if (on_heap((byte*)*temp)) break;
            else temp = (word*)*temp;
        }
        *target = temp;
    }
}