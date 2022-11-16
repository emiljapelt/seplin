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
    uint total_size = 8+SIZE(type);
    byte* alloc = (byte*)malloc(total_size);
    if (alloc+total_size > heap_max) heap_max = alloc+total_size;
    if (alloc < heap_min) heap_min = alloc;

    byte* b = alloc+8;
    for(uint i = 0; i < total_size-8; i++) b[i] = 0;

    uword header = 1ul << 32;  // reference count
    header = header | ((uword)SIZE(type) << 1); // payload size in bytes, and 0 for non struct

    *((uword*)alloc) = header;

    return alloc+8;
}

byte* allocate_struct(uint fields) {
    uint total_size = 8+(fields*8);
    byte* alloc = (byte*)malloc(total_size);
    if (alloc+total_size > heap_max) heap_max = alloc+total_size;
    if (alloc < heap_min) heap_min = alloc;

    uword* field = ((uword*)alloc)+1;
    for(uint i = 0; i < fields; i++) field[i] = 0;

    uword header = 1ul << 32;  // reference count
    header = header | ((fields << 1) | 1); // amount of fields, and 1 for struct

    *((uword*)alloc) = header;

    return alloc+8;
}

void try_free(word* addr) {
    printf("freeing %llx\n", ((word)addr)+0);
    if (addr == 0) return;
    
    printf("refs %lld\n", REF_COUNT(addr));
    printf("struct %lld\n", IS_STRUCT(addr));
    printf("size %lld\n", ALLOC_SIZE(addr));
    DECR_REF_COUNT(addr);
    printf("refs %lld\n", REF_COUNT(addr));
    printf("struct %lld\n", IS_STRUCT(addr));
    printf("size %lld\n", ALLOC_SIZE(addr));
    if (IS_STRUCT(addr)) {
        printf("freeing struct\n");
        uint fields = ALLOC_SIZE(addr);
        word** point = (word**)addr;
        for(int i = 0; i < fields; i++) try_free(*(point + i));
    }
    if (REF_COUNT(addr)) return;
    free(addr);
}

byte on_heap(byte* addr) {
    return heap_min <= addr && addr <= heap_max;
}

byte on_stack(byte* addr, uword sp) {
    return stack_base <= addr && addr <= (stack_base + sp);
}
