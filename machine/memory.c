#include <limits.h>
#include <stdlib.h>

#include "memory.h"
#include "defs.h"

byte* heap_min;
byte* heap_max;
byte* stack_base;

void memory_init(byte* sb) {
    stack_base = sb;
    heap_max = INT_MIN;
    heap_min = INT_MAX;
}

byte* allocate(uint size) {
    byte* alloc = (byte*)malloc(size+8);
    if (alloc+size+8 > heap_max) heap_max = alloc+size+8;
    if (alloc < heap_min) heap_min = alloc;

    *((int*)alloc) = 0; // ref_count
    *(((int*)alloc)+1) = size; // seqment size

    return alloc+8;
}

void try_free(byte* addr, uword sp) {
    if (addr == 0) return;
    if (on_stack(addr, sp)) return;
    free(addr);
}

byte on_heap(byte* addr) {
    return heap_min <= addr && addr <= heap_max;
}

byte on_stack(byte* addr, uword sp) {
    return stack_base <= addr && addr <= (stack_base + sp);
}
