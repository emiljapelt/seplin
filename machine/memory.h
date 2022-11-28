
#include "defs.h"

#define FULL 0
#define HALF 1
#define SHORT 2
#define BYTE 3

#define SIZE(t) (((t) == FULL) ? 8 : ((t) == HALF) ? 4 : ((t) == SHORT) ? 2 : 1)

#define IS_STRUCT(addr) (((uword*)addr)[-1] & 1)
#define ALLOC_SIZE(addr) (((((uword*)addr)[-1]) << 32) >> 33)
#define REF_COUNT(addr) (((uword*)addr)[-1] >> 32)
#define INCR_REF_COUNT(addr) (((unsigned int*)addr)[-1] = ((unsigned int*)addr)[-1] + 1)
#define DECR_REF_COUNT(addr) (((unsigned int*)addr)[-1] = ((unsigned int*)addr)[-1] - 1)

extern byte* heap_min;
extern byte* heap_max;
extern byte* stack_base;

#define ON_HEAP(addr) (heap_min <= addr && addr <= heap_max)
#define ON_STACK(addr, sp) (stack_base <= addr && addr <= (stack_base + sp))

void memory_init(byte* sb);
byte* allocate_simple(byte type);
byte* allocate_struct(unsigned int fields);
void try_free(word* addr, uword sp, unsigned int depth, byte trace);
byte to_origin(word** target, uword sp);