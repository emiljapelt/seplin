
#include "defs.h"

#define REF_COUNT(addr) (((int*)addr)[-2])
#define ALLOC_SIZE(addr) (((int*)addr)[-1])

void memory_init();
byte* allocate(int size);
void try_free(byte* addr, uword sp);
byte on_heap(byte* addr);
byte on_stack(byte* addr);