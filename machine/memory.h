#ifndef MEMORY_H
#define MEMORY_H

#define true 1
#define false 0

#define FULL 0
#define HALF 1
#define SHORT 2
#define BYTE 3

#define STACKSIZE 80000

#if _WIN64 || __x86_64__ || __ppc64__
    #define ENV64
#else
    #define ENV32
    #error "Error: The Seplin virtual machine does not yet support 32bit architectures."
#endif

#if defined(ENV64)
    typedef signed long long int full_t;
    typedef unsigned long long int ufull_t;
    typedef signed int half_t;
    typedef unsigned int uhalf_t;
    typedef signed short int short_t;
    typedef unsigned short int ushort_t;
    typedef signed char byte_t;
    typedef unsigned char ubyte_t;

    #define SIZE(t) (((t) == FULL) ? 8 : ((t) == HALF) ? 4 : ((t) == SHORT) ? 2 : 1)

    #define IS_STRUCT(addr) (((uhalf_t*)addr)[-1] & 1)
    #define ALLOC_SIZE(addr) ((((uhalf_t*)addr)[-1]) >> 1)
    #define REF_COUNT(addr) (((ufull_t*)addr)[-2])
    #define INCR_REF_COUNT(addr) (((uhalf_t*)addr)[-2] = ((uhalf_t*)addr)[-2] + 1)
    #define DECR_REF_COUNT(addr) (((uhalf_t*)addr)[-2] = ((uhalf_t*)addr)[-2] - 1)
#elif defined(ENV32)
    #error "Error: The Seplin virtual machine does not yet support 32bit architectures."
#else
    #error "Error: Could not resolve architecture size (32/64bit)."
#endif

extern byte_t* heap_min;
extern byte_t* heap_max;
extern byte_t* stack_base;

#define ON_HEAP(addr) (heap_min <= ((byte_t*)addr) && ((byte_t*)addr) <= heap_max)
#define ON_STACK(addr, sp) (stack_base <= ((byte_t*)addr) && ((byte_t*)addr) <= (stack_base + sp))

void memory_init(byte_t** stack);
byte_t* allocate_simple(byte_t type);
byte_t* allocate_struct(unsigned int fields);
void try_free(full_t* addr, ufull_t sp, unsigned int depth, byte_t trace);
byte_t to_origin(full_t** target, ufull_t sp);

#endif