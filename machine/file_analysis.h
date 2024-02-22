#include "memory.h"
#include <stddef.h>

typedef struct file_segments
{
    char* struct_segment;
    char* global_var_segment;
    char* entry_point_segment;
    byte_t* instructions_segment;
} file_segments;

typedef struct entry_point_info
{
    byte_t found;
    full_t instruction_addr;
    byte_t argument_count;
    byte_t* argument_types;
} entry_point_info;

char string_ends_with(size_t suffix_size, char* suffix, size_t size, char* string);

file_segments find_segments(byte_t* file);
entry_point_info find_entry_point(byte_t* ptr, char* name);
void print_entry_points(byte_t* ptr, byte_t* struct_seg);
void print_structs(byte_t* structs);
void print_global_vars(byte_t* ptr, byte_t* structs);
void dissas(byte_t* origin, byte_t* instr, full_t file_len);