#include "memory.h"

// Moves progresser from the start of the entry points section, to the start of the global variables section.
void find_global_vars_start(byte_t* ptr, byte_t** progresser);

// Moves progresser from the start of the global variables section, to the start of the instructions section.
void find_instruction_start(byte_t* ptr, byte_t** progresser);

full_t* find_entry_point(byte_t* ptr, char* routine);

void print_entry_points(char* ptr);