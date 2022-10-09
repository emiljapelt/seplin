#include "defs.h"

// Moves progresser from the start of the entry points section, to the start of the global variables section.
void find_global_vars_start(byte* ptr, byte** progresser);

// Moves progresser from the start of the global variables section, to the start of the instructions section.
void find_instruction_start(byte* ptr, byte** progresser);

word* find_entry_point(byte* ptr, char* routine);

void print_entry_points(char* ptr);