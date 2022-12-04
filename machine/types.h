#include "memory.h"

#define ADDR 0
#define BOOL 1
#define INT 2

char* type_name(char index);
ufull_t parse_int(char* str);
byte_t parse_bool(char* str);