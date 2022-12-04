#include "memory.h"

#define ADDR 0
#define INT 1
#define BOOL 2
#define CHAR 3

char* type_name(char index);
ufull_t parse_int(char* str);
byte_t parse_bool(char* str);
byte_t parse_char(char* str);