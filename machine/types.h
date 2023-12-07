#include "memory.h"

#define INT 0
#define BOOL 1
#define CHAR 2
#define ARRAY 3
#define STRUCT 4
#define GENERIC 5
#define ROUTINE 6

char* type_name(char index);
ufull_t parse_int(char* str);
byte_t parse_bool(char* str);
byte_t parse_char(char* str);

byte_t* load_simple_argument(char type, char* arg);