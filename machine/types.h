#include "defs.h"

#define ADDR 0
#define BOOL 1
#define INT 2

#define TYPE_SIZE(t) (((t) == ADDR || (t) == INT) ? 8 : ((t) == BOOL) ? 1 : 0)

char* type_name(char index);
byte* allocate_type(byte type);
word parse_int(char* str);
byte parse_bool(char* str);