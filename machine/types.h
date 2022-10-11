#include "defs.h"

#define ADDR 0
#define BOOL 1
#define INT 2

char* type_name(char index);
byte type_size(char index);

word parse_int(char* str);

byte parse_bool(char* str);

byte* payload(word* addr);