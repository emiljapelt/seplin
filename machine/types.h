#include "memory.h"

#ifndef TYPES_H
#define TYPES_H

#define INT 0
#define BOOL 1
#define CHAR 2
#define ARRAY 3
#define STRUCT 4
#define GENERIC 5
#define ROUTINE 6

char* type_name(byte_t index);

#endif