#ifndef INPUT_H
#define INPUT_H

#include "memory.h"

void read_input(unsigned int max_size, char** ret);
ufull_t parse_int(char* str);
byte_t parse_bool(char* str);
byte_t parse_char(char* str);
byte_t* load_simple_argument(char type, char* arg);

#endif