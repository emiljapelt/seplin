#include "memory.h"

#define I 0
#define DISASS 1
#define RUN 3
#define HELP 4

byte_t command_index(char* command);

#define TRACE 0
#define TIME 1

byte_t is_flag(char* flag);
byte_t flag_index(char* flag);