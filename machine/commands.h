#include "defs.h"

#define I 0
#define DISASS 1
#define RUN 3
#define HELP 4

byte command_index(char* command);

#define TRACE 0
#define TIME 1

byte is_flag(char* flag);
byte flag_index(char* flag);