#include "defs.h"

#define I 0
#define DISASS 1
#define RUN 3

byte command_index(char* command);

#define UNKNOWN_FLAG 0
#define TRACE 1
#define TIME 2

byte flag_index(char* flag);