#include <string.h>

#include "commands.h"
#include "defs.h"

byte command_index(char* command) {
    if (strcmp(command, "i") == 0) return I;
    else if (strcmp(command, "disass") == 0) return DISASS;
    else if (strcmp(command, "debug") == 0) return DEBUG;
    else if (strcmp(command, "run") == 0) return RUN;
}