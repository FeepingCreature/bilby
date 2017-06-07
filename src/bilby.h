#ifndef BILBY_H
#define BILBY_H

#include <rdparse/util.h>

typedef struct Definitions Definitions;

Definitions *alloc_definition_set();

void setup_runtime(Definitions *defs);

bool parse_tl(char **textp, Definitions *defs);

bool eat_bilby_filler(char **textp);

void fail() __attribute__((noreturn));

#endif
