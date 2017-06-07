#ifndef XFP_H
#define XFP_H

#include <rdparse/util.h>

typedef struct Definitions Definitions;

Definitions *alloc_definition_set();

void setup_runtime(Definitions *defs);

bool parse_tl(char **textp, Definitions *defs);

bool eat_xfp_filler(char **textp);

#endif
