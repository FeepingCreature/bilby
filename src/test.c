#include "xfp.h"

#include <rdparse/parser.h>
#include <stdio.h>

int main() {
  set_user_clean_fn(eat_xfp_filler);
  
  TextRange test = readfile("test.xfp");
  register_file(test, "test.xfp", 0, 0);
  
  Definitions *defs = alloc_definition_set();
  setup_runtime(defs);
  
  char *source = test.start;
  while (true) {
    if (parse_tl(&source, defs)) continue;
    if (eat_string(&source, "\n")) continue;
    if (source == test.end) break;
    log_parser_error(source, "Unexpected input.");
    abort();
  }
  fprintf(stderr, "Parse complete\n");
  return 0;
}
