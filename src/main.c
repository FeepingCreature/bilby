#include "bilby.h"

#include <rdparse/parser.h>
#include <stdio.h>

int main(int argc, const char **argv) {
  set_user_clean_fn(eat_bilby_filler);
  
  if (argc == 1) {
    fprintf(stderr, "Usage: %s <file.bb>\n", argv[0]);
    return 1;
  }
  
  for (int i = 1; i < argc; i++) {
    const char *filename = argv[i];
    TextRange filerange = readfile((char*) filename);
    register_file(filerange, filename, 0, 0);
    
    Definitions *defs = alloc_definition_set();
    Environment env = {
      .defs = defs,
      .verbose = false
    };
    setup_runtime(&env);
    
    char *source = filerange.start;
    while (true) {
      if (parse_tl(&source, &env)) continue;
      if (eat_string(&source, "\n")) continue;
      if (source == filerange.end) break;
      log_parser_error(source, "Unexpected input.");
      fail();
    }
  }
  return 0;
}

