#include "bilby.h"

#include <rdparse/parser.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

int main() {
  set_user_clean_fn(eat_bilby_filler);
  
  Definitions *defs = alloc_definition_set();
  Environment env = {
    .defs = defs,
    .verbose = false
  };
  setup_runtime(&env);
  
  while (true) {
    char *line = readline("> ");
    if (parse_tl(&line, &env)) continue;
    if (eat_string(&line, "\n")) continue;
    log_parser_error(line, "Unexpected input.");
    abort();
  }
  fprintf(stderr, "Parse complete\n");
  return 0;
}
