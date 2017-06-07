#include "bilby.h"

#include <rdparse/parser.h>

#include <stdio.h>
#include <string.h>

/*
 * bilby_file = ((definition | expression) '\n'+)*
 * 
 * expression = expr_0
 * expr_0 = expr_1 ['+' expr_1|'-' expr_1]*
 * expr_1 = expr_2 ['*' expr_2|'/' expr_2]*
 * expr_2 = expr_base
 * expr_base = <integer> | call | <identifier> | '(' expression ')'
 * call = expression expr_base
 * 
 * definition = <identifier> [match]* '=' expression
 * match = <integer> | <identifier>
 * 
 * note: fn a b = 5 is the same as fn = a -> b -> 5
 */

int DEBUG_X = 0;

typedef enum {
  EXPR_INT,
  EXPR_STRING,
  EXPR_IDENTIFIER,
  EXPR_FUNCTION,
  EXPR_NATIVE_FUNCTION,
  EXPR_CALL
} ExprType;

typedef struct {
  ExprType type;
} Expr;

typedef struct {
  const char *name;
  Expr *value;
} Definition;

struct DefinitionList;
typedef struct DefinitionList DefinitionList;
struct DefinitionList {
  Definition def;
  DefinitionList *prev;
};

struct Definitions {
  DefinitionList *listp;
};

Expr *make_int_expr(int i);
Expr *make_string_expr(const char *str);
Expr *make_identifier_expr(char *name);
// variadic functions need one normal parameter
Expr *make_call_expr(Expr *fn, Expr *arg);
Expr *make_call3_expr(Expr *fn, Expr *arg1, Expr *arg2) {
  return make_call_expr(make_call_expr(fn, arg1), arg2);
}
Expr *make_function_expr(Expr *match, Expr *value);
Expr *make_native_function_expr(const char *name, Expr* (*)(Definitions*,Expr*,Expr*));
void evaluate(Expr *expr, Definitions *defs, Expr ***exprs_ptr_p, int *exprs_len_p);
Expr *flatten(Expr *expr, Definitions *defs);
// fn x y = 5 is parsed by treating "fn x y" as an expression, ie. ((fn x) y)
// this function transforms it into fn = x -> y -> 5
Expr *transform_call_to_function_form(Expr *match_expr, Expr *value, const char **ident);

void dump_expr(Expr*);

void define(Definitions *defs, const char *name, Expr *value);

void parse_must(char *text, bool cond, char *msg, ...) {
  if (cond) return;
  va_list ap;
  va_start(ap, msg);
  vlog_parser_error(text, msg, ap);
  va_end(ap);
  abort();
}

bool parse_expr(char **text, Expr **expr);

bool parse_expr_base2(char **text, Expr **expr) {
  int i;
  if (parse_int(text, &i)) {
    *expr = make_int_expr(i);
    return true;
  }
  
  char *str;
  ParseResult res = parse_string(text, &str);
  if (res == PARSE_ERROR) abort();
  if (res == PARSE_OK) {
    *expr = make_string_expr(str);
    return true;
  }
  
  char *ident = parse_identifier(text);
  if (ident) {
    *expr = make_identifier_expr(ident);
    return true;
  }
  if (eat_string(text, "(")) {
    if (!parse_expr(text, expr)) {
      log_parser_error(*text, "Expression expected.");
      abort();
    }
    parse_must(*text, eat_string(text, ")"), "Expression expected.");
    return true;
  }
  return false;
}

bool parse_expr_base(char **text, Expr **expr) {
  if (!parse_expr_base2(text, expr)) return false;
  while (true) {
    Expr *arg;
    if (eat_string(text, "\n")) break;
    if (!parse_expr_base2(text, &arg)) break;
    *expr = make_call_expr(*expr, arg);
  }
  return true;
}

bool parse_expr2(char **text, Expr **expr) {
  return parse_expr_base(text, expr);
}

bool parse_expr1(char **text, Expr **expr) {
  if (!parse_expr2(text, expr)) return false;
  while (true) {
    if (eat_string(text, "*")) {
      Expr *expr2;
      parse_must(*text, parse_expr2(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("*"), *expr, expr2);
      continue;
    }
    if (eat_string(text, "/")) {
      Expr *expr2;
      parse_must(*text, parse_expr2(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("/"), *expr, expr2);
      continue;
    }
    return true;
  }
}

bool parse_expr0(char **text, Expr **expr) {
  if (!parse_expr1(text, expr)) return false;
  while (true) {
    if (eat_string(text, "+")) {
      Expr *expr2;
      parse_must(*text, parse_expr1(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("+"), *expr, expr2);
      continue;
    }
    if (eat_string(text, "-")) {
      Expr *expr2;
      parse_must(*text, parse_expr1(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("-"), *expr, expr2);
      continue;
    }
    return true;
  }
}

bool parse_expr(char **text, Expr **expr) {
  return parse_expr0(text, expr);
}

bool parse_tl_define(char **textp, Definitions *defs) {
  char *text = *textp;
  Expr *match_value;
  if (!parse_expr(&text, &match_value)) return false;
  if (!eat_string(&text, "=")) return false;
  
  Expr *value;
  parse_must(text, parse_expr(&text, &value), "Expected expression for definition value.");
  
  const char *ident;
  value = transform_call_to_function_form(match_value, value, &ident);
  
  define(defs, ident, value);
  
  *textp = text;
  return true;
}

void define(Definitions *defs, const char *name, Expr *value) {
  DefinitionList *prev = defs->listp;
  defs->listp = malloc(sizeof(DefinitionList));
  *defs->listp = (DefinitionList) {
    .prev = prev,
    .def = {
      .name = name,
      .value = value
    }
  };
}

void expr_array_init_capacity(int *capacity_p, int *exprs_len_p) {
  *capacity_p = *exprs_len_p;
  *exprs_len_p = 0;
}

void expr_array_push(Expr ***exprs_ptr_p, int *exprs_len_p, int *capacity_p, Expr *value) {
  (*exprs_len_p) ++;
  
  if (*exprs_len_p < *capacity_p + 1) {
  } else if (*exprs_len_p == *capacity_p + 1) {
    *capacity_p = -1;
    Expr **new_exprs_ptr_p = malloc(sizeof(Expr*) * *exprs_len_p);
    memcpy(new_exprs_ptr_p, *exprs_ptr_p, sizeof(Expr*) * (*exprs_len_p - 1));
    *exprs_ptr_p = new_exprs_ptr_p;
  } else {
    *exprs_ptr_p = realloc(*exprs_ptr_p, sizeof(Expr*) * *exprs_len_p);
  }
  
  (*exprs_ptr_p)[*exprs_len_p - 1] = value;
}

void expr_array_reset(Expr ***exprs_ptr_p, int *exprs_len_p, int *capacity_p,
                      Expr **exprs_ptr_start, int exprs_len_start) {
  if (*exprs_ptr_p != exprs_ptr_start) {
    free(*exprs_ptr_p);
    *exprs_ptr_p = exprs_ptr_start;
  }
  *exprs_len_p = exprs_len_start;
  expr_array_init_capacity(capacity_p, exprs_len_p);
}

void lookup(Definitions *defs, const char *name, Expr ***exprs_ptr_p, int *exprs_len_p) {
  int capacity;
  expr_array_init_capacity(&capacity, exprs_len_p);
  
  DefinitionList *listp = defs->listp;
  while (listp) {
    // fprintf(stderr, "? %s = %s\n", name, listp->def.name);
    if (strcmp(listp->def.name, name) == 0) {
      expr_array_push(exprs_ptr_p, exprs_len_p, &capacity, listp->def.value);
    }
    listp = listp->prev;
  }
}

void lookup_one(Definitions *defs, const char *name, Expr **expr_p, bool allow_none) {
  Expr **exprs_ptr = expr_p; int exprs_len = 1;
  lookup(defs, name, &exprs_ptr, &exprs_len);
  if (exprs_len == 0) {
    if (allow_none) {
      *expr_p = NULL;
      return;
    }
    fprintf(stderr, "identifier not found: %s\n", name);
    abort();
  }
  if (exprs_ptr != expr_p) free(exprs_ptr);
  /*if (exprs_len != 1) {
    fprintf(stderr, "multiple defs found for %s, single expected\n", name);
    abort();
  }*/
}

bool match(Expr *value, Expr *target, Definitions *defs, Definitions *newdefs,
           int *specificity_p, bool lexical);

bool parse_tl_expr(char **textp, Definitions *defs) {
  Expr *value;
  if (!parse_expr(textp, &value)) return false;
  Definitions newdefs = {0};
  int specificity;
  if (!match(value, make_identifier_expr("v"), defs, &newdefs, &specificity, false)) {
    fprintf(stderr, "Could not evaluate top-level expression.\n");
    abort();
  }
  Expr *expr;
  Expr **exprs_ptr = &expr; int exprs_len = 1;
  lookup(&newdefs, "v", &exprs_ptr, &exprs_len);
  if (exprs_len != 1) {
    fprintf(stderr, "Internal error.\n");
    abort();
  }
  
  expr = flatten(expr, defs);
  dump_expr(expr); fprintf(stderr, "\n");
  return true;
}

typedef struct {
  Expr base;
  int value;
} IntExpr;

typedef struct {
  Expr base;
  const char *str;
} StringExpr;

typedef struct {
  Expr base;
  const char *name;
} IdentifierExpr;

typedef struct {
  Expr base;
  Expr *fn;
  Expr *arg;
} CallExpr;

typedef struct {
  Expr base;
  Expr *match;
  Expr *value;
} FunctionExpr;

typedef struct {
  Expr base;
  const char *name;
  // TODO arity other than 2
  Expr* (*resolve_fn)(Definitions*, Expr *a, Expr *b);
  Expr *a, *b; // arguments
} NativeFunctionExpr;

Expr *make_int_expr(int i) {
  IntExpr *res = malloc(sizeof(IntExpr));
  res->base.type = EXPR_INT;
  res->value = i;
  return (Expr*) res;
}

Expr *make_string_expr(const char *str) {
  StringExpr *res = malloc(sizeof(StringExpr));
  res->base.type = EXPR_STRING;
  res->str = str;
  return (Expr*) res;
}

Expr *make_identifier_expr(char *name) {
  IdentifierExpr *res = malloc(sizeof(IdentifierExpr));
  res->base.type = EXPR_IDENTIFIER;
  res->name = name;
  return (Expr*) res;
}

Expr *make_call_expr(Expr *fn, Expr *arg) {
  if (fn->type == EXPR_INT) {
    abort();
  }
  
  CallExpr *res = malloc(sizeof(CallExpr));
  res->base.type = EXPR_CALL;
  res->fn = fn;
  res->arg = arg;
  return (Expr*) res;
}

Expr *make_function_expr(Expr *match, Expr *value) {
  FunctionExpr *res = malloc(sizeof(FunctionExpr));
  res->base.type = EXPR_FUNCTION;
  res->match = match;
  res->value = value;
  return (Expr*) res;
}

Expr *make_native_function_expr(const char *name, Expr *(*fn)(Definitions*,Expr*,Expr*)) {
  NativeFunctionExpr *res = malloc(sizeof(NativeFunctionExpr));
  res->base.type = EXPR_NATIVE_FUNCTION;
  res->name = name;
  res->resolve_fn = fn;
  res->a = NULL;
  res->b = NULL;
  return (Expr*) res;
}

Expr *transform_call_to_function_form(Expr *match_expr, Expr *value, const char **ident) {
  if (match_expr->type == EXPR_CALL) {
    CallExpr *match = (CallExpr*) match_expr;
    value = make_function_expr(match->arg, value);
    return transform_call_to_function_form(match->fn, value, ident);
  }
  if (match_expr->type == EXPR_IDENTIFIER) {
    IdentifierExpr *match = (IdentifierExpr*) match_expr;
    *ident = match->name;
    return value;
  }
  fprintf(stderr, "transform: "); dump_expr(match_expr); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  abort();
}

Definitions fork_definitions(Definitions *defs, Definitions *fallback) {
  if (defs->listp == NULL) {
    return (Definitions) {
      .listp = fallback->listp
    };
  }
  return (Definitions) {
    .listp = defs->listp
  };
}

void merge_definitions(Definitions *target, Definitions *src) {
  DefinitionList *listp = src->listp;
  while (listp) {
    define(target, listp->def.name, listp->def.value);
    listp = listp->prev;
  }
}

Expr *flatten(Expr *expr, Definitions *defs) {
  while (true) {
    /*fprintf(stderr, " - - -f-l-a-t-t-e-n- - -\n");
    dump_expr(expr);
    fprintf(stderr, "\n - - - - - - - - - - - -\n");*/
    
    Expr *expr2 = NULL;
    Expr **exprs2_ptr = &expr2; int exprs2_len = 1;
    evaluate(expr, defs, &exprs2_ptr, &exprs2_len);
    if (exprs2_len != 1) {
      fprintf(stderr, "Internal error.\n");
      abort();
    }
    if (expr2 == expr) break;
    expr = expr2;
  }
  return expr;
}

Expr *substitute(Expr *expr, Definitions *changes, Definitions *defs) {
  if (expr->type == EXPR_CALL) {
    CallExpr *expr_call = (CallExpr*) expr;
    return make_call_expr(
      substitute(expr_call->fn, changes, defs),
      substitute(expr_call->arg, changes, defs)
    );
  }
  if (expr->type == EXPR_INT || expr->type == EXPR_STRING) {
    return expr;
  }
  if (expr->type == EXPR_IDENTIFIER) {
    IdentifierExpr *expr_ident = (IdentifierExpr*) expr;
    
    Expr *expr_scrap[1];
    Expr **exprs_ptr = expr_scrap; int exprs_len = 1;
    lookup(changes, expr_ident->name, &exprs_ptr, &exprs_len);
    
    if (exprs_len == 0) return expr;
    else if (exprs_len == 1) {
      // fprintf(stderr, "substitute %s with ", expr_ident->name);
      // dump_expr(exprs_ptr[0]);
      // fprintf(stderr, "\n");
      return exprs_ptr[0];
    }
    else {
      fprintf(stderr, "logic error: ambiguous substitution!\n");
      abort();
    }
  }
  if (expr->type == EXPR_FUNCTION) {
    FunctionExpr *expr_fun = (FunctionExpr*) expr;
    // TODO raise a stink if this substitution collides with our running one,
    // or mask the current substitution.
    return make_function_expr(
      expr_fun->match,
      substitute(expr_fun->value, changes, defs)
    );
  }
  if (expr->type == EXPR_NATIVE_FUNCTION) {
    NativeFunctionExpr *expr_native_fn = (NativeFunctionExpr*) expr;
    Expr *a, *b;
    lookup_one(changes, "a", &a, true);
    lookup_one(changes, "b", &b, true);
    NativeFunctionExpr *expr_copy = (NativeFunctionExpr*) make_native_function_expr(
      expr_native_fn->name,
      expr_native_fn->resolve_fn
    );
    expr_copy->a = expr_native_fn->a;
    expr_copy->b = expr_native_fn->b;
    if (a && !expr_copy->a) expr_copy->a = a;
    if (b && !expr_copy->b) expr_copy->b = b;
    return (Expr*) expr_copy;
  }
  fprintf(stderr, "substitute: "); dump_expr(expr); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  abort();
}

void evaluate(Expr *expr, Definitions *defs, Expr ***exprs_ptr_p, int *exprs_len_p) {
  // fprintf(stderr, "--"); dump_expr(expr); fprintf(stderr, "\n");
  if (expr->type == EXPR_IDENTIFIER) {
    IdentifierExpr *ident_expr = (IdentifierExpr*) expr;
    
    lookup(defs, ident_expr->name, exprs_ptr_p, exprs_len_p);
    if (!*exprs_len_p) {
      fprintf(stderr, "Identifier %s not found.\n", ident_expr->name);
      abort();
    }
    return;
  }
  if (expr->type == EXPR_CALL) {
    CallExpr *call_expr = (CallExpr*) expr;
    Expr *fn_expr_scrap[1];
    Expr **fn_exprs_ptr = fn_expr_scrap; int fn_exprs_len = 1;
    
    evaluate(call_expr->fn, defs, &fn_exprs_ptr, &fn_exprs_len);
    
    Expr **exprs_ptr_start = *exprs_ptr_p;
    int exprs_len_start = *exprs_len_p;
    
    int capacity;
    expr_array_init_capacity(&capacity, exprs_len_p);
    
    Expr *arg = call_expr->arg;
    
    int best_specificity = 0;
    
    for (int i = 0; i < fn_exprs_len; i++) {
      Expr *fn_expr = fn_exprs_ptr[i];
      if (fn_expr->type != EXPR_FUNCTION) {
        fprintf(stderr, "cannot call: not a function, ");
        dump_expr(fn_expr);
        fprintf(stderr, "\n");
        abort();
      }
      FunctionExpr *fn = (FunctionExpr*) fn_expr;
      Definitions newdefs = {0};
      int specificity;
      if (match(arg, fn->match, defs, &newdefs, &specificity, false)) {
        Expr *new_value = substitute(fn->value, &newdefs, defs);
        if (specificity < best_specificity) continue;
        if (specificity > best_specificity) {
          expr_array_reset(exprs_ptr_p, exprs_len_p, &capacity,
                          exprs_ptr_start, exprs_len_start);
        }
        best_specificity = specificity;
        expr_array_push(exprs_ptr_p, exprs_len_p, &capacity, new_value);
        continue;
      }
    }
    if (best_specificity == 0) {
      /*fprintf(stderr, "!! calls matched no patterns - evaluate arg\n");
      for (int i = 0; i < fn_exprs_len; i++) {
        dump_expr(fn_exprs_ptr[i]); fprintf(stderr, "\n");
      }
      dump_expr(arg); fprintf(stderr, "\n");*/
      // if (DEBUG_X++ == 5) abort();
      
      Expr *args_expr_scrap[1];
      Expr **args_exprs_ptr = args_expr_scrap; int args_exprs_len = 1;
      evaluate(arg, defs, &args_exprs_ptr, &args_exprs_len);
      
      if (args_exprs_len != 1) {
        fprintf(stderr, "TODO WHY IS THIS HAPPENING\n");
        abort();
      }
      Expr *subarg = args_exprs_ptr[0];
      if (subarg != arg) {
        expr_array_reset(exprs_ptr_p, exprs_len_p, &capacity,
                        exprs_ptr_start, exprs_len_start);
        *exprs_len_p = exprs_len_start;
        
        Expr *subcall = make_call_expr(call_expr->fn, subarg);
        evaluate(subcall, defs, exprs_ptr_p, exprs_len_p);
        return;
      }
    }
    
    if (fn_exprs_ptr != fn_expr_scrap) free(fn_exprs_ptr);
    
    if (*exprs_len_p == 1) {
      return;
    }
    
    fprintf(stderr, "while evaluating ");
    dump_expr(expr);
    fprintf(stderr, ":\n");
    
    fprintf(stderr, "got %i exprs of %i\n", *exprs_len_p, best_specificity);
    
    for (int i = 0; i < *exprs_len_p; i++) {
      fprintf(stderr, " -- ");
      dump_expr((*exprs_ptr_p)[i]);
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "TODO\n");
    abort();
  }
  if (expr->type == EXPR_INT || expr->type == EXPR_STRING) {
    int capacity;
    expr_array_init_capacity(&capacity, exprs_len_p);
    expr_array_push(exprs_ptr_p, exprs_len_p, &capacity, expr);
    return;
  }
  if (expr->type == EXPR_NATIVE_FUNCTION) {
    NativeFunctionExpr *expr_native = (NativeFunctionExpr*) expr;
    Expr *res_expr = expr_native->resolve_fn(defs, expr_native->a, expr_native->b);
    
    int capacity;
    expr_array_init_capacity(&capacity, exprs_len_p);
    expr_array_push(exprs_ptr_p, exprs_len_p, &capacity, res_expr);
    return;
  }
  
  fprintf(stderr, "evaluate: "); dump_expr(expr); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  abort();
}

bool match(Expr *value, Expr *target, Definitions *defs, Definitions *newdefs,
           int *specificity_p, bool lexical) {
  // fprintf(stderr, "match: "); dump_expr(value); fprintf(stderr, "\n");
  // fprintf(stderr, "   to: "); dump_expr(target); fprintf(stderr, "\n");
  
  bool value_is_primitive = value->type == EXPR_INT || value->type == EXPR_STRING;
  bool target_is_primitive = target->type == EXPR_INT || target->type == EXPR_STRING;
  
  bool value_is_evaluable = value->type == EXPR_IDENTIFIER || value->type == EXPR_CALL;
  
  bool target_is_term = target_is_primitive || target->type == EXPR_IDENTIFIER;
  if (value_is_evaluable && target_is_primitive) {
    Expr *values_scrap[1];
    Expr **values_ptr = values_scrap; int values_len = 1;
    evaluate(value, defs, &values_ptr, &values_len);
    
    int num_matched = 0;
    for (int i = 0; i < values_len; i++) {
      if (match(values_ptr[0], target, defs, newdefs, specificity_p, false))
        num_matched ++;
    }
    if (values_ptr != values_scrap) free(values_ptr);
    
    if (num_matched == 0) return false;
    if (num_matched == 1) return true;
    
    fprintf(stderr, "TODO\n");
    abort();
  }
  
  if (value->type == EXPR_NATIVE_FUNCTION && target_is_term) {
    NativeFunctionExpr *value_native = (NativeFunctionExpr*) value;
    value = value_native->resolve_fn(defs, value_native->a, value_native->b);
    return match(value, target, defs, newdefs, specificity_p, false);
  }
  
  if (value->type == EXPR_INT && target->type == EXPR_INT) {
    if (((IntExpr*) value)->value != ((IntExpr*) target)->value) return false;
    *specificity_p = 2;
    return true;
  }
  
  if (lexical && value->type == EXPR_IDENTIFIER && target->type == EXPR_IDENTIFIER) {
    IdentifierExpr *value_ident = (IdentifierExpr*) value;
    IdentifierExpr *target_ident = (IdentifierExpr*) target;
    if (strcmp(value_ident->name, target_ident->name) != 0) return false;
    *specificity_p = 3;
    return true;
  }
  
  if (!lexical && target->type == EXPR_IDENTIFIER) {
    IdentifierExpr *target_ident = (IdentifierExpr*) target;
    define(newdefs, target_ident->name, value);
    *specificity_p = 1;
    return true;
  }
  
  if (value->type == EXPR_CALL && target->type == EXPR_CALL) {
    CallExpr *value_call = (CallExpr*) value;
    CallExpr *target_call = (CallExpr*) target;
    int subspecificity;
    if (!match(value_call->fn, target_call->fn, defs, newdefs, &subspecificity, true)) return false;
    if (!match(value_call->arg, target_call->arg, defs, newdefs, &subspecificity, false)) return false;
    *specificity_p = 4 + subspecificity;
    return true;
  }
  
  if (value->type == EXPR_IDENTIFIER && target->type == EXPR_CALL) {
    Expr *values_scrap[1];
    Expr **values_ptr = values_scrap; int values_len = 1;
    evaluate(value, defs, &values_ptr, &values_len);
    
    int best_specificity = 0;
    Definitions best_defs_HACK = {0};
    
    Expr *matches_scrap[1];
    Expr **matches_ptr = matches_scrap; int matches_len = 1;
    int matches_capacity;
    expr_array_init_capacity(&matches_capacity, &matches_len);
    
    for (int i = 0; i < values_len; i++) {
      Definitions newdefs2 = {0};
      int subspecificity;
      if (match(values_ptr[i], target, defs, &newdefs2, &subspecificity, false)) {
        if (subspecificity < best_specificity) continue;
        if (subspecificity > best_specificity) {
          expr_array_reset(&matches_ptr, &matches_len, &matches_capacity,
                           matches_scrap, 1);
        }
        best_specificity = subspecificity;
        best_defs_HACK = newdefs2;
        expr_array_push(&matches_ptr, &matches_len, &matches_capacity, values_ptr[i]);
        continue;
      }
    }
    
    if (matches_len == 0) return false;
    
    if (matches_len == 1) {
      merge_definitions(newdefs, &best_defs_HACK);
      return matches_ptr[0];
    }
    fprintf(stderr, "uuuhh %i\n", matches_len);
  }
  
  if (value_is_primitive && target->type == EXPR_CALL) {
    return false;
  }
  
  if (value->type == EXPR_FUNCTION/* && target->type == EXPR_CALL*/) {
    return false;
  }
  
  if (lexical && value->type == EXPR_CALL && target->type == EXPR_IDENTIFIER) {
    return false;
  }
  
  if (value->type == EXPR_NATIVE_FUNCTION && target->type == EXPR_CALL) {
    return false;
  }
  
  fprintf(stderr, "match: "); dump_expr(value); fprintf(stderr, "\n");
  fprintf(stderr, "   to: "); dump_expr(target); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  abort();
}

/*
 * fib 0 = 1
 * function: "fib" [match 0] = <int 1>
 * fib 1 = 1
 * function: "fib" [match 1] = <int 1>
 * fib n = fib (n - 1) + fib (n - 2)
 * function: "fib" [match bind 'n'] =
 *    <call> (<identifier '+'>,
 *      <call> (<identifier fib>,
 *        <call> (<identifier '-'>, <identifier n>, <int 1>)
 *      ),
 *      <call> (<identifier fib>,
 *        <call> (<identifier '-'>, <identifier n>, <int 2>)
 *      )
 *    )
 * ??
 * 
 * okay..
 * 
 * start: (fib 5)
 * 1 match
 * full match, rewrite n=5 => (+ (fib (- 5 1)) (fib (- 5 2)))
 * 1 match
 * partial match, recurse against %1: (fib (- 5 1))
 *  partial match, recurse against n: (- 5 1)
 *   1 match
 *   full match, yield 4
 *  (fib 4)
 *  full match, rewrite n=4 => (+ (fib (- 4 1)) (fib (- 4 2)))
 * etc.
 */

void dump_expr(Expr *expr) {
  if (expr->type == EXPR_INT) {
    fprintf(stderr, "%i", ((IntExpr*)expr)->value);
  } else if (expr->type == EXPR_STRING) {
    fprintf(stderr, "'%s'", ((StringExpr*)expr)->str);
  } else if (expr->type == EXPR_IDENTIFIER) {
    fprintf(stderr, "%s", ((IdentifierExpr*)expr)->name);
  } else if (expr->type == EXPR_CALL) {
    CallExpr *expr_call = (CallExpr*) expr;
    dump_expr(expr_call->fn);
    fprintf(stderr, " (");
    dump_expr(expr_call->arg);
    fprintf(stderr, ")");
  } else if (expr->type == EXPR_FUNCTION) {
    FunctionExpr *expr_fn = (FunctionExpr*) expr;
    dump_expr(expr_fn->match);
    fprintf(stderr, " -> (");
    dump_expr(expr_fn->value);
    fprintf(stderr, ")");
  } else if (expr->type == EXPR_NATIVE_FUNCTION) {
    NativeFunctionExpr *expr_native = (NativeFunctionExpr*) expr;
    if (expr_native->a && expr_native->b) {
      fprintf(stderr, "%s (", expr_native->name);
      dump_expr(expr_native->a);
      fprintf(stderr, ") (");
      dump_expr(expr_native->b);
      fprintf(stderr, ")");
    } else {
      fprintf(stderr, "<native %s>", expr_native->name);
    }
  } else abort();
}

bool eat_bilby_filler(char **textp) {
  while (**textp) {
    if (starts_with(textp, ";")) {
      while (**textp && **textp != '\n') (*textp)++;
    }
    else if (**textp == ' ') (*textp)++;
    else break;
  }
  return true;
}

Expr *add_fn(Definitions *defs, Expr *a, Expr *b) {
  // fprintf(stderr, "[flatten a]\n");
  a = flatten(a, defs);
  // fprintf(stderr, "[flatten b]\n");
  b = flatten(b, defs);
  
  if (a->type == EXPR_STRING && b->type == EXPR_INT) {
    char *bla = malloc(16);
    snprintf(bla, 16, "%i", ((IntExpr*) b)->value);
    b = make_string_expr(bla);
  }
  
  if (a->type == EXPR_STRING && b->type == EXPR_STRING) {
    StringExpr *a_str = (StringExpr*) a;
    StringExpr *b_str = (StringExpr*) b;
    char *combined = malloc(strlen(a_str->str) + strlen(b_str->str) + 1);
    combined[0] = 0;
    strcat(combined, a_str->str);
    strcat(combined, b_str->str);
    return make_string_expr(combined);
  }
  if (a->type != EXPR_INT || b->type != EXPR_INT) {
    fprintf(stderr, "internal error: invalid merge for native function\n");
    abort();
  }
  return make_int_expr(((IntExpr*) a)->value + ((IntExpr*) b)->value);
}

Expr *sub_fn(Definitions *defs, Expr *a, Expr *b) {
  a = flatten(a, defs);
  b = flatten(b, defs);
  
  if (a->type != EXPR_INT || b->type != EXPR_INT) {
    fprintf(stderr, "internal error: invalid merge for native function\n");
    abort();
  }
  return make_int_expr(((IntExpr*) a)->value - ((IntExpr*) b)->value);
}

void setup_runtime(Definitions *defs) {
  define(defs, "+", make_function_expr(make_identifier_expr("a"),
    make_function_expr(make_identifier_expr("b"),
      make_native_function_expr("add_fn", add_fn)
    )
  ));
  
  define(defs, "-", make_function_expr(make_identifier_expr("a"),
    make_function_expr(make_identifier_expr("b"),
      make_native_function_expr("sub_fn", sub_fn)
    )
  ));
}

bool parse_tl(char **textp, Definitions *defs) {
  if (parse_tl_define(textp, defs)) return true;
  if (parse_tl_expr(textp, defs)) return true;
  return false;
}

Definitions *alloc_definition_set() {
  Definitions *res = malloc(sizeof(Definitions));
  *res = (Definitions) {0};
  return res;
}
