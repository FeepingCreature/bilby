#include "bilby.h"

#include <rdparse/parser.h>

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

bool verbose = false;

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
Expr *make_call_expr_from_list(ExprList *fn, ExprList *arg);
Expr *make_call3_expr(Expr *fn, Expr *arg1, Expr *arg2) {
  return make_call_expr(make_call_expr(fn, arg1), arg2);
}
Expr *make_undefined_expr();
Expr *make_function_expr(Expr *match, Expr *value);
Expr *make_function_expr_from_list(Expr *match, ExprList *value);
Expr *make_native_function_expr(const char *name, Expr* (*)(Definitions*,ExprList*,ExprList*));
void evaluate(Expr *expr, Definitions *defs, Expr ***exprs_ptr_p, int *exprs_len_p);
Expr *flatten(Expr *expr, Definitions *defs);
// fn x y = 5 is parsed by treating "fn x y" as an expression, ie. ((fn x) y)
// this function transforms it into fn = x -> y -> 5
Expr *transform_call_to_function_form(Expr *match_expr, Expr *value, const char **ident);

void dump_expr(Expr*);

void define(Definitions *defs, const char *name, Expr *value, bool may_repeat);

void fail() {
#ifdef __AFL_COMPILER
  exit(0);
#else
  abort();
#endif
}

ExprList *list_from_expr(Expr *expr) {
  Expr **ptr = malloc(sizeof(Expr*) * 1);
  ptr[0] = expr;
  ExprList *list = malloc(sizeof(ExprList));
  *list = (ExprList) {
    .ptr = ptr,
    .len = 1,
    .next_evaluate = 0
  };
  return list;
}

ExprList *list_clone(ExprList *list) {
  ExprList *newlist = malloc(sizeof(ExprList));
  newlist->ptr = malloc(list->len * sizeof(Expr*));
  memcpy(newlist->ptr, list->ptr, list->len * sizeof(Expr*));
  newlist->len = list->len;
  newlist->next_evaluate = list->next_evaluate;
  return newlist;
}

Expr *get_single_expr(ExprList *list) {
  if (list->len != 1) {
    fprintf(stderr, "Internal error. %i.\n", list->len);
    dump_expr_list(list);
    fprintf(stderr, "\n");
    abort();
  }
  return list->ptr[0];
}

void dump_expr_list(ExprList *list) {
  if (list->len != 1) {
    fprintf(stderr, "[");
  }
  for (int i = 0; i < list->len; i++) {
    if (i > 0) fprintf(stderr, ", ");
    dump_expr(list->ptr[i]);
  }
  if (list->len != 1) {
    fprintf(stderr, "]");
  }
}

void parse_must(char *text, bool cond, char *msg, ...) {
  if (cond) return;
  va_list ap;
  va_start(ap, msg);
  vlog_parser_error(text, msg, ap);
  va_end(ap);
  fail();
}

bool parse_expr(char **text, Expr **expr);

bool parse_expr_base2(char **textp, Expr **expr) {
  int i;
  if (parse_int(textp, &i)) {
    *expr = make_int_expr(i);
    return true;
  }
  
  char *str;
  ParseResult res = parse_string(textp, &str);
  if (res == PARSE_ERROR) fail();
  if (res == PARSE_OK) {
    *expr = make_string_expr(str);
    return true;
  }
  
  {
    bool force_lexical = false, force_dynamic = false;
    char *text2 = *textp;
    if (eat_string(&text2, "%")) force_lexical = true;
    else if (eat_string(&text2, "$")) force_dynamic = true;
    char *ident = parse_identifier(&text2);
    if (ident) {
      *expr = make_identifier_expr(ident);
      ((IdentifierExpr*) *expr)->force_lexical = force_lexical;
      ((IdentifierExpr*) *expr)->force_dynamic = force_dynamic;
      *textp = text2;
      return true;
    }
  }
  
  if (eat_string(textp, "(")) {
    if (!parse_expr(textp, expr)) {
      log_parser_error(*textp, "Expression expected.");
      fail();
    }
    parse_must(*textp, eat_string(textp, ")"), "Expression expected.");
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

bool parse_expr3(char **text, Expr **expr) {
  return parse_expr_base(text, expr);
}

bool parse_expr2(char **text, Expr **expr) {
  if (!parse_expr3(text, expr)) return false;
  while (true) {
    if (eat_string(text, "*")) {
      Expr *expr2;
      parse_must(*text, parse_expr3(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("*"), *expr, expr2);
      continue;
    }
    if (eat_string(text, "/")) {
      Expr *expr2;
      parse_must(*text, parse_expr3(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("/"), *expr, expr2);
      continue;
    }
    return true;
  }
}

bool parse_expr1(char **text, Expr **expr) {
  if (!parse_expr2(text, expr)) return false;
  while (true) {
    if (eat_string(text, "+")) {
      Expr *expr2;
      parse_must(*text, parse_expr2(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("+"), *expr, expr2);
      continue;
    }
    if (eat_string(text, "-")) {
      Expr *expr2;
      parse_must(*text, parse_expr2(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("-"), *expr, expr2);
      continue;
    }
    return true;
  }
}

bool parse_expr0(char **text, Expr **expr) {
  if (!parse_expr1(text, expr)) return false;
  while (true) {
    if (eat_string(text, "<")) {
      Expr *expr2;
      parse_must(*text, parse_expr1(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("<"), *expr, expr2);
      continue;
    }
    if (eat_string(text, "==")) {
      Expr *expr2;
      parse_must(*text, parse_expr1(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr("=="), *expr, expr2);
      continue;
    }
    if (eat_string(text, ">")) {
      Expr *expr2;
      parse_must(*text, parse_expr1(text, &expr2), "Expression expected");
      *expr = make_call3_expr(make_identifier_expr(">"), *expr, expr2);
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
  
  Expr *value;
  if (eat_string(&text, ".")) {
    value = make_undefined_expr();
  } else if (eat_string(&text, "=")) {
    parse_must(text, parse_expr(&text, &value), "Expected expression for definition value.");
  } else return false;
  
  const char *ident;
  value = transform_call_to_function_form(match_value, value, &ident);
  
  define(defs, ident, value, true);
  
  *textp = text;
  return true;
}

void define(Definitions *defs, const char *name, Expr *value, bool may_repeat) {
  if (!may_repeat) {
    DefinitionList *cursor = defs->listp;
    while (cursor) {
      if (strcmp(cursor->def.name, name) == 0) {
        fprintf(stderr, "attempt to define redundant name: %s\n", name);
        abort();
      }
      cursor = cursor->prev;
    }
  }
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

void expr_list_push(ExprList *list, Expr *value) {
  list->ptr = realloc(list->ptr, sizeof(Expr*) * ++list->len);
  list->ptr[list->len - 1] = value;
}

void expr_array_push_list(Expr ***exprs_ptr_p, int *exprs_len_p, int *capacity_p, ExprList *values) {
  for (int i = 0; i < values->len; i++) {
    expr_array_push(exprs_ptr_p, exprs_len_p, capacity_p, values->ptr[i]);
  }
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
    fail();
  }
  if (exprs_ptr != expr_p) free(exprs_ptr);
  /*if (exprs_len != 1) {
    fprintf(stderr, "multiple defs found for %s, single expected\n", name);
    fail();
  }*/
}

bool match(Expr *value, Expr *target, Definitions *defs, Definitions *newdefs,
           bool lexical, bool direct);

bool parse_tl_expr(char **textp, Definitions *defs) {
  Expr *value;
  if (!parse_expr(textp, &value)) return false;
  Definitions newdefs = {0};
  if (!match(value, make_identifier_expr("v"), defs, &newdefs, false, false)) {
    fprintf(stderr, "Could not evaluate top-level expression.\n");
    fail();
  }
  Expr *expr;
  Expr **exprs_ptr = &expr; int exprs_len = 1;
  lookup(&newdefs, "v", &exprs_ptr, &exprs_len);
  if (exprs_len != 1) {
    fprintf(stderr, "Internal error.\n");
    fail();
  }
  
  expr = flatten(expr, defs);
  dump_expr(expr); fprintf(stderr, "\n");
  return true;
}

Expr *make_int_expr(int i) {
  IntExpr *res = malloc(sizeof(IntExpr));
  res->base.type = EXPR_INT;
  res->value = i;
  return (Expr*) res;
}

Expr *make_undefined_expr() {
  Expr *res = malloc(sizeof(Expr));
  res->type = EXPR_UNDEFINED;
  return res;
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
  res->force_lexical = false;
  res->force_dynamic = false;
  return (Expr*) res;
}

Expr *make_call_expr(Expr *fn, Expr *arg) {
  if (fn->type == EXPR_INT) {
    fail();
  }
  
  return make_call_expr_from_list(list_from_expr(fn), list_from_expr(arg));
}

Expr *make_call_expr_from_list(ExprList *fn, ExprList *arg) {
  CallExpr *res = malloc(sizeof(CallExpr));
  res->base.type = EXPR_CALL;
  res->fn = fn;
  res->arg = arg;
  return (Expr*) res;
}

Expr *make_function_expr(Expr *match, Expr *value) {
  return make_function_expr_from_list(match, list_from_expr(value));
}

Expr *make_function_expr_from_list(Expr *match, ExprList *value) {
  FunctionExpr *res = malloc(sizeof(FunctionExpr));
  res->base.type = EXPR_FUNCTION;
  res->match = match;
  res->value = value;
  return (Expr*) res;
}

Expr *make_native_function_expr(const char *name, Expr *(*fn)(Definitions*,ExprList*,ExprList*)) {
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
    value = make_function_expr(get_single_expr(match->arg), value);
    return transform_call_to_function_form(get_single_expr(match->fn), value, ident);
  }
  if (match_expr->type == EXPR_IDENTIFIER) {
    IdentifierExpr *match = (IdentifierExpr*) match_expr;
    *ident = match->name;
    return value;
  }
  fprintf(stderr, "transform: "); dump_expr(match_expr); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  fail();
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
    define(target, listp->def.name, listp->def.value, false);
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
    if (exprs2_len == 0) return make_undefined_expr();
    if (exprs2_len != 1) {
      fprintf(stderr, "Internal error.\n");
      fail();
    }
    if (expr2 == expr) break;
    expr = expr2;
  }
  return expr;
}

ExprList *substitute_in_list(ExprList *list, Definitions *changes, Definitions *defs);

Expr *substitute_in_expr(Expr *expr, Definitions *changes, Definitions *defs) {
  if (expr->type == EXPR_CALL) {
    CallExpr *expr_call = (CallExpr*) expr;
    return make_call_expr_from_list(
      substitute_in_list(expr_call->fn, changes, defs),
      substitute_in_list(expr_call->arg, changes, defs)
    );
  }
  if (expr->type == EXPR_INT || expr->type == EXPR_STRING || expr->type == EXPR_UNDEFINED) {
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
      fprintf(stderr, "logic error: ambiguous substitution! (%s)\n", expr_ident->name);
      for (int i = 0; i < exprs_len; i++) {
        fprintf(stderr, "%i: ", i);
        dump_expr(exprs_ptr[i]);
        fprintf(stderr, "\n");
      }
      fail();
    }
  }
  if (expr->type == EXPR_FUNCTION) {
    FunctionExpr *expr_fun = (FunctionExpr*) expr;
    // TODO raise a stink if this substitution collides with our running one,
    // or mask the current substitution.
    return make_function_expr_from_list(
      expr_fun->match,
      substitute_in_list(expr_fun->value, changes, defs)
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
    if (a && !expr_copy->a) expr_copy->a = list_from_expr(a);
    if (b && !expr_copy->b) expr_copy->b = list_from_expr(b);
    return (Expr*) expr_copy;
  }
  fprintf(stderr, "substitute: "); dump_expr(expr); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  fail();
}

int indent_depth;

ExprList *substitute_in_list(ExprList *list, Definitions *changes, Definitions *defs) {
  ExprList *res = list_clone(list);
  for (int i = 0; i < res->len; i++) {
    res->ptr[i] = substitute_in_expr(res->ptr[i], changes, defs);
  }
  return res;
}

int get_specificity(Expr *expr, bool force_lexical);

int get_match_specificity(Expr *match_expr) {
  if (match_expr->type == EXPR_IDENTIFIER) {
    IdentifierExpr *ident = (IdentifierExpr*) match_expr;
    if (ident->force_lexical) return 2;
    return 1;
  }
  if (match_expr->type == EXPR_INT || match_expr->type == EXPR_STRING) return 2;
  if (match_expr->type == EXPR_CALL) {
    CallExpr *call = (CallExpr*) match_expr;
    // TODO: ??
    return 4
      + get_match_specificity(get_single_expr(call->fn))
      + get_match_specificity(get_single_expr(call->arg));
  }
  fprintf(stderr, "%*sget_specificity match: ", indent_depth, ""); dump_expr(match_expr); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  fail();
}

int get_specificity(Expr *expr, bool force_lexical) {
  if (expr->type == EXPR_FUNCTION) {
    FunctionExpr *fn = (FunctionExpr*) expr;
    Expr *match_expr = fn->match;
    return get_match_specificity(match_expr);
  }
  
  if (expr->type == EXPR_IDENTIFIER) {
    IdentifierExpr *iexpr = (IdentifierExpr*) expr;
    if (iexpr->force_lexical) force_lexical = true;
    if (iexpr->force_dynamic) force_lexical = false;
    if (force_lexical) return 2; // literal match
    else return 1; // lowest specificity
  }
  
  fprintf(stderr, "%*sget_specificity %i: ", indent_depth, "", expr->type); dump_expr(expr); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  fail();
}

void flatten_list(ExprList *list, Definitions *defs) {
  Expr *new_exprs_scrap[1];
  Expr **new_exprs_ptr = new_exprs_scrap; int new_exprs_len = 1;
  int capacity = 1;
  expr_array_init_capacity(&capacity, &new_exprs_len);
  while (list->next_evaluate != list->len) {
    Expr *expr = list->ptr[list->next_evaluate++];
    
    expr_array_reset(&new_exprs_ptr, &new_exprs_len, &capacity,
                     new_exprs_scrap, new_exprs_len);
    evaluate(expr, defs, &new_exprs_ptr, &new_exprs_len);
    for (int i = 0; i < new_exprs_len; i++) {
      bool dupe = false;
      for (int k = 0; k < list->len; k++) {
        if (list->ptr[k] == new_exprs_ptr[i]) {
          dupe = true;
          break;
        }
      }
      if (dupe) continue;
      expr_list_push(list, new_exprs_ptr[i]);
    }
  }
}

void evaluate(Expr *expr, Definitions *defs, Expr ***exprs_ptr_p, int *exprs_len_p) {
  if (verbose) {
    fprintf(stderr, "%*seval : ", indent_depth, ""); dump_expr(expr); fprintf(stderr, "\n");
  }
  if (expr->type == EXPR_IDENTIFIER) {
    IdentifierExpr *ident_expr = (IdentifierExpr*) expr;
    
    lookup(defs, ident_expr->name, exprs_ptr_p, exprs_len_p);
    if (!*exprs_len_p) {
      fprintf(stderr, "Identifier %s not found.\n", ident_expr->name);
      fail();
    }
    return;
  }
  if (expr->type == EXPR_CALL) {
    indent_depth ++;
    CallExpr *call_expr = (CallExpr*) expr;
    
    // ExprList *fn_list = list_clone(call_expr->fn);
    // fprintf(stderr, "%*sflatten functions: ", indent_depth, ""); dump_expr_list(call_expr->fn); fprintf(stderr, "\n");
    ExprList *fn_list = call_expr->fn;
    // int prev_len = fn_list->len;
    flatten_list(fn_list, defs);
    /*if (fn_list->len != prev_len) {
      fprintf(stderr, "%*s => : ", indent_depth, ""); dump_expr_list(call_expr->fn); fprintf(stderr, "\n");
    }*/
    
    int capacity;
    expr_array_init_capacity(&capacity, exprs_len_p);
    
    int highest_specificity = 0;
    int *spec_p = calloc(sizeof(int), fn_list->len);
    for (int i = 0; i < fn_list->len; i++) {
      Expr *fn_expr = fn_list->ptr[i];
      if (fn_expr->type != EXPR_FUNCTION) {
        continue;
      }
      
      int spec = get_specificity(fn_list->ptr[i], false);
      spec_p[i] = spec;
      if (spec > highest_specificity) highest_specificity = spec;
    }
    
    ExprList *arg_list = call_expr->arg;
    
    for (int s = highest_specificity; s >= 0; s--) {
      int num_matched = 0;
      for (int i = 0; i < fn_list->len; i++) {
        if (spec_p[i] != s) continue;
        
        Expr *fn_expr = fn_list->ptr[i];
        if (fn_expr->type != EXPR_FUNCTION) {
          continue;
          /*fprintf(stderr, "cannot call: not a function, ");
          dump_expr(fn_expr);
          fprintf(stderr, "\n");
          fail();*/
        }
        FunctionExpr *fn = (FunctionExpr*) fn_expr;
        
        for (int cur_arg = 0; cur_arg < arg_list->len; cur_arg++) {
          Expr *arg = arg_list->ptr[cur_arg];
          Definitions newdefs = {0};
          if (match(arg, fn->match, defs, &newdefs, false, true)) {
            ExprList *new_values = substitute_in_list(fn->value, &newdefs, defs);
            expr_array_push_list(exprs_ptr_p, exprs_len_p, &capacity, new_values);
            num_matched ++;
            break;
          }
          
          if (cur_arg < arg_list->len - 1) continue; // still got cached args to go
          
          if (arg_list->next_evaluate == arg_list->len) break; // have evaluated all args, already found no new ones
          
          Expr *eval_arg = arg_list->ptr[arg_list->next_evaluate++];
          
          Expr *new_args_expr_scrap[1];
          Expr **new_args_exprs_ptr = new_args_expr_scrap; int new_args_exprs_len = 1;
          evaluate(eval_arg, defs, &new_args_exprs_ptr, &new_args_exprs_len);
          for (int i = 0; i < new_args_exprs_len; i++) {
            bool dupe = false;
            for (int k = 0; k < arg_list->len; k++) {
              if (arg_list->ptr[k] == new_args_exprs_ptr[i]) {
                dupe = true;
                break;
              }
            }
            if (dupe) continue;
            expr_list_push(arg_list, new_args_exprs_ptr[i]);
          }
        }
      }
      /*if (num_matched > 1) {
        fprintf(stderr, "whaat.. multiple functions matched at the same level of specificity: %i\n", num_matched);
        dump_expr_list(fn_list); fprintf(stderr, "\n");
        dump_expr_list(arg_list); fprintf(stderr, "\n");
        fail();
      }*/
      if (num_matched >= 1) break;
    }
    
    indent_depth --;
    
    // free(fn_list->ptr);
    free(spec_p);
    
    if (*exprs_len_p == 0) {
      return;
    }
    if (*exprs_len_p == 1) {
      if (verbose) {
        fprintf(stderr, "%*seval : return ", indent_depth, ""); dump_expr((*exprs_ptr_p)[0]); fprintf(stderr, "\n");
      }
      return;
    }
    return;
    
    fprintf(stderr, "while evaluating ");
    dump_expr(expr);
    fprintf(stderr, ":\n");
    
    fprintf(stderr, "got %i exprs\n", *exprs_len_p);
    
    for (int i = 0; i < *exprs_len_p; i++) {
      fprintf(stderr, " -- ");
      dump_expr((*exprs_ptr_p)[i]);
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "TODO\n");
    fail();
  }
  if (expr->type == EXPR_INT || expr->type == EXPR_STRING) {
    int capacity;
    expr_array_init_capacity(&capacity, exprs_len_p);
    expr_array_push(exprs_ptr_p, exprs_len_p, &capacity, expr);
    return;
  }
  if (expr->type == EXPR_UNDEFINED || expr->type == EXPR_FUNCTION) {
    int capacity;
    expr_array_init_capacity(&capacity, exprs_len_p);
    return;
  }
  if (expr->type == EXPR_NATIVE_FUNCTION) {
    // abort();
    NativeFunctionExpr *expr_native = (NativeFunctionExpr*) expr;
    indent_depth ++;
    Expr *res_expr = expr_native->resolve_fn(defs, expr_native->a, expr_native->b);
    indent_depth --;
    
    int capacity;
    expr_array_init_capacity(&capacity, exprs_len_p);
    if (res_expr->type != EXPR_UNDEFINED) {
      expr_array_push(exprs_ptr_p, exprs_len_p, &capacity, res_expr);
    }
    return;
  }
  
  fprintf(stderr, "evaluate: "); dump_expr(expr); fprintf(stderr, "\n");
  fprintf(stderr, "TODO\n");
  fail();
}

bool match_any(ExprList *values, Expr *target, Definitions *defs, Definitions *newdefs,
           bool lexical, bool direct) {
  for (int i = 0; i < values->len; i++) {
    if (match(values->ptr[i], target, defs, newdefs, lexical, direct)) return true;
  }
  return false;
}

bool _match(Expr *value, Expr *target, Definitions *defs, Definitions *newdefs,
           bool lexical, bool direct) {
  bool value_is_primitive = value->type == EXPR_INT || value->type == EXPR_STRING || value->type == EXPR_UNDEFINED;
  bool target_is_primitive = target->type == EXPR_INT || target->type == EXPR_STRING || value->type == EXPR_UNDEFINED;
  
  bool value_is_evaluable = value->type == EXPR_IDENTIFIER || value->type == EXPR_CALL;
  
  bool target_is_term = target_is_primitive || target->type == EXPR_IDENTIFIER;
  if (value_is_evaluable && target_is_primitive) {
    if (direct) return false; // trust the eval loop above us to handle it
    
    Expr *values_scrap[1];
    Expr **values_ptr = values_scrap; int values_len = 1;
    evaluate(value, defs, &values_ptr, &values_len);
    
    int num_matched = 0;
    for (int i = 0; i < values_len; i++) {
      if (match(values_ptr[0], target, defs, newdefs, false, false))
        num_matched ++;
    }
    if (values_ptr != values_scrap) free(values_ptr);
    
    if (num_matched == 0) return false;
    if (num_matched == 1) return true;
    
    fprintf(stderr, "TODO\n");
    fail();
  }
  
  if (value->type == EXPR_NATIVE_FUNCTION && target_is_term) {
    if (direct) return false;
    NativeFunctionExpr *value_native = (NativeFunctionExpr*) value;
    value = value_native->resolve_fn(defs, value_native->a, value_native->b);
    if (value->type == EXPR_UNDEFINED) return false;
    return match(value, target, defs, newdefs, false, false);
  }
  
  if (value->type == EXPR_INT && target->type == EXPR_INT) {
    if (((IntExpr*) value)->value != ((IntExpr*) target)->value) return false;
    return true;
  }
  
  // check for lexical override, ie. %foo
  if (target->type == EXPR_IDENTIFIER) {
    IdentifierExpr *target_ident = (IdentifierExpr*) target;
    if (target_ident->force_lexical) {
      lexical = true;
    }
    if (target_ident->force_dynamic) {
      lexical = false;
    }
  }
  
  if (lexical && value->type == EXPR_IDENTIFIER && target->type == EXPR_IDENTIFIER) {
    IdentifierExpr *value_ident = (IdentifierExpr*) value;
    IdentifierExpr *target_ident = (IdentifierExpr*) target;
    if (strcmp(value_ident->name, target_ident->name) != 0) return false;
    return true;
  }
  
  if (!lexical && target->type == EXPR_IDENTIFIER) {
    IdentifierExpr *target_ident = (IdentifierExpr*) target;
    define(newdefs, target_ident->name, value, false);
    return true;
  }
  
  if (value->type == EXPR_CALL && target->type == EXPR_CALL) {
    CallExpr *value_call = (CallExpr*) value;
    CallExpr *target_call = (CallExpr*) target;
    // TODO
    if (!match_any(value_call->fn, get_single_expr(target_call->fn), defs, newdefs, true, false)) return false;
    if (!match_any(value_call->arg, get_single_expr(target_call->arg), defs, newdefs, false, false)) return false;
    return true;
  }
  
  if (value->type == EXPR_IDENTIFIER && target->type == EXPR_CALL) {
    if (direct) return false;
    Expr *values_scrap[1];
    Expr **values_ptr = values_scrap; int values_len = 1;
    evaluate(value, defs, &values_ptr, &values_len);
    
    Expr *match_expr = NULL;
    Definitions match_defs = {0};
    
    for (int i = 0; i < values_len; i++) {
      Definitions newdefs2 = {0};
      if (match(values_ptr[i], target, defs, &newdefs2, false, false)) {
        match_defs = newdefs2;
        match_expr = values_ptr[i];
        continue;
      }
    }
    
    if (!match_expr) return false;
    
    merge_definitions(newdefs, &match_defs);
    return match_expr;
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
  
  if (lexical && value_is_primitive && target->type == EXPR_IDENTIFIER) {
    return false;
  }
  
  if (value->type == EXPR_NATIVE_FUNCTION && target->type == EXPR_CALL) {
    return false;
  }
  
  fprintf(stderr, "match: "); dump_expr(value); fprintf(stderr, "\n");
  fprintf(stderr, "   to: "); dump_expr(target); fprintf(stderr, "\n");
  fprintf(stderr, "TODO %i %i\n", lexical, direct);
  fail();
}

// lexical: match function names as names, not values
// direct: do not attempt to evaluate.
bool match(Expr *value, Expr *target, Definitions *defs, Definitions *newdefs,
           bool lexical, bool direct) {
  if (indent_depth > 1024) {
    fprintf(stderr, "depth exceeded.\n");
    fail();
  }
  if (verbose) {
    fprintf(stderr, "%*smatch: ", indent_depth, ""); dump_expr(value); fprintf(stderr, "\n");
    fprintf(stderr, "%*sto   : ", indent_depth, ""); dump_expr(target); fprintf(stderr, "\n");
  }
  indent_depth ++;
  bool res = _match(value, target, defs, newdefs, lexical, direct);
  indent_depth --;
  if (verbose) {
    if (res) fprintf(stderr, "%*s.. match!\n", indent_depth, "");
    else fprintf(stderr, "%*s.. no match!\n", indent_depth, "");
  }
  return res;
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
    dump_expr_list(expr_call->fn);
    fprintf(stderr, " (");
    dump_expr_list(expr_call->arg);
    fprintf(stderr, ")");
  } else if (expr->type == EXPR_FUNCTION) {
    FunctionExpr *expr_fn = (FunctionExpr*) expr;
    dump_expr(expr_fn->match);
    fprintf(stderr, " -> (");
    dump_expr_list(expr_fn->value);
    fprintf(stderr, ")");
  } else if (expr->type == EXPR_NATIVE_FUNCTION) {
    NativeFunctionExpr *expr_native = (NativeFunctionExpr*) expr;
    if (expr_native->a && expr_native->b) {
      fprintf(stderr, "%s (", expr_native->name);
      dump_expr_list(expr_native->a);
      fprintf(stderr, ") (");
      dump_expr_list(expr_native->b);
      fprintf(stderr, ")");
    } else {
      fprintf(stderr, "<native %s>", expr_native->name);
    }
  } else if (expr->type == EXPR_UNDEFINED) {
    fprintf(stderr, "<undefined>");
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

Expr *add_fn(Definitions *defs, ExprList *a, ExprList *b) {
  // fprintf(stderr, "[flatten a]\n");
  flatten_list(a, defs);
  // fprintf(stderr, "[flatten b]\n");
  flatten_list(b, defs);
  
  if (a->len == 0) fail();
  Expr *a_expr = a->ptr[a->len - 1]; // most resolved
  
  if (b->len == 0) fail();
  Expr *b_expr = b->ptr[b->len - 1];
  
  // if (a_expr->type == EXPR_UNDEFINED) abort();
  // if (b_expr->type == EXPR_UNDEFINED) abort();
  // this might come up if we're defining a toplevel variable "as undefined".
  if (a_expr->type == EXPR_UNDEFINED) return a_expr;
  if (b_expr->type == EXPR_UNDEFINED) return b_expr;
  
  if (a_expr->type == EXPR_STRING && b_expr->type == EXPR_INT) {
    char *bla = malloc(16);
    snprintf(bla, 16, "%i", ((IntExpr*) b_expr)->value);
    b_expr = make_string_expr(bla);
  }
  
  if (a_expr->type == EXPR_STRING && b_expr->type == EXPR_STRING) {
    StringExpr *a_str = (StringExpr*) a_expr;
    StringExpr *b_str = (StringExpr*) b_expr;
    char *combined = malloc(strlen(a_str->str) + strlen(b_str->str) + 1);
    combined[0] = 0;
    strcat(combined, a_str->str);
    strcat(combined, b_str->str);
    return make_string_expr(combined);
  }
  if (a_expr->type != EXPR_INT || b_expr->type != EXPR_INT) {
    fprintf(stderr, "internal error: invalid merge for native function\n");
    fail();
  }
  return make_int_expr(((IntExpr*) a_expr)->value + ((IntExpr*) b_expr)->value);
}

Expr *sub_fn(Definitions *defs, ExprList *a, ExprList *b) {
  flatten_list(a, defs);
  flatten_list(b, defs);
  
  if (a->len == 0) fail();
  Expr *a_expr = a->ptr[a->len - 1]; // most resolved
  
  if (b->len == 0) fail();
  Expr *b_expr = b->ptr[b->len - 1];
  
  if (a_expr->type == EXPR_UNDEFINED) return a_expr;
  if (b_expr->type == EXPR_UNDEFINED) return b_expr;
  
  if (a_expr->type != EXPR_INT || b_expr->type != EXPR_INT) {
    fprintf(stderr, "internal error: invalid merge for native function '-'\n");
    fprintf(stderr, "a: "); dump_expr(a_expr); fprintf(stderr, "\n");
    fprintf(stderr, "b: "); dump_expr(b_expr); fprintf(stderr, "\n");
    fail();
  }
  return make_int_expr(((IntExpr*) a_expr)->value - ((IntExpr*) b_expr)->value);
}

Expr *cmp_fn(Definitions *defs, ExprList *a, ExprList *b, int type) {
  flatten_list(a, defs);
  flatten_list(b, defs);
  
  if (a->len == 0) fail();
  Expr *a_expr = a->ptr[a->len - 1]; // most resolved
  
  if (b->len == 0) fail();
  Expr *b_expr = b->ptr[b->len - 1];
  
  if (a_expr->type == EXPR_UNDEFINED) return a_expr;
  if (b_expr->type == EXPR_UNDEFINED) return b_expr;
  
  if (a_expr->type != EXPR_INT || b_expr->type != EXPR_INT) {
    fprintf(stderr, "internal error: invalid merge for native function '<>'\n");
    fprintf(stderr, "a: "); dump_expr(a_expr); fprintf(stderr, "\n");
    fprintf(stderr, "b: "); dump_expr(b_expr); fprintf(stderr, "\n");
    fail();
  }
  
  int av = ((IntExpr*) a_expr)->value;
  int bv = ((IntExpr*) b_expr)->value;
  if (type == 0) return make_int_expr(av < bv);
  if (type == 1) return make_int_expr(av == bv);
  if (type == 2) return make_int_expr(av > bv);
  abort();
}

Expr *smaller_fn(Definitions *defs, ExprList *a, ExprList *b) {
  return cmp_fn(defs, a, b, 0);
}

Expr *equal_fn(Definitions *defs, ExprList *a, ExprList *b) {
  return cmp_fn(defs, a, b, 1);
}

Expr *larger_fn(Definitions *defs, ExprList *a, ExprList *b) {
  return cmp_fn(defs, a, b, 2);
}

void setup_runtime(Definitions *defs) {
  define(defs, "+", make_function_expr(make_identifier_expr("a"),
    make_function_expr(make_identifier_expr("b"),
      make_native_function_expr("add_fn", add_fn)
    )
  ), true);
  
  define(defs, "-", make_function_expr(make_identifier_expr("a"),
    make_function_expr(make_identifier_expr("b"),
      make_native_function_expr("sub_fn", sub_fn)
    )
  ), true);
  
  define(defs, "<", make_function_expr(make_identifier_expr("a"),
    make_function_expr(make_identifier_expr("b"),
      make_native_function_expr("smaller_fn", smaller_fn)
    )
  ), true);
  
  define(defs, "==", make_function_expr(make_identifier_expr("a"),
    make_function_expr(make_identifier_expr("b"),
      make_native_function_expr("equal_fn", equal_fn)
    )
  ), true);
  
  define(defs, ">", make_function_expr(make_identifier_expr("a"),
    make_function_expr(make_identifier_expr("b"),
      make_native_function_expr("larger_fn", larger_fn)
    )
  ), true);
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
