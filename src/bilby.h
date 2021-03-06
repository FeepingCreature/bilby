#ifndef BILBY_H
#define BILBY_H

#include <rdparse/util.h>

typedef struct Definitions Definitions;

typedef struct {
  Definitions *defs;
  bool verbose;
} Environment;

Definitions *alloc_definition_set();

void setup_runtime(Environment *env);

bool parse_tl(char **textp, Environment *env);

bool eat_bilby_filler(char **textp);

void fail() __attribute__((noreturn));

typedef enum {
  EXPR_INT,
  EXPR_STRING,
  EXPR_UNDEFINED, // use for value of functions that do not return a value.
  EXPR_IDENTIFIER,
  EXPR_FUNCTION,
  EXPR_NATIVE_FUNCTION,
  EXPR_CALL
} ExprType;

typedef struct {
  ExprType type;
} Expr;

// list of evaluated expressions, in order
typedef struct {
  Expr **ptr;
  int len;
  int next_evaluate; // index of next unevaluated expression
} ExprList;

ExprList *list_from_expr(Expr *expr);
ExprList *list_clone(ExprList *list);
// for exprlists that are assumed to only ever contain exactly one expr
Expr *get_single_expr(ExprList *list);
void dump_expr_list(ExprList *list);

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
  bool force_lexical, force_dynamic;
} IdentifierExpr;

typedef struct {
  Expr base;
  ExprList *fn;
  ExprList *arg;
} CallExpr;

typedef struct {
  Expr base;
  Expr *match;
  ExprList *value;
} FunctionExpr;

typedef Expr* (*ResolveFn)(Environment*, ExprList *a, ExprList *b);

typedef struct {
  Expr base;
  const char *name;
  // TODO arity other than 2
  ResolveFn resolve_fn;
  ExprList *a, *b; // arguments
} NativeFunctionExpr;

#endif
