
#include "cons.h"
#include "value.h"
#include "eval.h"

struct {
  value_t _if;
} symbols;

void init_symbols() {
  symbols._if = make_string("if");
}

#define car(x) first(x)
#define cdr(x) rest(x)
#define cadr(x) car(cdr(x))
#define caddr(x) cadr(cdr(x))
#define cadddr(x) caddr(cdr(x))

value_t eval_if(value_t sexp) {
  value_t condition = cadr(sexp);
  value_t then_branch = caddr(sexp);
  value_t else_branch = cadddr(sexp);

  if (is_nil(condition))
    return eval(else_branch);
  else
    return eval(then_branch);
}

extern
value_t eval(value_t program) {
  value_t command = first(program);
  if (value_eq(command, symbols._if)) return eval_if(program);
  // All other cases are unsupported.
  return make_uint(0xdead);
}

