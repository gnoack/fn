
#include <stdio.h>

#include "cons.h"
#include "value.h"
#include "eval.h"

struct {
  value_t _if;
} symbols;

bool initialized = NO;

void init_symbols_if_needed() {
  if (initialized) return;
  symbols._if = make_string("if");
  initialized = YES;
}

#define car(x) first(x)
#define cdr(x) rest(x)
#define cadr(x) car(cdr(x))
#define caddr(x) cadr(cdr(x))
#define cadddr(x) caddr(cdr(x))

// Prints values, for debugging.
void print_value_internal(value_t v) {
  if (is_uint(v)) {
    printf("%d", v.uint);
  } else if (is_string(v)) {
    printf("%s", v.string);
  } else {
    CHECK(is_cons(v), "Can only be cons");
    printf("(");
    while (!is_nil(v)) {
      // is_cons(v) holds.
      if (is_cons(v)) {
	print_value_internal(first(v));
	v = rest(v);
	if (!is_nil(v)) {
	  printf(" ");
	}
      } else {
	// not a cons, print it.
	printf(" . ");
	print_value_internal(v);
	v = NIL;
      }
    }
    printf(")");
  }
}

void print_value(value_t v) {
  print_value_internal(v);
  printf("\n");
}

value_t eval_if(value_t sexp) {
  CHECK(length_int(sexp) == 4, "Must have size of 4");
  value_t condition = cadr(sexp);
  value_t then_branch = caddr(sexp);
  value_t else_branch = cadddr(sexp);

  if (is_nil(eval(condition)))
    return eval(else_branch);
  else
    return eval(then_branch);
}

extern
value_t eval(value_t program) {
  init_symbols_if_needed();
  printf("eval: ");
  print_value(program);
  if (is_uint(program) || is_string(program)) {
    return program;
  }
  CHECK(is_cons(program), "What is this? I can't evaluate it!");
  value_t command = car(program);
  if (value_eq(command, symbols._if)) return eval_if(program);
  // All other cases are unsupported.
  printf("We hit an unsupported case!\n");
  print_value(program);
}

