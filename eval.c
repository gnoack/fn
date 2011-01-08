
#include <stdio.h>

#include "cons.h"
#include "value.h"
#include "carcdr.h"
#include "procedures.h"
#include "symbols.h"
#include "env.h"

#include "eval.h"

oop global_env;

oop add(oop args) {
  // TODO: Pretty inaccurate. This works on smallints only.
  uint i = 0;
  while (!is_nil(args)) {
    oop arg = car(args);
    // TODO: How to do proper error handling in-lisp?
    if (!is_smallint(arg)) {
      printf("Warning: Trying to int-add bad value: ");
      print_value(arg);
    }
    i += arg.smallint >> 1;
    args = cdr(args);
  }
  return make_smallint(i);
}

void register_in_global_env(const char* name, function fn) {
  global_env = make_env(make_symbol(name),
			make_native_fn(fn),
			global_env);
}

void init_eval() {
  static bool initialized = NO;
  if (initialized) return;
  global_env = NIL;
  register_in_global_env("+", add);
}

// Evaluation.

oop eval_if(oop sexp, oop env) {
  CHECK(length_int(sexp) == 4, "Must have size of 4");
  oop condition = cadr(sexp);
  oop then_branch = caddr(sexp);
  oop else_branch = cadddr(sexp);
  if (is_nil(eval(condition, env)))
    return eval(else_branch, env);
  else
    return eval(then_branch, env);
}

oop eval_let(oop sexp, oop env) {
  CHECK(value_eq(symbols._let, car(sexp)), "Must be a let form.");
  CHECK(length_int(sexp) == 3, "Bad let form length.");
  oop bindings = cadr(sexp);
  while (!is_nil(bindings)) {
    oop binding = car(bindings);
    CHECK(length_int(binding) == 2, "Bad binding length.");
    env = make_env(car(binding), eval(cadr(binding), env), env);
    bindings = cdr(bindings);
  }
  oop body = caddr(sexp);
  return eval(body, env);
}

oop eval_lambda(oop program, oop env) {
  return make_procedure(cadr(program),
			caddr(program),
			env);
}

extern
oop eval_global(oop program) {
  return eval(program, global_env);
}

// C equivalent to calling (map eval list).
oop map_eval(oop list, oop env) {
  if (is_nil(list)) {
    return list;
  } else {
    // Done in two steps for in-order execution.
    oop mycar = eval(car(list), env);
    return make_cons(mycar, map_eval(cdr(list), env));
  }
}

extern
oop eval(oop program, oop env) {
  printf("eval: ");
  print_value(program);
  if (is_smallint(program) || is_nil(program)) {
    return program;
  } else if (is_symbol(program)) {
    // TODO: Proper error handling.
    CHECK(env_haskey(env, program), "Unknown symbol.");
    return env_lookup(env, program);
  }
  CHECK(is_cons(program), "What is this? I can't evaluate it!");
  oop command = car(program);
  if (value_eq(command, symbols._if)) return eval_if(program, env);
  if (value_eq(command, symbols._lambda)) {
    return eval_lambda(program, env);
  }
  if (value_eq(command, symbols._let)) {
    return eval_let(program, env);
  }
  // Otherwise, it must be a function application.
  return apply(map_eval(program, env));
}

