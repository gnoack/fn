
#include <stdio.h>

#include "cons.h"
#include "value.h"
#include "carcdr.h"
#include "procedures.h"
#include "symbols.h"

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

oop make_env(oop key, oop value, oop env) {
  return make_cons(make_cons(key, value), env);
}

// Defines the type "function" to be oop --> oop.
// Who came up with this syntax?
typedef oop (*function)(oop args);

oop make_native_fn(function c_function) {
  CHECK((c_function == (function)((((uint) c_function) >> 1) << 1)),
	"Very bad. Can't save native procedure in one smallint.");
  return LIST(symbols._native_procedure_marker,
	      make_smallint((uint) c_function));
}

bool is_native_fn(oop fn) {
  return value_eq(car(fn), symbols._native_procedure_marker);
}

function native_fn_function(oop fn) {
  return (function)(cadr(fn).smallint >> 1);
}

oop native_fn_apply(oop fn, oop args) {
  function c_function = native_fn_function(fn);
  return c_function(args);
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

bool env_haskey(oop env, oop key) {
  while (!is_nil(env)) {
    if (value_eq(key, caar(env))) {
      return YES;
    }
    env = cdr(env);
  }
  return NO;
}

oop env_lookup(oop env, oop key) {
  if (is_nil(env)) {
    return NIL;
  } else {
    oop pair = car(env);
    oop mykey = car(pair);
    if (value_eq(mykey, key)) {
      return cdr(pair);
    } else {
      return env_lookup(cdr(env), key);
    }
  }
}

// Given a lambda list and a argument vector,
// returns a filled environment linking to the
// environment env.
oop destructure_lambda_list(oop ll, oop args, oop env) {
  if (is_nil(ll)) {
    return env;
  } else {
    return make_env(car(ll), car(args),
		    destructure_lambda_list(cdr(ll), cdr(args), env));
  }
}

oop eval_lambda(oop program, oop env) {
  return make_procedure(cadr(program),
			caddr(program),
			env);
}

// Function application.
// First argument is function, rest are arguments.
oop apply(oop oop_vector) {
  oop fn = car(oop_vector);
  if (is_lisp_procedure(fn)) {
    oop args = cdr(oop_vector);
    oop env = destructure_lambda_list(fn_lambda_list(fn),
				      args, fn_env(fn));
    return eval(fn_body(fn), env);
  } else if (is_native_fn(fn)) {
    return native_fn_apply(fn, cdr(oop_vector));
  }
  printf("Not a procedure.");
  print_value(fn);
}

extern
oop eval_global(oop program) {
  return eval(program, global_env);
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

