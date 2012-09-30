
#include <stdio.h>

#include "value.h"
#include "cons.h"
#include "carcdr.h"
#include "symbols.h"
#include "eval.h"
#include "env.h"
#include "memory.h"

#include "procedures.h"

// Lisp procedures and accessing them.
oop make_procedure(oop lambda_list, oop body, oop env) {
  oop result = mem_alloc(5);
  mem_set(result, 0, symbols._procedure);
  mem_set(result, 1, NIL);  // Name.
  mem_set(result, 2, lambda_list);
  mem_set(result, 3, body);
  mem_set(result, 4, env);
  return result;
}

oop fn_name(oop fn) { return mem_get(fn, 1); }
oop fn_lambda_list(oop fn) { return mem_get(fn, 2); }
oop fn_body(oop fn) { return mem_get(fn, 3); }
oop fn_env(oop fn) { return mem_get(fn, 4); }
boolean is_lisp_procedure(oop fn) {
  return TO_BOOL(is_mem(fn) &&
                 value_eq(symbols._procedure, mem_get(fn, 0)));
}

// NOTE: Works for both native and Lisp procedures!
// Can only be set once per procedure.
oop fn_set_name(oop fn, oop name) {
  if (is_nil(fn_name(fn))) {
    return mem_set(fn, 1, name);
  } else {
    return name;
  }
}

// Given a lambda list and a argument vector,
// returns a filled environment linking to the
// environment env.
// Helper for apply_lisp_procedure below.
oop destructure_lambda_list(oop ll, oop args, oop env) {
  if (is_nil(ll)) {
    CHECKV(is_nil(args), args, "Too many arguments in function call.");
    return env;
  } else {
    if (is_cons(car(ll))) {
      // Nested destructuring
      CHECK(!is_nil(args), "Need more arguments in function call.");
      return destructure_lambda_list(
	  cdr(ll), cdr(args),
	  destructure_lambda_list(car(ll), car(args), env));
    } else if (value_eq(car(ll), symbols._rest)) {
      // &rest binds to a list of all remaining arguments.
      CHECK(is_cons(cdr(ll)), "Need variable identifier after &rest");
      CHECK(is_nil(cddr(ll)), "Only one variable identifier after &rest");
      return make_env(cadr(ll), args, env);
    } else {
      CHECK(!is_nil(args), "Need more arguments in function call.");
      return make_env(car(ll), car(args),
		      destructure_lambda_list(cdr(ll), cdr(args), env));
    }
  }
}

oop apply_lisp_procedure(oop fn, oop args) {
  oop env = destructure_lambda_list(fn_lambda_list(fn),
				    args, fn_env(fn));
  return eval_all(fn_body(fn), env);
}

// Native procedures
oop make_native_fn(function c_function) {
  oop result = mem_alloc(3);
  mem_set(result, 0, symbols._native_procedure);
  mem_set(result, 1, NIL);  // Name.
  mem_set(result, 2, make_smallint((fn_uint) c_function));
  return result;
}

boolean is_native_procedure(oop fn) {
  return TO_BOOL(is_mem(fn) &&
                 value_eq(mem_get(fn, 0), symbols._native_procedure));
}

function native_fn_function(oop fn) {
  return (function)(get_smallint(mem_get(fn, 2)));
}

oop native_fn_apply(oop fn, oop args) {
  function c_function = native_fn_function(fn);
  return c_function(args);
}

boolean is_procedure(oop fn) {
  return TO_BOOL(is_lisp_procedure(fn) || is_native_procedure(fn));
}

// Application

oop apply_stack[4000];
int apply_stack_pos = 0;

/*
 * Print the current stack for debugging.
 * Conflicts badly with the GC running in between,
 * it's better to only do it when you're crashing anyway. :)
 */
void print_apply_stack() {
  int i = apply_stack_pos;
  while (i >= 0) {
    printf("%3d ", i);
    print_value(apply_stack[i]);
    i--;
  }
}

// Function application.
// First argument is function, rest are arguments.
oop apply(oop values) {
  print_stack_frame = print_apply_stack; // TODO: Set somewhere else.
  apply_stack[apply_stack_pos++] = values;
  oop fn = car(values);
  oop result;
  if (is_lisp_procedure(fn)) {
    result = apply_lisp_procedure(fn, cdr(values));
  } else {
    CHECKV(is_native_procedure(fn), fn, "Must be a procedure for applying.");
    result = native_fn_apply(fn, cdr(values));
  }
  apply_stack_pos--;
  return result;
}
