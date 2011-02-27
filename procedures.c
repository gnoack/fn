
#include <stdio.h>

#include "value.h"
#include "cons.h"
#include "carcdr.h"
#include "symbols.h"
#include "eval.h"
#include "env.h"

#include "procedures.h"

// Lisp procedures and accessing them.
oop make_procedure(oop lambda_list, oop body, oop env) {
  return LIST(symbols._lisp_procedure_marker,
	      lambda_list, body, env);
}
oop fn_lambda_list(oop fn) { return cadr(fn); }
oop fn_body(oop fn) { return caddr(fn); }
oop fn_env(oop fn) { return cadddr(fn); }
bool is_lisp_procedure(oop fn) {
  CHECKV(is_cons(fn), fn, "Must be cons to be a composite procedure.");
  return value_eq(symbols._lisp_procedure_marker, car(fn));
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
    CHECK(!is_nil(args), "Need more arguments in function call.");
    return make_env(car(ll), car(args),
		    destructure_lambda_list(cdr(ll), cdr(args), env));
  }
}

oop apply_lisp_procedure(oop fn, oop args) {
  oop env = destructure_lambda_list(fn_lambda_list(fn),
				    args, fn_env(fn));
  return eval(fn_body(fn), env);
}

// Native procedures
oop make_native_fn(function c_function) {
  return LIST(symbols._native_procedure_marker,
	      make_smallint((uint) c_function));
}

bool is_native_fn(oop fn) {
  return value_eq(car(fn), symbols._native_procedure_marker);
}

function native_fn_function(oop fn) {
  return (function)(get_smallint(cadr(fn)));
}

oop native_fn_apply(oop fn, oop args) {
  function c_function = native_fn_function(fn);
  return c_function(args);
}


// Application

// Function application.
// First argument is function, rest are arguments.
oop apply(oop values) {
  oop fn = car(values);
  if (is_lisp_procedure(fn)) {
    return apply_lisp_procedure(fn, cdr(values));
  } else if (is_native_fn(fn)) {
    return native_fn_apply(fn, cdr(values));
  }
  printf("Not a procedure.");
  print_value(fn);
}
