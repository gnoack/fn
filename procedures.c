
#include <stdio.h>

#include "cons.h"
#include "carcdr.h"
#include "symbols.h"

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
  return value_eq(symbols._lisp_procedure_marker, car(fn));
}

// Native procedures
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
