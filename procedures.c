
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
