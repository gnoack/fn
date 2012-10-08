
#include <stdio.h>

#include "data.h"
#include "value.h"
#include "cons.h"
#include "carcdr.h"
#include "eval.h"
#include "interpreter.h"
#include "memory.h"
#include "symbols.h"

#include "procedures.h"

// Interpreted Lisp procedure.
oop make_procedure(oop lambda_list, oop body, oop env) {
  oop result = mem_alloc(5);
  mem_set(result, 0, symbols._procedure);
  mem_set(result, 1, NIL);  // Name.
  mem_set(result, 2, lambda_list);
  mem_set(result, 3, body);
  mem_set(result, 4, env);
  return result;
}

// Compiled Lisp procedure.
oop make_compiled_procedure(oop lambda_list, oop code, oop env) {
  CHECKV(is_cons(code), code,
         "Code has to be a three tuple.");
  CHECKNUMBER(first(code));
  CHECKV(is_primitive_mem(cadr(code)), cadr(code), "Needs to have bytecode.");
  oop result = mem_alloc(5);
  mem_set(result, 0, symbols._compiled_procedure);
  mem_set(result, 1, NIL);  // Name.
  mem_set(result, 2, lambda_list);
  mem_set(result, 3, code);
  mem_set(result, 4, env);
  return result;
}

// Native procedures
oop make_native_procedure(function c_function) {
  oop result = mem_alloc(3);
  mem_set(result, 0, symbols._native_procedure);
  mem_set(result, 1, NIL);  // Name.
  mem_set(result, 2, make_smallint((fn_uint) c_function));
  return result;
}


// These work for both interpreted and compiled Lisp procedures.
oop fn_name(oop fn) { return mem_get(fn, 1); }
oop fn_lambda_list(oop fn) { return mem_get(fn, 2); }
oop fn_code(oop fn) { return mem_get(fn, 3); }
oop fn_env(oop fn) { return mem_get(fn, 4); }

// Get the C function stored in a native procedure.
function native_fn_function(oop fn) {
  return (function)(get_smallint(mem_get(fn, 2)));
}


/*
 * Identification.
 */

boolean is_lisp_procedure(oop fn) {
  return TO_BOOL(is_mem(fn) &&
                 value_eq(symbols._procedure, mem_get(fn, 0)));
}

boolean is_compiled_lisp_procedure(oop cfn) {
  return TO_BOOL(is_mem(cfn) &&
                 value_eq(symbols._compiled_procedure, mem_get(cfn, 0)));
}

boolean is_native_procedure(oop fn) {
  return TO_BOOL(is_mem(fn) &&
                 value_eq(mem_get(fn, 0), symbols._native_procedure));
}

boolean is_procedure(oop fn) {
  return TO_BOOL(is_lisp_procedure(fn) ||
                 is_compiled_lisp_procedure(fn) ||
                 is_native_procedure(fn));
}


// NOTE: Works for all kinds of procedures!
// Can only be set once per procedure.
oop procedure_set_name(oop fn, oop name) {
  CHECKV(is_procedure(fn), fn, "Needs to be a procedure to set its name.");
  if (is_nil(fn_name(fn))) {
    return mem_set(fn, 1, name);
  } else {
    return name;
  }
}


// Lambda list destructuring.
// TODO: This code needs major cleanup.

// TODO: Get rid of this function.
oop make_env(oop key, oop value, oop env) {
  return make_cons(make_cons(key, value), env);
}

// Given a lambda list and a argument vector,
// returns a filled environment linking to the
// environment env.
// Helper for apply_lisp_procedure below.
// TODO: Make lamba list destructuring much faster!
oop destructure_lambda_list_inner(oop ll, oop args, oop env) {
  if (is_nil(ll)) {
    CHECKV(is_nil(args), args, "Too many arguments in function call.");
    return env;
  } else {
    if (is_cons(car(ll))) {
      // Nested destructuring
      CHECK(!is_nil(args), "Need more arguments in function call.");
      return destructure_lambda_list_inner(
	  cdr(ll), cdr(args),
	  destructure_lambda_list_inner(car(ll), car(args), env));
    } else if (value_eq(car(ll), symbols._rest)) {
      // &rest binds to a list of all remaining arguments.
      CHECK(is_cons(cdr(ll)), "Need variable identifier after &rest");
      CHECK(is_nil(cddr(ll)), "Only one variable identifier after &rest");
      return make_env(cadr(ll), args, env);
    } else {
      CHECK(!is_nil(args), "Need more arguments in function call.");
      return make_env(car(ll), car(args),
		      destructure_lambda_list_inner(cdr(ll), cdr(args), env));
    }
  }
}

oop old_style_env_to_dframe(oop env, oop next_frame) {
  fn_uint size = length_int(env);
  oop result = make_dframe(next_frame, size);
  int i;
  for (i=0; i<size; i++) {
    oop key = first(first(env));
    oop value = rest(first(env));
    dframe_register_key(result, i, key, value);
    env = cdr(env);
  }
  return result;
}

// Same as above, but converts into dframe.
oop destructure_lambda_list(oop ll, oop args, oop next_frame) {
  return old_style_env_to_dframe(destructure_lambda_list_inner(ll, args, NIL),
                                 next_frame);
}


// Specialized apply methods.

oop apply_compiled_lisp_procedure(oop cfn, oop args) {
  oop linked_list_env =
    destructure_lambda_list_inner(fn_lambda_list(cfn), args, NIL);
  oop outer_frame = fn_env(cfn);
  fn_uint inner_frame_size = length_int(linked_list_env);
  oop inner_frame = make_frame(inner_frame_size, NIL, NIL, outer_frame);
  unsigned int index = 0;
  while (!is_nil(linked_list_env)) {
    set_var(inner_frame, index, rest(first(linked_list_env)));
    index++;
    linked_list_env = rest(linked_list_env);
  }
  return interpret(inner_frame, fn_code(cfn));
}

oop apply_lisp_procedure(oop fn, oop args) {
  oop env = destructure_lambda_list(fn_lambda_list(fn),
				    args, fn_env(fn));
  return eval_all(fn_code(fn), env);
}


oop native_fn_apply(oop fn, oop args) {
  function c_function = native_fn_function(fn);
  return c_function(args);
}


// Debug printing.

void print_procedure(oop fn) {
  if (is_lisp_procedure(fn)) {
    printf("<PROCEDURE ");
  } else if (is_compiled_lisp_procedure(fn)) {
    printf("<COMPILED-PROCEDURE ");
  } else if (is_native_procedure(fn)) {
    printf("<NATIVE-PROCEDURE ");
  }
  print_value_internal(fn_name(fn));
  if (!is_native_procedure(fn)) {
    printf(" ");
    print_value_internal(fn_lambda_list(fn));
  }
  printf(">");
}


// Application

#define MAX_APPLY_STACK 4000
oop apply_stack[MAX_APPLY_STACK];
int apply_stack_pos = 0;

/*
 * Print the current stack for debugging.
 * Conflicts badly with the GC running in between,
 * it's better to only do it when you're crashing anyway. :)
 */
void print_apply_stack() {
  int i = apply_stack_pos - 1;
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
  CHECK(apply_stack_pos < MAX_APPLY_STACK, "Stack exhausted.");
  apply_stack[apply_stack_pos++] = values;
  oop fn = car(values);
  oop result;
  if (is_lisp_procedure(fn)) {
    result = apply_lisp_procedure(fn, cdr(values));
  } else if (is_compiled_lisp_procedure(fn)) {
    result = apply_compiled_lisp_procedure(fn, cdr(values));
  } else {
    CHECKV(is_native_procedure(fn), fn, "Must be a procedure for applying.");
    result = native_fn_apply(fn, cdr(values));
  }
  apply_stack_pos--;
  return result;
}
