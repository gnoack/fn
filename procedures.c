
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

// Count variables in a lambda list and do basic syntax checks (forward decl).
fn_uint num_vars_in_ll(oop ll);

// Interpreted Lisp procedure.
oop make_procedure(oop lambda_list, oop body, oop env) {
  oop result = mem_alloc(6);
  mem_set(result, 0, symbols._procedure);
  mem_set(result, 1, NIL);  // Name.
  mem_set(result, 2, lambda_list);
  mem_set(result, 3, body);
  mem_set(result, 4, env);
  mem_set(result, 5, make_smallint(num_vars_in_ll(lambda_list)));
  return result;
}

// Compiled Lisp procedure.
oop make_compiled_procedure(oop lambda_list, oop code, oop env) {
  CHECKV(is_cons(code), code,
         "Code has to be a three tuple.");
  CHECKNUMBER(first(code));
  CHECKV(is_primitive_mem(cadr(code)), cadr(code), "Needs to have bytecode.");
  oop result = mem_alloc(6);
  mem_set(result, 0, symbols._compiled_procedure);
  mem_set(result, 1, NIL);  // Name.
  mem_set(result, 2, lambda_list);
  mem_set(result, 3, code);
  mem_set(result, 4, env);
  mem_set(result, 5, make_smallint(num_vars_in_ll(lambda_list)));
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
fn_uint fn_argnum(oop fn) { return get_smallint(mem_get(fn, 5)); }

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
// TODO: Remove duplication between destructuring functions.

fn_uint num_vars_in_ll(oop ll) {
  fn_uint count = 0L;
  while (is_cons(ll)) {
    oop item = car(ll);
    if (is_symbol(item)) {
      if (value_eq(item, symbols._rest)) {
        ll = cdr(ll);
        CHECKV(is_cons(ll), ll, "Need one more item after &rest.");
        CHECKV(is_symbol(car(ll)), car(ll), "Need a symbol after &rest.");
        count++;
        ll = cdr(ll);
        CHECKV(is_nil(ll), ll, "Need only one symbol after &rest.");
        return count;
      } else {
        count++;
      }
    } else if (is_cons(item)) {
      count += num_vars_in_ll(item);
    }
    ll = cdr(ll);
  }
  return count;
}

fn_uint destructure_lambda_list_into_dframe(oop ll, oop args, oop dframe,
                                            fn_uint idx) {
  while (is_cons(ll)) {
    oop ll_item = car(ll);
    if (is_symbol(ll_item)) {
      if (value_eq(ll_item, symbols._rest)) {
        ll = cdr(ll);
        oop key = car(ll);
        oop value = args;
        dframe_register_key(dframe, idx, key, value);
        idx++;
        return idx;
      } else {
        oop key = ll_item;
        oop value = car(args);
        dframe_register_key(dframe, idx, key, value);
        idx++;
      }
    } else if (is_cons(ll_item)) {
      oop arg_item = car(args);
      CHECKV(is_cons(arg_item), arg_item, "Not enough arguments.");
      idx = destructure_lambda_list_into_dframe(ll_item, arg_item, dframe,
                                                idx);
    }
    ll = cdr(ll);
    args = cdr(args);
  }
  CHECKV(is_nil(args), args, "Too many arguments.");
  return idx;
}

fn_uint destructure_lambda_list_into_frame(oop ll, oop args, oop frame,
                                           fn_uint idx) {
  while (is_cons(ll)) {
    oop ll_item = car(ll);
    if (is_symbol(ll_item)) {
      if (value_eq(ll_item, symbols._rest)) {
        ll = cdr(ll);
        oop value = args;
        set_var(frame, idx, value);
        idx++;
        return idx;
      } else {
        oop value = car(args);
        set_var(frame, idx, value);
        idx++;
      }
    } else if (is_cons(ll_item)) {
      oop arg_item = car(args);
      CHECKV(is_cons(arg_item), arg_item, "Not enough arguments.");
      idx = destructure_lambda_list_into_frame(ll_item, arg_item, frame,
                                               idx);
    }
    ll = cdr(ll);
    args = cdr(args);
  }
  CHECKV(is_nil(args), args, "Too many arguments.");
  return idx;
}


// Specialized apply methods.

oop apply_compiled_lisp_procedure(oop cfn, oop args) {
  oop env = make_frame(fn_argnum(cfn), NIL, NIL, fn_env(cfn));
  destructure_lambda_list_into_frame(fn_lambda_list(cfn), args, env, 0);
  return interpret(env, fn_code(cfn));
}

oop apply_lisp_procedure(oop fn, oop args) {
  oop env = make_dframe(fn_env(fn), fn_argnum(fn));
  destructure_lambda_list_into_dframe(fn_lambda_list(fn), args, env, 0);
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
