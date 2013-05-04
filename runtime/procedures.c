
#include <stdio.h>

#include "carcdr.h"
#include "cons.h"
#include "data.h"
#include "debug.h"
#include "eval.h"
#include "gc.h"
#include "interpreter.h"
#include "memory.h"
#include "symbols.h"
#include "value.h"

#include "procedures.h"

#define MAX_NATIVE_PROCEDURES 256
function native_procedures[MAX_NATIVE_PROCEDURES];
fn_uint next_native_procedure = 0;

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
  CHECKV(is_raw_mem(cadr(code)), cadr(code), "Needs to have bytecode.");
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
  fn_uint index = next_native_procedure;
  CHECK(index < MAX_NATIVE_PROCEDURES, "Too many native functions registered.");
  native_procedures[index] = c_function;
  next_native_procedure++;

  oop result = mem_alloc(3);
  mem_set(result, 0, symbols._native_procedure);
  mem_set(result, 1, NIL);  // Name.
  mem_set(result, 2, make_smallint(index));
  return result;
}


// These work for both interpreted and compiled Lisp procedures.
oop fn_name(oop fn) { return mem_get(fn, 1); }
oop fn_lambda_list(oop fn) { return mem_get(fn, 2); }
oop fn_code(oop fn) { return mem_get(fn, 3); }
oop fn_env(oop fn) { return mem_get(fn, 4); }
fn_uint fn_argnum(oop fn) { return get_smallint(mem_get(fn, 5)) >> 1; }
boolean fn_nested_args(oop fn) { return TO_BOOL(get_smallint(mem_get(fn, 5)) & 1); }

// Get the C function stored in a native procedure.
function native_fn_function(oop fn) {
  return native_procedures[get_smallint(mem_get(fn, 2))];
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

// Returns twice the number of variables in the lambda list,
// plus 1, if lambda list was nested or had varargs (not flat),
fn_uint num_vars_in_ll(oop ll) {
  fn_uint count = 0L;
  while (is_cons(ll)) {
    oop ll_item = car(ll);
    if (is_symbol(ll_item)) {
      if (value_eq(ll_item, symbols._rest)) {
        ll = cdr(ll);
        CHECKV(is_cons(ll), ll, "Need one more item after &rest.");
        CHECKV(is_symbol(car(ll)), car(ll), "Need a symbol after &rest.");
        count = (count + 2) | 1;
        ll = cdr(ll);
        CHECKV(is_nil(ll), ll, "Need only one symbol after &rest.");
        return count;
      } else {
        count += 2;
      }
    } else if (is_cons(ll_item)) {
      count = (count + num_vars_in_ll(ll_item)) | 1;
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
        CHECKV(is_cons(args), args, "Not enough arguments.");
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
        CHECKV(is_cons(args), args, "Not enough arguments.");
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

oop make_frame_for_application(oop cfn, oop args) {
  oop env = make_frame(fn_argnum(cfn), fn_env(cfn));
  destructure_lambda_list_into_frame(fn_lambda_list(cfn), args, env, 0);
  return env;
}

oop apply_compiled_lisp_procedure(oop cfn, oop args) {
  return interpret(make_frame_for_application(cfn, args), fn_code(cfn));
}

oop make_dframe_for_application(oop lfn, oop args) {
  oop env = make_dframe(fn_env(lfn), fn_argnum(lfn));
  destructure_lambda_list_into_dframe(fn_lambda_list(lfn), args, env, 0);
  return env;
}

oop apply_lisp_procedure(oop fn, oop args) {
  oop env = make_dframe_for_application(fn, args);
  gc_protect_counter++;
  oop result = eval(make_cons(symbols._progn, fn_code(fn)), env);
  gc_protect_counter--;
  return result;
}

oop apply_native_fn(oop fn, oop args) {
  function c_function = native_fn_function(fn);
  gc_protect_counter++;
  oop result = c_function(args);
  gc_protect_counter--;
  return result;
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
  print_value(fn_name(fn));
  if (!is_native_procedure(fn)) {
    printf(" ");
    print_value(fn_lambda_list(fn));
  }
  printf(">");
}


// Application

#define MAX_APPLY_STACK 10000
oop apply_stack[MAX_APPLY_STACK];
int apply_stack_pos = 0;

/*
 * Print the current stack for debugging.
 * Conflicts badly with the GC running in between,
 * it's better to only do it when you're crashing anyway. :)
 */
void print_apply_stack() {
  int i;
  for (i=apply_stack_pos-1; i>=0; i--) {
    printf("%3d ", i);
    println_value(apply_stack[i]);
  }
}

void apply_stack_push(oop values) {
  if (apply_stack_pos < MAX_APPLY_STACK) {
    apply_stack[apply_stack_pos] = values;
  }
  apply_stack_pos++;
}

void apply_stack_pop() {
  apply_stack_pos--;
}

// Function application.
// First argument is function, rest are arguments.
oop apply(oop values) {
  apply_stack_push(values);
  oop fn = car(values);
  oop result;
  if (is_lisp_procedure(fn)) {
    result = apply_lisp_procedure(fn, cdr(values));
  } else if (is_compiled_lisp_procedure(fn)) {
    result = apply_compiled_lisp_procedure(fn, cdr(values));
  } else {
    CHECKV(is_native_procedure(fn), fn, "Must be a procedure for applying.");
    result = apply_native_fn(fn, cdr(values));
  }
  apply_stack_pop();
  return result;
}

void enumerate_apply_stack_roots(void (*accept)(oop* place)) {
  int i;
  for (i=0; i<apply_stack_pos; i++) {
    accept(&apply_stack[i]);
  }
}

void init_procedures() {
  print_stack_frame = print_apply_stack;
  gc_register_persistent_refs(enumerate_apply_stack_roots);
}
