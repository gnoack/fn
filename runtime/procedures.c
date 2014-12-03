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

// Compiled Lisp procedure.
proc_t* make_compiled_procedure(oop lambda_list, oop env,
				oop bytecode, oop ip, oop oop_lookup_table,
				fn_uint max_stack_depth) {
  CHECKNUMBER(ip);
  CHECKV(is_raw_mem(bytecode), bytecode, "Needs to have bytecode.");
  // TODO: Check that num_vars_in_ll < 2**16, so it won't overlap.
  fn_uint numbers = (max_stack_depth << 16) | num_vars_in_ll(lambda_list);
  proc_t* result = MemAlloc(proc_t);
  *result = (proc_t) {
      .type         = symbols._compiled_procedure,
      .mutable_name = NIL,
      .lambda_list  = lambda_list,
      .numbers      = make_smallint(numbers),
      .env          = env,
      .bytecode     = bytecode,
      .ip           = ip,
      .oop_table    = oop_lookup_table
  };
  return result;
}

// Native procedures
oop make_native_procedure(function c_function) {
  fn_uint index = next_native_procedure;
  CHECK(index < MAX_NATIVE_PROCEDURES, "Too many native functions registered.");
  native_procedures[index] = c_function;
  next_native_procedure++;

  oop result = mem_alloc(3);
  MEM_SET(result, 0, symbols._native_procedure);
  MEM_SET(result, 1, NIL);  // Name.
  MEM_SET(result, 2, make_smallint(index));
  return result;
}

// TODO: Rename to cfn_*?
oop fn_name(oop fn) { return mem_get(fn, 1); }
oop fn_lambda_list(oop fn) { return mem_get(fn, 2); }

// Get the C function stored in a native procedure.
function native_fn_function(oop fn) {
  return native_procedures[get_smallint(mem_get(fn, 2))];
}


/*
 * Identification.
 */

boolean is_compiled_lisp_procedure(oop cfn) {
  return TO_BOOL(is_mem(cfn) &&
                 value_eq(symbols._compiled_procedure, MEM_GET(cfn, 0)));
}

boolean is_native_procedure(oop fn) {
  return TO_BOOL(is_mem(fn) &&
                 value_eq(symbols._native_procedure, MEM_GET(fn, 0)));
}

boolean is_procedure(oop fn) {
  return TO_BOOL(is_compiled_lisp_procedure(fn) ||
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
    } else {
      CHECKV(is_cons(ll_item) || is_nil(ll_item), ll_item,
	     "Only symbols and nested lists supported in lambda lists.");
      count = (count + num_vars_in_ll(ll_item)) | 1;
    }
    ll = cdr(ll);
  }
  return count;
}

fn_uint destructure_lambda_list_into_frame(oop ll, oop args, oop frame,
                                           fn_uint idx) {
  while (is_cons(ll)) {
    oop ll_item = car(ll);
    if (is_symbol(ll_item)) {
      if (value_eq(ll_item, symbols._rest)) {
        ll = cdr(ll);
        oop value = args;
        frame_set_var(frame, idx, value);
        idx++;
        return idx;
      } else {
        CHECKV(is_cons(args), args, "Not enough arguments.");
        oop value = car(args);
        frame_set_var(frame, idx, value);
        idx++;
      }
    } else {
      CHECKV(is_cons(ll_item) || is_nil(ll_item), ll_item,
	     "Only symbols and nested lists supported in lambda lists.");

      oop arg_item = car(args);
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

oop make_frame_for_application(proc_t* proc, oop args, oop caller) {
  oop env = make_frame(proc, caller);
  destructure_lambda_list_into_frame(proc->lambda_list, args, env, 0);
  return env;
}

oop apply_compiled_lisp_procedure(proc_t* proc, oop args, oop caller) {
  return interpret(make_frame_for_application(proc, args, caller), proc);
}

oop current_native_procedure_caller = NIL;

// Convert an consed argument list into argv, argc form.
void extract_args(oop args, oop** out_argv, size_t* out_argc) {
  size_t length = length_int(args);
  size_t idx;
  *out_argc = length;
  *out_argv = (oop*)calloc(sizeof(oop), length);
  for (idx = 0; idx < length; idx++) {
    (*out_argv)[idx] = first(args);
    args = rest(args);
  }
}

oop apply_native_fn_directly(oop fn, oop* argv, size_t argc, oop caller) {
  function c_function = native_fn_function(fn);
  gc_protect_counter++;

  // TODO: Find a way to track C-level stack frames.  (This is slow!)
  current_native_procedure_caller = caller;
  oop result = c_function(argv, argc);

  current_native_procedure_caller = NIL;
  gc_protect_counter--;
  return result;
}

oop apply_native_fn(oop fn, oop args, oop caller) {
  size_t argc;
  oop* argv;

  extract_args(args, &argv, &argc);
  oop result = apply_native_fn_directly(fn, argv, argc, caller);
  free(argv);

  return result;
}

oop native_procedure_caller() {
  return current_native_procedure_caller;
}


// Debug printing.

void print_procedure(oop fn) {
  if (is_compiled_lisp_procedure(fn)) {
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


// Function application.
// First argument in values is function, rest are arguments.
// Caller is the calling function frame.
oop apply_with_caller(oop values, oop caller) {
  oop fn = car(values);
  oop result;
  if (is_compiled_lisp_procedure(fn)) {
    result = apply_compiled_lisp_procedure(to_proc(fn), cdr(values), caller);
  } else {
    CHECKV(is_native_procedure(fn), fn, "Must be a procedure for applying.");
    result = apply_native_fn(fn, cdr(values), caller);
  }
  return result;
}

// For convenience and top-level invocations.
oop apply(oop values) {
  return apply_with_caller(values, NIL);
}
