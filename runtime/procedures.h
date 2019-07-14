#ifndef _PROCEDURES_H_
#define _PROCEDURES_H_

#include "value.h"
#include "interpreter.h"

// Defines the type "function".
typedef oop (*function)(oop* argv, size_t argc);

// Bytecode procedures.
typedef struct procedure {
  oop type;
  oop mutable_name;  // The only mutable field.
  oop lambda_list;
  oop numbers;
  frame_t* env;
  oop bytecode;
  oop ip;
  oop oop_table;
} proc_t;

// code is bytecode (see compiler.fn), env is the lexical environment frame.
proc_t* make_compiled_procedure(oop lambda_list, frame_t* env,
                                oop bytecode, oop ip, oop oop_lookup_table,
                                fn_uint max_stack_size);

// Construct a low-level, C-implemented procedure.
oop make_native_procedure(function c_function);

/*
 * More.
 */

// Set a procedure's stored name, once per procedure.
oop procedure_set_name(oop fn, oop name);

void print_procedure(oop fn);
bool is_procedure(oop fn);

// TODO: Better name.
frame_t* make_frame_for_application(proc_t* proc, oop args, frame_t* caller);
bool is_compiled_lisp_procedure(oop fn);
bool is_native_procedure(oop fn);
oop apply(oop values);
oop apply_native_fn(oop fn, oop args, frame_t* caller);
oop apply_native_fn_directly(oop fn, oop* argv, size_t argc, frame_t* caller);
oop apply_compiled_lisp_procedure(proc_t* proc, oop args, frame_t* caller);
oop fn_name(oop fn);

static inline fn_uint proc_num_fixargs(proc_t* p) {
  return (get_smallint(p->numbers) & 0xffff) >> 1;
}

static inline fn_uint proc_max_stack_depth(proc_t* p) {
  return get_smallint(p->numbers) >> 16;
}

// 1 if the function had varargs.
static inline fn_uint proc_has_varargs(proc_t* p) {
  return get_smallint(p->numbers) & 1;
}

// The caller of the currently executing native procedure.
frame_t* native_procedure_caller();

void init_procedures();

#endif  // _PROCEDURES_H_
