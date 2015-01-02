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
extern proc_t* make_compiled_procedure(oop lambda_list, frame_t* env,
				       oop bytecode, oop ip, oop oop_lookup_table,
				       fn_uint max_stack_size);

// Construct a low-level, C-implemented procedure.
extern oop make_native_procedure(function c_function);

/*
 * More.
 */

// Set a procedure's stored name, maybe.
extern oop procedure_set_name(oop fn, oop name);

extern void print_procedure(oop fn);
extern boolean is_procedure(oop fn);

// TODO: Better name.
extern frame_t* make_frame_for_application(proc_t* proc, oop args, frame_t* caller);
extern boolean is_compiled_lisp_procedure(oop fn);
extern boolean is_native_procedure(oop fn);
extern oop apply(oop values);
extern oop apply_native_fn(oop fn, oop args, frame_t* caller);
extern oop apply_native_fn_directly(oop fn, oop* argv, size_t argc, frame_t* caller);
extern oop apply_compiled_lisp_procedure(proc_t* proc, oop args, frame_t* caller);
extern oop fn_name(oop fn);

static inline fn_uint proc_argnum(proc_t* p) {
  return (get_smallint(p->numbers) & 0xffff) >> 1;
}

static inline fn_uint proc_max_stack_depth(proc_t* p) {
  return get_smallint(p->numbers) >> 16;
}

static inline boolean proc_nested_args(proc_t* p) {
  return TO_BOOL(get_smallint(p->numbers) & 1);
}

// The caller of the currently executing native procedure.
extern frame_t* native_procedure_caller();

extern void init_procedures();

#endif  // _PROCEDURES_H_
