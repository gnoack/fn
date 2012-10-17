
#ifndef __PROCEDURES_H__

#include "value.h"
#include "interpreter.h"

// Defines the type "function" to be oop --> oop.
typedef oop (*function)(oop args);


/*
 * Construction: Make Lisp procedures from a lambda list, a body and a
 * lexical environment.
 */

// body is a list of expressions, env is a @dframe.
extern oop make_procedure(oop lambda_list, oop body, oop env);

// code is a code structure (see compiler.fn), env is a @frame.
extern oop make_compiled_procedure(oop lambda_list, oop code, oop env);

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
extern oop make_frame_for_application(oop cfn, oop args);
extern boolean is_compiled_lisp_procedure(oop fn);
extern oop apply(oop values);
extern oop fn_code(oop cfn);
extern oop fn_env(oop fn);
extern fn_uint fn_argnum(oop fn);
extern boolean fn_nested_args(oop fn);

// Apply stack.
extern void apply_stack_push(oop values);
extern void apply_stack_pop();

extern void init_procedures();

#define __PROCEDURES_H__ 0
#endif // __PROCEDURES_H__
