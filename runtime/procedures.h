#ifndef _PROCEDURES_H_
#define _PROCEDURES_H_

#include "value.h"
#include "interpreter.h"

// Defines the type "function" to be oop --> oop.
typedef oop (*function)(oop args);


/*
 * Construction: Make Lisp procedures from a lambda list, a body and a
 * lexical environment.
 */

// body is a list of expressions, env is a Dframe.
extern oop make_procedure(oop lambda_list, oop body, oop env);

// code is bytecode (see compiler.fn), env is a Frame.
extern oop make_compiled_procedure(oop lambda_list, oop env,
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
extern oop make_frame_for_application(oop cfn, oop args, oop caller);
extern oop make_dframe_for_application(oop lfn, oop args, oop caller);
extern boolean is_compiled_lisp_procedure(oop fn);
extern boolean is_lisp_procedure(oop fn);
extern oop apply(oop values);
extern oop apply_with_caller(oop values, oop caller);
extern oop fn_name(oop fn);
extern oop fn_code(oop cfn);
extern oop fn_env(oop fn);
extern fn_uint fn_argnum(oop fn);
extern boolean fn_nested_args(oop fn);
extern fn_uint fn_max_stack_depth(oop fn);

#define CFN_CODE         5
#define CFN_IP           6
#define CFN_LOOKUP_TABLE 7

// The caller of the currently executing native procedure.
extern oop native_procedure_caller();

#endif  // _PROCEDURES_H_
