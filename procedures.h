
#ifndef __PROCEDURES_H__

#include "value.h"

//
// Lisp procedures.
//

// Makes a lisp procedure from a lambda list, its body and
// a captured environment.
extern oop make_procedure(oop lambda_list, oop body, oop env);

// Accessors for lambda list, body and captured environment.
extern oop fn_lambda_list(oop fn);
extern oop fn_body(oop fn);
extern oop fn_env(oop fn);

// True if it's a lisp procedure.
extern bool is_lisp_procedure(oop fn);



//
// Native (C) procedures.
//

// Defines the type "function" to be oop --> oop.
// Who came up with this syntax?
typedef oop (*function)(oop args);

extern oop make_native_fn(function c_function);
extern bool is_native_fn(oop fn);
extern oop native_fn_apply(oop fn, oop args);


#define __PROCEDURES_H__ 0
#endif // __PROCEDURES_H__