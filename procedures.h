
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
extern oop fn_name(oop fn);
extern oop fn_set_name(oop fn, oop name);

// True if it's a lisp procedure.
extern boolean is_lisp_procedure(oop fn);



//
// Native (C) procedures.
//

// Defines the type "function" to be oop --> oop.
// Who came up with this syntax?
typedef oop (*function)(oop args);

extern oop make_native_fn(function c_function);
extern boolean is_native_procedure(oop fn);
extern oop native_fn_apply(oop fn, oop args);

// Recognizing both kinds of procedures.
extern boolean is_procedure(oop fn);

// Function application.
extern oop apply(oop values);

#define __PROCEDURES_H__ 0
#endif // __PROCEDURES_H__
