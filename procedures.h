
#ifndef __PROCEDURES_H__

#include "value.h"

//
// Lisp procedures.
//

// TODO: Trim down these exports.

// Makes a lisp procedure from a lambda list, its body and
// a captured environment.
extern oop make_procedure(oop lambda_list, oop body, oop env);

// Accessors for lambda list, body and captured environment.
extern oop fn_set_name(oop fn, oop name);

// Makes a compiled Lisp procedure.
extern oop make_compiled_procedure(oop lambda_list, oop code, oop env);

// Printing for debugging.
extern void print_procedure(oop fn);


//
// Native (C) procedures.
//

// Defines the type "function" to be oop --> oop.
// Who came up with this syntax?
typedef oop (*function)(oop args);

extern oop make_native_procedure(function c_function);

// Recognizing both kinds of procedures.
extern boolean is_procedure(oop fn);

// Function application.
extern oop apply(oop values);

#define __PROCEDURES_H__ 0
#endif // __PROCEDURES_H__
