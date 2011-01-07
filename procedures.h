
#ifndef __PROCEDURES_H__

#include "value.h"

// Makes a lisp procedure from a lambda list, its body and
// a captured environment.
extern oop make_procedure(oop lambda_list, oop body, oop env);

// Accessors for lambda list, body and captured environment.
extern oop fn_lambda_list(oop fn);
extern oop fn_body(oop fn);
extern oop fn_env(oop fn);

// True if it's a lisp procedure.
extern bool is_lisp_procedure(oop fn);

#define __PROCEDURES_H__ 0
#endif // __PROCEDURES_H__
