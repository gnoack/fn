#ifndef _VARS_H_
#define _VARS_H_

#include "value.h"

// A 'var' is a small object containing the value of a variable and
// the variable name.  The purpose of var objects is to save hash
// table lookups when referencing variable definitions in the global
// environment.  Instead of pointing to values directly, the global
// environment points to a var object that holds the value.  When a
// compiled function refers to the variable, it doesn't need to look
// it up in the global environment any more, but can simply look it up
// in the var object.

oop make_undefined_var(symbol_t* symbol);
bool is_var(oop var);
bool is_set_var(oop var);
void var_set(oop var, oop value);
void var_unset(oop var);
oop var_get(oop var);
oop var_get_unchecked(oop var);
symbol_t* var_name(oop var);
void print_var(oop var);

#endif  // _VARS_H_
