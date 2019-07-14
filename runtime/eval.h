#ifndef _EVAL_H_
#define _EVAL_H_

#include "procedures.h"
#include "value.h"

// Global namespace.
extern oop global_env;

bool is_global_env(oop v);

void register_globally(const char* name, oop value);
void register_globally_fn(const char* name, function fn);
oop lookup_globally(symbol_t* key);
// Avoid.  Use lookup_globally() to lookup values.
oop lookup_var_object_globally(symbol_t* key);

void init_eval();
void load_decls(oop decls);

#endif  // _EVAL_H_
