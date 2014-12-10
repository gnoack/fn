#ifndef _EVAL_H_
#define _EVAL_H_

#include "procedures.h"
#include "value.h"

// Global namespace.
extern oop global_env;

extern oop eval_global(oop program);

extern void register_globally(const char* name, oop value);

extern void register_globally_fn(const char* name, function fn);

extern oop lookup_globally(symbol_t* key);

// Avoid.  Use lookup_globally() to lookup values.
extern oop lookup_var_object_globally(symbol_t* key);

extern boolean is_global_env(oop v);

extern void init_eval();

extern void load_decls(oop decls);

#endif  // _EVAL_H_
