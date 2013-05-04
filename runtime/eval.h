#ifndef _EVAL_H_
#define _EVAL_H_

#include "procedures.h"
#include "value.h"

// Global namespace.
extern oop global_env;

extern oop eval_global(oop program);

extern oop eval(oop program, oop env);

extern void register_globally(const char* name, oop value);

extern void register_globally_fn(const char* name, function fn);

extern void set_globally_oop(oop key, oop value);

extern oop lookup_globally(oop key);

extern boolean is_global_env(oop v);

extern void init_eval();

extern void load_decls(oop decls);

#endif  // _EVAL_H_
