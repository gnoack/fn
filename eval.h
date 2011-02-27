
#ifndef __EVAL_H__

#include "procedures.h"
#include "value.h"

// Global namespace.
extern
oop global_env;

extern
oop eval_global(oop program);

extern
oop eval(oop program, oop env);

extern
void register_globally_fn(const char* name, function fn);

extern
void init_eval();

extern
void load_decls(oop decls);

#define __EVAL_H__ 0
#endif // __EVAL_H__
