
#ifndef __EVAL_H__

#import "value.h"

// Global namespace.
extern
oop global_env;

extern
oop eval_global(oop program);

extern
oop eval(oop program, oop env);

extern
void init_eval();

#define __EVAL_H__ 0
#endif // __EVAL_H__
