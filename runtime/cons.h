#ifndef _CONS_H_
#define _CONS_H_

#include "value.h"
#include <stdbool.h>

extern
oop make_cons(oop car, oop cdr);

extern
bool is_cons(oop v);

extern
oop first(oop cons);

extern
oop rest(oop cons);

extern
unsigned int length_int(oop list);

/** The end marker value is guaranteed to be hard to guess. */
extern
oop end_marker();

/** End this with the value returned by end_marker. */
extern
oop make_list(oop first, ...);

#define LIST(...) make_list(__VA_ARGS__, end_marker())

#endif // _CONS_H_
