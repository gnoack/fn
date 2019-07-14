#ifndef _CONS_H_
#define _CONS_H_

#include "value.h"
#include <stdbool.h>

oop make_cons(oop car, oop cdr);
bool is_cons(oop v);
oop first(oop cons);
oop rest(oop cons);
unsigned int length_int(oop list);

// End this with the value returned by end_marker.
oop make_list(oop first, ...);

// The end marker value is guaranteed to be hard to guess.
oop end_marker();

// Helper macro to define a cons list from C.
#define LIST(...) make_list(__VA_ARGS__, end_marker())

#endif // _CONS_H_
