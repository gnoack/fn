
#ifndef _CONS_H_
#define _CONS_H_ 1

#include "value.h"

typedef struct cons_s {
  union value_u first;
  union value_u rest;
} cons_t;

extern
oop make_cons(oop car, oop cdr);

extern
oop first(oop cons);

extern
oop rest(oop cons);

extern
oop length(oop list);

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