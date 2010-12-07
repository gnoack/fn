
#ifndef _CONS_H_
#define _CONS_H_ 1

#include "value.h"

typedef struct cons_s {
  union value_u first;
  union value_u rest;
} cons_t;

extern
value_t make_cons(value_t car, value_t cdr);

extern
value_t first(value_t cons);

extern
value_t rest(value_t cons);

/** End this with nil. */
extern
value_t make_list(value_t first, ...);

#define LIST(...) make_list(__VA_ARGS__, NIL)

#endif // _CONS_H_
