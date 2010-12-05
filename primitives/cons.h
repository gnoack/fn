
#ifndef _CONS_H_
#define _CONS_H_ 1

#include "value.h"

typedef struct cons_s {
  union value_u first;
  union value_u rest;
} cons_t;

extern
cons_t* make_cons(value_t car, value_t cdr);

extern
value_t first(cons_t* cons);

extern
value_t rest(cons_t* cons);

#endif // _CONS_H_
