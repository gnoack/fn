
#include "value.h"

typedef struct cons_s {
  value_t first;
  value_t rest;
} cons_t;

extern
cons_t* make_cons(value_t car, value_t cdr);

extern
value_t first(cons_t* cons);

extern
value_t rest(cons_t* cons);
