
#include "malloc.h"
#include "cons.h"
#include "value.h"

extern
cons_t* make_cons(value_t car, value_t cdr) {
  cons_t* cons = malloc(sizeof(cons_t));
  cons->first = car;
  cons->rest = cdr;
  return cons;
}

extern
value_t first(cons_t* cons) {
  return cons->first;
}

extern
value_t rest(cons_t* cons) {
  return cons->rest;
}

