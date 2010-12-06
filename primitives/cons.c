
#include "malloc.h"
#include "cons.h"
#include "value.h"

extern
value_t make_cons(value_t car, value_t cdr) {
  value_t cons;
  cons.cons = malloc(sizeof(cons_t));
  cons.cons->first = car;
  cons.cons->rest = cdr;
  return cons;
}

extern
value_t first(value_t cons) {
  return cons.cons->first;
}

extern
value_t rest(value_t cons) {
  return cons.cons->rest;
}

