
#include <stdarg.h>
#include <stdio.h>

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

extern
value_t make_list(value_t first, ...) {
  value_t firstcons = make_cons(first, NIL);
  value_t currcons = firstcons;

  va_list ap;
  va_start(ap, first);
  value_t arg = va_arg(ap, value_t);
  while (!is_nil(arg)) {
    currcons.cons->rest = make_cons(arg, NIL);
    currcons = currcons.cons->rest;

    arg = va_arg(ap, value_t);
  }
  va_end(ap);
  return firstcons;
}
