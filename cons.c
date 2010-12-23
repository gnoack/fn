
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
  CHECK(is_cons(cons), "must be a cons for caring");
  return cons.cons->first;
}

extern
value_t rest(value_t cons) {
  CHECK(is_cons(cons), "must be a cons for cdring");
  return cons.cons->rest;
}

extern
unsigned int length_int(value_t list) {
  if (is_cons(list)) {
    return length_int(rest(list)) + 1;
  } else {
    CHECK(is_nil(list), "This is not a list");
    return 0;
  }
}

extern
value_t length(value_t list) {
  return make_uint(length_int(list));
}


value_t the_end_marker;
bool initialized_marker = NO;

extern
value_t end_marker() {
  if (!initialized_marker) {
    the_end_marker = make_string("hard to guess");
    initialized_marker = YES;
  }
  return the_end_marker;
}

bool is_end_marker(value_t v) {
  return value_eq(end_marker(), v);
}

extern
value_t make_list(value_t first, ...) {
  value_t firstcons = make_cons(first, NIL);
  value_t currcons = firstcons;

  va_list ap;
  va_start(ap, first);
  value_t arg = va_arg(ap, value_t);
  while (!is_end_marker(arg)) {
    currcons.cons->rest = make_cons(arg, NIL);
    currcons = currcons.cons->rest;

    arg = va_arg(ap, value_t);
  }
  va_end(ap);
  return firstcons;
}
