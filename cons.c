
#include <stdarg.h>
#include <stdio.h>

#include "malloc.h"
#include "cons.h"
#include "value.h"

extern
oop make_cons(oop car, oop cdr) {
  oop cons;
  cons.cons = malloc(sizeof(cons_t));
  cons.cons->first = car;
  cons.cons->rest = cdr;
  return cons;
}

extern
oop first(oop cons) {
  CHECK(is_cons(cons), "must be a cons for caring");
  return cons.cons->first;
}

extern
oop rest(oop cons) {
  CHECK(is_cons(cons), "must be a cons for cdring");
  return cons.cons->rest;
}

extern
void set_rest(oop cons, oop value) {
  CHECK(is_cons(cons), "must be a cons for setting the cdr");
  cons.cons->rest = value;
}

extern
void set_first(oop cons, oop value) {
  CHECK(is_cons(cons), "must be a cons for setting the car");
  cons.cons->first = value;
}

extern
unsigned int length_int(oop list) {
  if (is_cons(list)) {
    return length_int(rest(list)) + 1;
  } else {
    CHECK(is_nil(list), "This is not a list");
    return 0;
  }
}

extern
oop length(oop list) {
  return make_smallint(length_int(list));
}


oop the_end_marker;
bool initialized_marker = NO;

extern
oop end_marker() {
  if (!initialized_marker) {
    the_end_marker = make_symbol("hard to guess");
    initialized_marker = YES;
  }
  return the_end_marker;
}

bool is_end_marker(oop v) {
  return value_eq(end_marker(), v);
}

extern
oop make_list(oop first, ...) {
  oop firstcons = make_cons(first, NIL);
  oop currcons = firstcons;

  va_list ap;
  va_start(ap, first);
  oop arg = va_arg(ap, oop);
  while (!is_end_marker(arg)) {
    currcons.cons->rest = make_cons(arg, NIL);
    currcons = currcons.cons->rest;

    arg = va_arg(ap, oop);
  }
  va_end(ap);
  return firstcons;
}
