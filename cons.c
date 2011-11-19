
#include <stdarg.h>
#include <stdio.h>

#include "memory.h"
#include "cons.h"
#include "value.h"
#include "symbols.h"

extern
oop make_cons(oop car, oop cdr) {
  oop cons = mem_alloc(3);
  mem_set(cons, 0, symbols._cons);
  mem_set(cons, 1, car);
  mem_set(cons, 2, cdr);
  return cons;
}

extern
boolean is_cons(oop v) {
  if (!is_mem(v)) {
    return NO;
  }
  return value_eq(symbols._cons, mem_get(v, 0));
}

extern
oop first(oop cons) {
  CHECKV(is_cons(cons), cons, "must be a cons for caring");
  return mem_get(cons, 1);
}

extern
oop rest(oop cons) {
  CHECKV(is_cons(cons), cons, "must be a cons for cdring");
  return mem_get(cons, 2);
}

extern
void set_rest(oop cons, oop value) {
  CHECKV(is_cons(cons), cons, "must be a cons for setting the cdr");
  mem_set(cons, 2, value);
}

extern
void set_first(oop cons, oop value) {
  CHECKV(is_cons(cons), cons, "must be a cons for setting the car");
  mem_set(cons, 1, value);
}

extern
unsigned int length_int(oop list) {
  if (is_cons(list)) {
    return length_int(rest(list)) + 1;
  } else {
    CHECKV(is_nil(list), list, "Was expecting nil.");
    return 0;
  }
}

extern
oop length(oop list) {
  return make_smallint(length_int(list));
}


oop the_end_marker;
boolean initialized_marker = NO;

extern
oop end_marker() {
  if (!initialized_marker) {
    the_end_marker = make_symbol("hard to guess");
    initialized_marker = YES;
  }
  return the_end_marker;
}

boolean is_end_marker(oop v) {
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
    oop nextcons = make_cons(arg, NIL);
    set_rest(currcons, nextcons);
    currcons = nextcons;

    arg = va_arg(ap, oop);
  }
  va_end(ap);
  return firstcons;
}
