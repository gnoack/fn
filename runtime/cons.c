#include <stdarg.h>
#include <stdio.h>

#include "cons.h"
#include "debug.h"
#include "gc.h"
#include "memory.h"
#include "symbols.h"
#include "value.h"

typedef struct cons {
  oop type;
  oop car;
  oop cdr;
} cons_t;

CONVERTERS(cons_t, cons);

extern
oop make_cons(oop car, oop cdr) {
  cons_t* cons = MemAlloc(cons_t);
  *cons = (cons_t) {
    .type = symbols._cons,
    .car  = car,
    .cdr  = cdr
  };
  return cons_to_oop(cons);
}

extern
boolean is_cons(oop v) {
  return TO_BOOL(is_mem(v) && value_eq(symbols._cons, MEM_GET(v, 0)));
}

extern
oop first(oop cons) {
  CHECKV(is_cons(cons), cons, "must be a cons for caring");
  return to_cons(cons)->car;
}

extern
oop rest(oop cons) {
  CHECKV(is_cons(cons), cons, "must be a cons for cdring");
  return to_cons(cons)->cdr;
}

extern
unsigned int length_int(oop list) {
  unsigned int result = 0;
  while (is_cons(list)) {
    list = to_cons(list)->cdr;
    result ++;
  }
  CHECKV(is_nil(list), list, "List must be nil-terminated.");
  return result;
}


oop the_end_marker;
boolean initialized_marker = NO;

void enumerate_end_marker_root(void (*accept)(oop* place)) {
  accept(&the_end_marker);
}

extern
oop end_marker() {
  if (!initialized_marker) {
    the_end_marker = symbol_to_oop(make_symbol("hard to guess"));
    gc_register_persistent_refs(enumerate_end_marker_root);
    initialized_marker = YES;
  }
  return the_end_marker;
}

boolean is_end_marker(oop v) {
  return value_eq(end_marker(), v);
}

extern
oop make_list(oop first, ...) {
  cons_t* firstcons = to_cons(make_cons(first, NIL));
  cons_t* currcons = firstcons;

  va_list ap;
  va_start(ap, first);
  oop arg = va_arg(ap, oop);
  while (!is_end_marker(arg)) {
    oop nextcons = make_cons(arg, NIL);
    currcons->cdr = nextcons;
    currcons = to_cons(nextcons);

    arg = va_arg(ap, oop);
  }
  va_end(ap);
  return cons_to_oop(firstcons);
}
