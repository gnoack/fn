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

oop make_cons(oop car, oop cdr) {
  cons_t* cons = MemAlloc(cons_t);
  *cons = (cons_t) {
    .type = symbols._cons,
    .car  = car,
    .cdr  = cdr,
  };
  return cons_to_oop(cons);
}

bool is_cons(oop v) {
  return is_mem(v) && value_eq(symbols._cons, MEM_GET(v, 0));
}

oop first(oop cons) {
  CHECKV(is_cons(cons), cons, "must be a cons for caring");
  return to_cons(cons)->car;
}

oop rest(oop cons) {
  CHECKV(is_cons(cons), cons, "must be a cons for cdring");
  return to_cons(cons)->cdr;
}

unsigned int length_int(oop list) {
  unsigned int result = 0;
  while (is_cons(list)) {
    list = to_cons(list)->cdr;
    result ++;
  }
  CHECKV(is_nil(list), list, "List must be nil-terminated.");
  return result;
}

// It's just a value that hopefully won't ever be a valid oop.
static oop the_end_marker = {.smallint = (~1)};

oop end_marker() {
  return the_end_marker;
}

static bool is_end_marker(oop v) {
  return value_eq(end_marker(), v);
}

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
