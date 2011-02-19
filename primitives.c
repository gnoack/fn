
#include "primitives.h"
#include "eval.h"
#include "cons.h"
#include "carcdr.h"

oop primitive_cons(oop args) {
  CHECK(length_int(args) == 2, "Needs exactly 2 arguments.");
  return make_cons(car(args), cadr(args));
}

oop primitive_first(oop args) {
  CHECK(length_int(args) == 1, "Needs exactly one argument.");
  return first(car(args));
}

oop primitive_rest(oop args) {
  CHECK(length_int(args) == 1, "Needs exactly one argument.");
  return rest(car(args));
}

void init_primitives() {
  register_globally_fn("cons", primitive_cons);
  register_globally_fn("first", primitive_first);
  register_globally_fn("rest", primitive_rest);
}
