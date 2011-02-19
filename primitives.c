
#include "primitives.h"
#include "eval.h"
#include "cons.h"
#include "carcdr.h"

void check_argument_number(oop args, int expected) {
  CHECK(length_int(args) == expected, "Argument number");
}

oop primitive_cons(oop args) {
  check_argument_number(args, 2);
  return make_cons(car(args), cadr(args));
}

oop primitive_first(oop args) {
  check_argument_number(args, 1);
  return first(car(args));
}

oop primitive_rest(oop args) {
  check_argument_number(args, 1);
  return rest(car(args));
}

oop primitive_char_to_num(oop args) {
  check_argument_number(args, 1);
  CHECK(is_char(car(args)), "Must be a char");
  return make_smallint(get_char(car(args)));
}

oop primitive_num_to_char(oop args) {
  check_argument_number(args, 1);
  CHECK(is_smallint(car(args)), "Must be a smallint");
  return make_char(get_smallint(car(args)));
}

void init_primitives() {
  register_globally_fn("cons", primitive_cons);
  register_globally_fn("first", primitive_first);
  register_globally_fn("rest", primitive_rest);
  register_globally_fn("char->num", primitive_char_to_num);
  register_globally_fn("num->char", primitive_num_to_char);
}
