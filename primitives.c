
#include "stdio.h"

#include "primitives.h"

#include "eval.h"
#include "cons.h"
#include "symbols.h"
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

// Integer addition.
oop primitive_add(oop args) {
  // TODO: Pretty inaccurate. This works on smallints only.
  uint i = 0;
  while (!is_nil(args)) {
    oop arg = car(args);
    // TODO: How to do proper in-lisp error handling?
    if (!is_smallint(arg)) {
      printf("Warning: Trying to int-add bad value: ");
      print_value(arg);
    }
    i += get_smallint(arg);
    args = cdr(args);
  }
  return make_smallint(i);
}

// TODO: This gives very wrong values, because we don't
//   support negative numbers yet..
oop primitive_sub(oop args) {
  // TODO: Allow more arguments.
  check_argument_number(args, 2);
  CHECK(is_smallint(car(args)), "Must be a number");
  CHECK(is_smallint(cadr(args)), "Must be a number");
  return make_smallint(get_smallint(car(args)) - get_smallint(cadr(args)));
}

oop primitive_mul(oop args) {
  // TODO: Allow more arguments.
  check_argument_number(args, 2);
  CHECK(is_smallint(car(args)), "Must be a number");
  CHECK(is_smallint(cadr(args)), "Must be a number");
  return make_smallint(get_smallint(car(args)) * get_smallint(cadr(args)));
}

oop lisp_bool(bool b) {
  return b ? symbols._true : symbols._false;
}

oop primitive_eq(oop args) {
  check_argument_number(args, 2);
  return lisp_bool(value_eq(car(args), cadr(args)));
}

oop primitive_cons_p(oop args) {
  check_argument_number(args, 1);
  return lisp_bool(is_cons(car(args)));
}

oop primitive_char_p(oop args) {
  check_argument_number(args, 1);
  return lisp_bool(is_char(car(args)));
}

oop primitive_number_p(oop args) {
  check_argument_number(args, 1);
  return lisp_bool(is_smallint(car(args)));
}

oop primitive_list(oop args) {
  // Any argument number accepted, of course. :)
  return args;
}

void init_primitives() {
  register_globally_fn("cons", primitive_cons);
  register_globally_fn("first", primitive_first);
  register_globally_fn("rest", primitive_rest);
  register_globally_fn("char->num", primitive_char_to_num);
  register_globally_fn("num->char", primitive_num_to_char);
  register_globally_fn("+", primitive_add);
  register_globally_fn("-", primitive_sub);
  register_globally_fn("*", primitive_mul);
  register_globally_fn("eq", primitive_eq);
  register_globally_fn("cons?", primitive_cons_p);
  register_globally_fn("char?", primitive_char_p);
  register_globally_fn("number?", primitive_number_p);
  register_globally_fn("list", primitive_list);
}
