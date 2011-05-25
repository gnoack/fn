
#include "stdio.h"

#include "primitives.h"

#include "carcdr.h"
#include "cons.h"
#include "eval.h"
#include "memory.h"
#include "strings.h"
#include "symbols.h"

void check_argument_number(oop args, int expected) {
  CHECKV(length_int(args) == expected, args, "Argument number");
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

/* UNSAFE */
oop primitive_mem_make(oop args) {
  uint size = length_int(args);
  oop result = mem_alloc(size);
  uint i;
  for (i = 0; i < size; i++) {
    mem_set(result, i, first(args));
    args = rest(args);
  }
  return result;
}

/* UNSAFE, except for index == 0. */
oop primitive_mem_get(oop args) {
  check_argument_number(args, 2);
  oop obj = first(args);
  oop index = cadr(args);
  return mem_get(obj, get_smallint(index));
}

/* UNSAFE, except for index == 0.
 * This is an imperative function.  Avoid.
 */
oop primitive_mem_set(oop args) {
  check_argument_number(args, 3);
  oop obj = car(args);
  oop index = cadr(args);
  oop value = caddr(args);
  return mem_set(obj, get_smallint(index), value);
}

oop primitive_char_to_num(oop args) {
  check_argument_number(args, 1);
  CHECKV(is_char(car(args)), car(args), "Must be a char");
  return make_smallint(get_char(car(args)));
}

oop primitive_num_to_char(oop args) {
  check_argument_number(args, 1);
  CHECKV(is_smallint(car(args)), car(args), "Must be a smallint");
  return make_char(get_smallint(car(args)));
}

oop primitive_string_to_symbol(oop args) {
  check_argument_number(args, 1);
  CHECKV(is_cons(car(args)) || is_nil(car(args)),
	 car(args), "Must be a string");
  return make_symbol(c_string(car(args)));
}

oop primitive_symbol_to_string(oop args) {
  check_argument_number(args, 1);
  CHECKV(is_symbol(car(args)), car(args), "Must be a symbol");
  return make_string(get_symbol(car(args)));
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
  CHECKV(is_smallint(car(args)), car(args), "Must be a number");
  CHECKV(is_smallint(cadr(args)), cadr(args), "Must be a number");
  return make_smallint(get_smallint(car(args)) - get_smallint(cadr(args)));
}

oop primitive_mul(oop args) {
  // TODO: Allow more arguments.
  check_argument_number(args, 2);
  CHECKV(is_smallint(car(args)), car(args), "Must be a number");
  CHECKV(is_smallint(cadr(args)), cadr(args), "Must be a number");
  return make_smallint(get_smallint(car(args)) * get_smallint(cadr(args)));
}

oop primitive_div(oop args) {
  check_argument_number(args, 2);
  CHECKV(is_smallint(car(args)), car(args), "Must be a number");
  CHECKV(is_smallint(cadr(args)), cadr(args), "Must be a number");
  return make_smallint(get_smallint(car(args)) / get_smallint(cadr(args)));
}

oop primitive_mod(oop args) {
  check_argument_number(args, 2);
  CHECKV(is_smallint(car(args)), car(args), "Must be a number");
  CHECKV(is_smallint(cadr(args)), cadr(args), "Must be a number");
  return make_smallint(get_smallint(car(args)) % get_smallint(cadr(args)));
}

oop lisp_bool(bool b) {
  return b ? symbols._true : symbols._false;
}

oop primitive_le(oop args) {
  check_argument_number(args, 2);
  CHECKV(is_smallint(car(args)), car(args), "Must be a number");
  CHECKV(is_smallint(cadr(args)), cadr(args), "Must be a number");
  return lisp_bool(get_smallint(car(args)) <= get_smallint(cadr(args)));
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

oop primitive_symbol_p(oop args) {
  check_argument_number(args, 1);
  return lisp_bool(is_symbol(car(args)));
}

oop primitive_list(oop args) {
  // Any argument number accepted, of course. :)
  return args;
}

oop primitive_apply(oop args) {
  check_argument_number(args, 2);
  return apply(make_cons(car(args), cadr(args)));
}

/* Attention: has side effect of printing, returns argument. */
oop primitive_write_out(oop args) {
  check_argument_number(args, 1);
  CHECKV(is_cons(car(args)) || is_nil(car(args)), car(args),
	 "Must be list of characters (string)");
  printf("%s\n", c_string(car(args)));
  return car(args);
}

void init_primitives() {
  register_globally_fn("cons", primitive_cons);
  register_globally_fn("first", primitive_first);
  register_globally_fn("rest", primitive_rest);
  register_globally_fn("$make", primitive_mem_make);
  register_globally_fn("$get", primitive_mem_get);
  register_globally_fn("$set", primitive_mem_set);
  register_globally_fn("char->num", primitive_char_to_num);
  register_globally_fn("num->char", primitive_num_to_char);
  register_globally_fn("string->symbol", primitive_string_to_symbol);
  register_globally_fn("symbol->string", primitive_symbol_to_string);
  register_globally_fn("+", primitive_add);
  register_globally_fn("-", primitive_sub);
  register_globally_fn("*", primitive_mul);
  register_globally_fn("/", primitive_div);
  register_globally_fn("mod", primitive_mod);
  register_globally_fn("integer<=", primitive_le);
  register_globally_fn("eq", primitive_eq);
  register_globally_fn("cons?", primitive_cons_p);
  register_globally_fn("char?", primitive_char_p);
  register_globally_fn("number?", primitive_number_p);
  register_globally_fn("symbol?", primitive_symbol_p);
  register_globally_fn("list", primitive_list);
  register_globally_fn("apply", primitive_apply);
  register_globally_fn("writeout", primitive_write_out);
}
