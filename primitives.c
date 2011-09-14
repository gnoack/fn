
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

#define PARSE_ONE_ARG(first_arg)    \
  check_argument_number(args, 1);   \
  oop first_arg = first(args);      \

#define PARSE_TWO_ARGS(first_arg, second_arg) \
  check_argument_number(args, 2);   \
  oop first_arg = first(args);      \
  oop second_arg = cadr(args);      \

#define PARSE_THREE_ARGS(first_arg, second_arg, third_arg) \
  check_argument_number(args, 3);   \
  oop first_arg = first(args);      \
  oop second_arg = cadr(args);      \
  oop third_arg = caddr(args);      \

oop primitive_first(oop args) {
  PARSE_ONE_ARG(cons_cell);
  return first(cons_cell);
}

oop primitive_rest(oop args) {
  PARSE_ONE_ARG(cons_cell);
  return rest(cons_cell);
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
  PARSE_TWO_ARGS(obj, index);
  return mem_get(obj, get_smallint(index));
}

/* UNSAFE, except for index == 0.
 * This is an imperative function.  Avoid.
 */
oop primitive_mem_set(oop args) {
  PARSE_THREE_ARGS(obj, index, value);
  return mem_set(obj, get_smallint(index), value);
}

oop primitive_char_to_num(oop args) {
  PARSE_ONE_ARG(c);
  CHECKV(is_char(c), c, "Must be a char");
  return make_smallint(get_char(c));
}

oop primitive_num_to_char(oop args) {
  PARSE_ONE_ARG(i);
  CHECKNUMBER(i);
  return make_char(get_smallint(i));
}

oop primitive_string_to_symbol(oop args) {
  PARSE_ONE_ARG(str);
  CHECKV(is_cons(str) || is_nil(str), str, "Must be a string");
  return make_symbol(c_string(str));
}

oop primitive_symbol_to_string(oop args) {
  PARSE_ONE_ARG(sym);
  CHECKV(is_symbol(sym), sym, "Must be a symbol");
  return make_string(get_symbol(sym));
}

// Integer addition.
oop primitive_add(oop args) {
  // TODO: Pretty inaccurate. This works on smallints only.
  uint i = 0;
  for (; !is_nil(args); args = cdr(args)) {
    oop arg = car(args);
    CHECKV(is_smallint(arg), arg, "Only smallints can be added for now.");
    i += get_smallint(arg);
  }
  return make_smallint(i);
}

// TODO: This gives very wrong values, because we don't
//   support negative numbers yet..
oop primitive_sub(oop args) {
  // TODO: Allow more arguments.
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) - get_smallint(b));
}

oop primitive_mul(oop args) {
  // TODO: Allow more arguments.
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) * get_smallint(b));
}

oop primitive_div(oop args) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) / get_smallint(b));
}

oop primitive_mod(oop args) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) % get_smallint(b));
}

// Helper function for predicate primitives.
oop lisp_bool(bool b) {
  return b ? symbols._true : symbols._false;
}

oop primitive_le(oop args) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return lisp_bool(get_smallint(a) <= get_smallint(b));
}

oop primitive_eq(oop args) {
  PARSE_TWO_ARGS(a, b);
  return lisp_bool(value_eq(a, b));
}

#define UNARY_PREDICATE(name, c_tester) \
oop name(oop args) {     \
  PARSE_ONE_ARG(value);                 \
  return lisp_bool(c_tester(value));    \
}

UNARY_PREDICATE(primitive_mem_p, is_mem);
UNARY_PREDICATE(primitive_char_p, is_char);
UNARY_PREDICATE(primitive_number_p, is_smallint);
UNARY_PREDICATE(primitive_symbol_p, is_symbol);

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

oop primitive_kill_lisp(oop args) {
  PARSE_ONE_ARG(exit_status);
  CHECKNUMBER(exit_status);
  exit(get_smallint(exit_status));
}

void init_primitives() {
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
  register_globally_fn("mem?", primitive_mem_p);
  register_globally_fn("char?", primitive_char_p);
  register_globally_fn("number?", primitive_number_p);
  register_globally_fn("symbol?", primitive_symbol_p);
  register_globally_fn("list", primitive_list);
  register_globally_fn("apply", primitive_apply);
  register_globally_fn("writeout", primitive_write_out);
  register_globally_fn("kill-lisp", primitive_kill_lisp);
}
