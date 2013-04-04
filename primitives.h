#ifndef _PRIMITIVES_H_

#include "value.h"
#include "carcdr.h"

// TODO: Remove unnecessary list traversal.

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

#define PARSE_FOUR_ARGS(first_arg, second_arg, third_arg, fourth_arg) \
  check_argument_number(args, 4);   \
  oop first_arg = first(args);      \
  oop second_arg = cadr(args);      \
  oop third_arg = caddr(args);      \
  oop fourth_arg = cadddr(args);    \

#define PARSE_FIVE_ARGS(first_arg, second_arg, third_arg, fourth_arg, fifth_arg) \
  check_argument_number(args, 5);   \
  oop first_arg = first(args);      \
  oop second_arg = cadr(args);      \
  oop third_arg = caddr(args);      \
  oop fourth_arg = cadddr(args);    \
  oop fifth_arg = caddddr(args);    \

#define UNARY_PREDICATE(name, c_tester) \
oop name(oop args) {                    \
  PARSE_ONE_ARG(value);                 \
  return lisp_bool(c_tester(value));    \
}

extern void check_argument_number(oop args, int expected);
extern void init_primitives();
extern oop lisp_bool(boolean b);

#define _PRIMITIVES_H_ 0
#endif  // _PRIMITIVES_H_
