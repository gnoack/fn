#ifndef _PRIMITIVES_H_

#include "value.h"

#define PARSE_ONE_ARG(first_arg)                                 \
  oop first_arg = first(args); args = rest(args);                \
  CHECKV(is_nil(args), args, "More than 1 argument provided.");  \

#define PARSE_TWO_ARGS(first_arg, second_arg)                    \
  oop first_arg = first(args); args = rest(args);                \
  oop second_arg = first(args); args = rest(args);               \
  CHECKV(is_nil(args), args, "More than 2 argument provided.");  \

#define PARSE_THREE_ARGS(first_arg, second_arg, third_arg)       \
  oop first_arg = first(args); args = rest(args);                \
  oop second_arg = first(args); args = rest(args);               \
  oop third_arg = first(args); args = rest(args);                \
  CHECKV(is_nil(args), args, "More than 3 argument provided.");  \

#define PARSE_FOUR_ARGS(first_arg, second_arg, third_arg, fourth_arg)   \
  oop first_arg = first(args); args = rest(args);                       \
  oop second_arg = first(args); args = rest(args);                      \
  oop third_arg = first(args); args = rest(args);                       \
  oop fourth_arg = first(args); args = rest(args);                      \
  CHECKV(is_nil(args), args, "More than 4 argument provided.");         \

#define PARSE_FIVE_ARGS(first_arg, second_arg, third_arg, fourth_arg, fifth_arg) \
  oop first_arg = first(args); args = rest(args);                       \
  oop second_arg = first(args); args = rest(args);                      \
  oop third_arg = first(args); args = rest(args);                       \
  oop fourth_arg = first(args); args = rest(args);                      \
  oop fifth_arg = first(args); args = rest(args);                       \
  CHECKV(is_nil(args), args, "More than 5 argument provided.");         \

#define UNARY_PREDICATE(name, c_tester) \
oop name(oop args) {                    \
  PARSE_ONE_ARG(value);                 \
  return lisp_bool(c_tester(value));    \
}

extern void init_primitives();
extern oop lisp_bool(boolean b);

#define _PRIMITIVES_H_ 0
#endif  // _PRIMITIVES_H_
