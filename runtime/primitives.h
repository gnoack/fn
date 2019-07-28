#ifndef _PRIMITIVES_H_
#define _PRIMITIVES_H_

#include "value.h"

#define FUNC(name) oop name(oop* argv, size_t argc)

#define PARSE_ONE_ARG(first_arg)                                 \
  CHECK(argc == 1, "Need exactly 1 argument.");                  \
  oop first_arg = argv[0];                                       \

#define PARSE_TWO_ARGS(first_arg, second_arg)                    \
  CHECK(argc == 2, "Need exactly 2 arguments.");                 \
  oop first_arg = argv[0];                                       \
  oop second_arg = argv[1];                                      \

#define PARSE_THREE_ARGS(first_arg, second_arg, third_arg)       \
  CHECK(argc == 3, "Need exactly 3 arguments.");                 \
  oop first_arg = argv[0];                                       \
  oop second_arg = argv[1];                                      \
  oop third_arg = argv[2];                                       \

#define PARSE_FOUR_ARGS(first_arg, second_arg, third_arg, fourth_arg) \
  CHECK(argc == 4, "Need exactly 4 arguments.");                 \
  oop first_arg = argv[0];                                       \
  oop second_arg = argv[1];                                      \
  oop third_arg = argv[2];                                       \
  oop fourth_arg = argv[3];                                      \

#define PARSE_FIVE_ARGS(first_arg, second_arg, third_arg, fourth_arg, fifth_arg) \
  CHECK(argc == 5, "Need exactly 5 arguments.");                 \
  oop first_arg = argv[0];                                       \
  oop second_arg = argv[1];                                      \
  oop third_arg = argv[2];                                       \
  oop fourth_arg = argv[3];                                      \
  oop fifth_arg = argv[4];                                       \

#define PARSE_SIX_ARGS(first_arg, second_arg, third_arg, fourth_arg, fifth_arg, sixth_arg) \
  CHECK(argc == 6, "Need exactly 6 arguments.");                 \
  oop first_arg = argv[0];                                       \
  oop second_arg = argv[1];                                      \
  oop third_arg = argv[2];                                       \
  oop fourth_arg = argv[3];                                      \
  oop fifth_arg = argv[4];                                       \
  oop sixth_arg = argv[5];                                       \

#define PARSE_SEVEN_ARGS(first_arg, second_arg, third_arg, fourth_arg, fifth_arg, sixth_arg, seventh_arg) \
  CHECK(argc == 7, "Need exactly 7 arguments.");                 \
  oop first_arg = argv[0];                                       \
  oop second_arg = argv[1];                                      \
  oop third_arg = argv[2];                                       \
  oop fourth_arg = argv[3];                                      \
  oop fifth_arg = argv[4];                                       \
  oop sixth_arg = argv[5];                                       \
  oop seventh_arg = argv[6];                                     \

#define UNARY_PREDICATE(name, c_tester) \
oop name(oop* argv, size_t argc) {      \
  PARSE_ONE_ARG(value);                 \
  return lisp_bool(c_tester(value));    \
}

void init_primitives();
oop lisp_bool(bool b);

#endif  // _PRIMITIVES_H_
