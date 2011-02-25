
#include "tests.h"

#include "eval.h"
#include "cons.h"
#include "env.h"
#include "primitives.h"
#include "carcdr.h"

#define I make_smallint
#define S make_symbol

void assert_exists(const char* string) {
  ASSERT_TRUE(env_haskey(global_env, make_symbol(string)));
}

TEST(primitives_existence) {
  // Cons.
  assert_exists("first");
  assert_exists("rest");
  assert_exists("cons");
  // Character conversion.
  assert_exists("char->num");
  assert_exists("num->char");
  // Arithmetic.
  assert_exists("+");
  assert_exists("-");
  assert_exists("*");
  // Equality.
  assert_exists("eq");
  // Testers.
  assert_exists("cons?");
  assert_exists("char?");
  assert_exists("number?");
  // List.
  assert_exists("list");
}

TEST(primitives_list) {
  oop result = eval_global(LIST(S("list"), I(1),
				LIST(S("+"), I(2), I(3))));
  ASSERT_TRUE(2 == length_int(result));
  ASSERT_EQ(I(1), car(result));
  ASSERT_EQ(I(5), cadr(result));
}

extern
void primitives_tests() {
  TESTRUN(primitives_existence);
  TESTRUN(primitives_list);
}
