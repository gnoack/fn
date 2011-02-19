
#include "tests.h"

#include "eval.h"
#include "env.h"
#include "primitives.h"

#define S make_symbol

void assert_exists(const char* string) {
  ASSERT_TRUE(env_haskey(global_env, make_symbol(string)));
}

TEST(primitives_existence) {
  // Cons.
  assert_exists("first");
  assert_exists("rest");
  assert_exists("cons");
  // TODO: For later.
  /* // Character conversion. */
  /* assert_exists("char->num"); */
  /* assert_exists("num->char"); */
  /* // Arithmetic. */
  /* assert_exists("+"); */
  /* assert_exists("-"); */
  /* assert_exists("*"); */
}

extern
void primitives_tests() {
  TESTRUN(primitives_existence);
}
