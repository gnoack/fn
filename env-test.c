
#include "cons.h"
#include "value.h"
#include "tests.h"

#include "env.h"

#include "env-test.h"

TEST(env_simple) {
  oop env = make_env(make_smallint(1), make_char('a'), NIL);
  ASSERT_TRUE(TO_BOOL(1 == length_int(env)));
  env = make_env(make_smallint(2), make_char('b'), env);
  ASSERT_TRUE(env_haskey(env, make_smallint(1)));
  ASSERT_TRUE(env_haskey(env, make_smallint(2)));
  ASSERT_FALSE(env_haskey(env, make_smallint(3)));
}

TEST(env_modify) {
  oop env = make_env(make_smallint(3), make_char('c'), NIL);
  ASSERT_TRUE(TO_BOOL(1 == length_int(env)));
  env_put(env, make_smallint(4), make_char('d'));
  ASSERT_TRUE(TO_BOOL(2 == length_int(env)));
  ASSERT_EQ(make_char('c'), env_lookup(env, make_smallint(3)));
  env_put(env, make_smallint(3), make_char('x'));
  ASSERT_EQ(make_char('x'), env_lookup(env, make_smallint(3)));
}

extern
void env_tests() {
  TESTRUN(env_simple);
  TESTRUN(env_modify);
}
