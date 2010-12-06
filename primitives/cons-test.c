
#include "tests.h"
#include "value.h"
#include "cons.h"

TEST(cons_simple_test) {
  value_t a, b;
  a.uint = 3L;
  b.uint = 4L;
  value_t cons = make_cons(a, b);
  ASSERT_EQ(a, first(cons));
  ASSERT_EQ(b, rest(cons));
}

extern
void cons_tests() {
  TESTRUN(cons_simple_test);
}
