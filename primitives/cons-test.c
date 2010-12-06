
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

TEST(cons_list_test) {
  value_t l = make_list(make_uint(1),
			make_uint(2),
			make_uint(3),
			NIL);
  ASSERT_EQ(make_uint(1), first(l));
  ASSERT_EQ(make_uint(2), first(rest(l)));
  ASSERT_EQ(make_uint(3), first(rest(rest(l))));
  ASSERT_NIL(rest(rest(rest(l))));
}

extern
void cons_tests() {
  TESTRUN(cons_simple_test);
  TESTRUN(cons_list_test);
}
