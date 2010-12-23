
#include "tests.h"
#include "value.h"
#include "cons.h"

TEST(cons_simple_test) {
  oop a = make_smallint(3);
  oop b = make_smallint(4);
  oop cons = make_cons(a, b);
  ASSERT_EQ(a, first(cons));
  ASSERT_EQ(b, rest(cons));
}

oop I(uint i) { return make_smallint(i); }

TEST(cons_list_test) {
  oop l = make_list(I(1),
		    I(2),
		    I(3),
		    end_marker());
  ASSERT_EQ(I(1), first(l));
  ASSERT_EQ(I(2), first(rest(l)));
  ASSERT_EQ(I(3), first(rest(rest(l))));
  ASSERT_NIL(rest(rest(rest(l))));
}

TEST(cons_list_macro_test) {
  oop l = LIST(I(1), I(2));
  ASSERT_EQ(I(1), first(l));
  ASSERT_EQ(I(2), first(rest(l)));
  ASSERT_NIL(rest(rest(l)));
}

extern
void cons_tests() {
  TESTRUN(cons_simple_test);
  TESTRUN(cons_list_test);
  TESTRUN(cons_list_macro_test);
}
