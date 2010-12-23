
#include "tests.h"
#include "value.h"
#include "cons.h"

TEST(strings) {
  oop s = make_string("foo");
  oop s2 = make_string("foo");
  ASSERT_EQ(s, s2);
}

TEST(identifying_strings) {
  ASSERT_TRUE(is_string(make_string("eggs")));
}

TEST(identifying_conses) {
  ASSERT_TRUE(is_cons(make_cons(make_string("a"), make_string("b"))));
}

TEST(identifying_smallint) {
  ASSERT_TRUE(is_smallint(make_smallint(200)));
}

TEST(value_size) {
  // TODO(gnoack): Ugh, this is implementation-specific.
  ASSERT_TRUE(4 == sizeof(oop));
  ASSERT_TRUE(4 == sizeof(uint));
  ASSERT_TRUE(4 == sizeof(char*));
}

extern
void value_tests() {
  TESTRUN(strings);
  TESTRUN(identifying_strings);
  TESTRUN(identifying_smallint);
  TESTRUN(identifying_conses);
  TESTRUN(value_size);
}
