
#include "tests.h"
#include "value.h"
#include "cons.h"

TEST(strings) {
  oop s = make_symbol("foo");
  oop s2 = make_symbol("foo");
  ASSERT_EQ(s, s2);
}

TEST(identifying_strings) {
  ASSERT_TRUE(is_symbol(make_symbol("eggs")));
}

TEST(identifying_conses) {
  ASSERT_TRUE(is_cons(make_cons(make_symbol("a"),
				make_symbol("b"))));
}

TEST(identifying_smallint) {
  ASSERT_TRUE(is_smallint(make_smallint(200)));
}

TEST(identifying_char) {
  oop a = make_char('a');
  ASSERT_TRUE(is_char(a));
  ASSERT_FALSE(is_smallint(a));
  ASSERT_FALSE(is_cons(a));
  ASSERT_FALSE(is_symbol(a));
}

TEST(value_size) {
  // TODO(gnoack): Ugh, this is implementation-specific.
  ASSERT_TRUE(4 == sizeof(oop));
  ASSERT_TRUE(4 == sizeof(uint));
  ASSERT_TRUE(4 == sizeof(char*));
}

TEST(int_equality) {
  oop a = make_smallint(10);
  oop b = make_smallint(10);
  oop c = make_smallint(11);
  ASSERT_TRUE(value_eq(a, b));
  ASSERT_FALSE(value_eq(a, c));
  ASSERT_FALSE(value_eq(NIL, a));
}

TEST(char_equality) {
  oop a = make_char('a');
  oop a2 = make_char('a');
  oop b = make_char('b');
  ASSERT_TRUE(value_eq(a, a2));
  ASSERT_FALSE(value_eq(b, a));
}

TEST(nil_equality) {
  oop a = make_smallint(10);
  ASSERT_FALSE(value_eq(NIL, a));
  ASSERT_EQ(NIL, NIL);
}

TEST(char_conversion) {
  oop c = make_char('b');
  ASSERT_TRUE(TO_BOOL(get_char(c) == 'b'));
}

extern
void value_tests() {
  TESTRUN(strings);
  TESTRUN(identifying_strings);
  TESTRUN(identifying_smallint);
  TESTRUN(identifying_conses);
  TESTRUN(identifying_char);
  TESTRUN(value_size);
  TESTRUN(int_equality);
  TESTRUN(nil_equality);
  TESTRUN(char_equality);
  TESTRUN(char_conversion);
}
