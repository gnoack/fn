
#include "tests.h"
#include "value.h"
#include "cons.h"

TEST(symbol_identity) {
  oop s = symbol_to_oop(make_symbol("foo"));
  oop s2 = symbol_to_oop(make_symbol("foo"));
  ASSERT_EQ(s, s2);
}

TEST(identifying_symbols) {
  oop a = symbol_to_oop(make_symbol("eggs"));
  ASSERT_FALSE(is_char(a));
  ASSERT_FALSE(is_smallint(a));
  ASSERT_FALSE(is_cons(a));
  ASSERT_TRUE(is_symbol(a));
}

TEST(identifying_conses) {
  oop a = make_cons(symbol_to_oop(make_symbol("a")),
		    symbol_to_oop(make_symbol("b")));
  ASSERT_FALSE(is_char(a));
  ASSERT_FALSE(is_smallint(a));
  ASSERT_TRUE(is_cons(a));
  ASSERT_FALSE(is_symbol(a));
}

TEST(identifying_smallint) {
  oop a = make_smallint(200);
  ASSERT_FALSE(is_char(a));
  ASSERT_TRUE(is_smallint(a));
  ASSERT_FALSE(is_cons(a));
  ASSERT_FALSE(is_symbol(a));
}

TEST(identifying_char) {
  oop a = make_char('a');
  ASSERT_TRUE(is_char(a));
  ASSERT_FALSE(is_smallint(a));
  ASSERT_FALSE(is_cons(a));
  ASSERT_FALSE(is_symbol(a));
}

TEST(value_size) {
  ASSERT_TRUE(sizeof(oop) == sizeof(fn_uint));
  ASSERT_TRUE(sizeof(oop) == sizeof(char*));
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
  TESTRUN(symbol_identity);
  TESTRUN(identifying_symbols);
  TESTRUN(identifying_smallint);
  TESTRUN(identifying_conses);
  TESTRUN(identifying_char);
  TESTRUN(value_size);
  TESTRUN(int_equality);
  TESTRUN(nil_equality);
  TESTRUN(char_equality);
  TESTRUN(char_conversion);
}
