
#include "tests.h"

#include "parser.h"
#include "strings.h"

TEST(parse_symbol) {
  oop str = make_string("foo");
  oop result = parse_sexpr(str);
  ASSERT_EQ(make_symbol("foo"), result);
}

TEST(parse_number) {
  oop str = make_string("123");
  ASSERT_EQ(make_smallint(123), parse_sexpr(str));
}

extern
void parser_tests() {
  TESTRUN(parse_symbol);
  TESTRUN(parse_number);
}
