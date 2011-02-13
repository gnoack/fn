
#include "tests.h"

#include "parser.h"
#include "strings.h"

TEST(parse_symbol) {
  oop str = make_string("foo");
  oop result = parse_sexpr(str);
  ASSERT_EQ(make_symbol("foo"), result);
}

extern
void parser_tests() {
  TESTRUN(parse_symbol);
}
