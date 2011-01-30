
#include "tests.h"

#include "strings.h"
#include "value.h"
#include "carcdr.h"

TEST(string_construction) {
  oop str = make_string("foo");
  ASSERT_EQ(make_char('f'), car(str));
  ASSERT_EQ(make_char('o'), cadr(str));
  ASSERT_EQ(make_char('o'), caddr(str));
  ASSERT_EQ(NIL, cdddr(str));
}

extern
void strings_tests() {
  TESTRUN(string_construction);
}
