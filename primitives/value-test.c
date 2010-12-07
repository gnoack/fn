
#include "tests.h"
#include "value.h"

TEST(strings) {
  value_t s = make_string("foo");
  value_t s2 = make_string("foo");
  ASSERT_EQ(s, s2);
}

extern
void value_tests() {
  TESTRUN(strings);
}
