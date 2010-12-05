
#include "value.h"
#include "intreg.h"
#include "tests.h"

TEST(simple_add) {
  value_t a, b, c;
  make_intreg(4L, &a);
  make_intreg(6L, &b);
  intreg_add(a, b, &c);
  value_t expected;
  make_intreg(10L, &expected);
  ASSERT_EQ(expected, c);
}

void intreg_tests() {
  TESTRUN(simple_add);
}
