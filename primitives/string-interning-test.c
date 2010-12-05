
#include <malloc.h>
#include "tests.h"
#include "string-interning.h"

TEST(simple_interning) {
  char* foo = malloc(4);
  foo[0] = 'f';
  foo[1] = 'o';
  foo[2] = 'o';
  foo[3] = '\0';
  const char* result = intern_string(foo);
  ASSERT_TRUE(strcmp("foo", result) == 0);
}

void interning_tests() {
  TESTRUN(simple_interning);
}
