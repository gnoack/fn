
#include <malloc.h>
#include "tests.h"
#include "string-interning.h"

// template must be a string of length 3 or more.
const char* make(const char* template) {
  char* foo = malloc(4);
  foo[0] = template[0];
  foo[1] = template[1];
  foo[2] = template[2];
  foo[3] = '\0';
  return foo;
}

TEST(simple_interning) {
  // Returns a string with the same content.
  const char* foo = make("foo");
  const char* result = intern_string(foo);
  ASSERT_TRUE(strcmp("foo", result) == 0);
}

TEST(interns_to_different_values) {
  const char* bar = make("bar");
  const char* baz = make("baz");
  ASSERT_TRUE(intern_string(bar) != intern_string(baz));
}

TEST(interning_different_values) {
  const char* foo = "foo";
  const char* result = intern_string(foo);
  ASSERT_TRUE(foo != result);
}

TEST(interning_idempotent) {
  const char* foo1 = make("foo");
  const char* foo2 = intern_string(foo1);
  const char* foo3 = intern_string(foo2);
  ASSERT_TRUE(foo1 != foo2);
  ASSERT_TRUE(foo2 == foo3);
}

TEST(interning_interns) {
  const char* foo1 = make("foo");
  const char* foo2 = make("foo");
  ASSERT_TRUE(foo1 != foo2);
  ASSERT_TRUE(intern_string(foo1) == intern_string(foo2));
}

void interning_tests() {
  TESTRUN(simple_interning);
  TESTRUN(interns_to_different_values);
  TESTRUN(interning_different_values);
  TESTRUN(interning_idempotent);
  TESTRUN(interning_interns);
}
