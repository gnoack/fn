
#include "value.h"
#include "memory.h"
#include "symbols.h"

#include "tests.h"

#include "interpreter.h"

#define I make_smallint
#define S make_symbol

TEST(stack_simple) {
  stack_push(I(3));
  stack_push(I(5));
  ASSERT_EQ(I(5), stack_pop());
  ASSERT_EQ(I(3), stack_pop());
}

extern void interpreter_tests() {
  TESTRUN(stack_simple);
}
