#include "value.h"
#include "memory.h"
#include "symbols.h"

#include "tests.h"

#include "interpreter.h"

#define I make_smallint
#define S make_symbol

TEST(stack_simple) {
  ASSERT_TRUE(stack_size() == 0);
  stack_push(I(3));
  stack_push(I(5));
  ASSERT_EQ(I(5), stack_pop());
  ASSERT_EQ(I(3), stack_pop());
  ASSERT_TRUE(stack_size() == 0);
}

TEST(stack_size_and_shrink) {
  ASSERT_TRUE(stack_size() == 0);
  stack_push(I(1));
  ASSERT_TRUE(stack_size() == 1);
  stack_push(I(2));
  ASSERT_TRUE(stack_size() == 2);
  stack_shrink(2);
  ASSERT_TRUE(stack_size() == 0);
}

TEST(stack_peek_at_top) {
  ASSERT_TRUE(stack_size() == 0);
  stack_push(I(42));
  stack_push(I(43));
  stack_push(I(44));
  ASSERT_EQ(I(44), stack_peek());
  stack_shrink(3);
  ASSERT_TRUE(stack_size() == 0);
}

TEST(stack_peek_at_depth) {
  ASSERT_TRUE(stack_size() == 0);
  stack_push(I(42));
  stack_push(I(43));
  stack_push(I(44));
  ASSERT_EQ(I(44), stack_peek_at(1));
  ASSERT_EQ(I(42), stack_peek_at(3));
  stack_shrink(3);
  ASSERT_TRUE(stack_size() == 0);
}

extern void interpreter_tests() {
  TESTRUN(stack_simple);
  TESTRUN(stack_size_and_shrink);
  TESTRUN(stack_peek_at_top);
  TESTRUN(stack_peek_at_depth);
}
