#include "value.h"
#include "memory.h"
#include "symbols.h"

#include "tests.h"

#include "interpreter.h"

#define I make_smallint
#define S make_symbol

TEST(stack_simple) {
  stack_t s;
  stack_init(&s);
  ASSERT_TRUE(stack_size(&s) == 0);
  stack_push(&s, I(3));
  stack_push(&s, I(5));
  ASSERT_EQ(I(5), stack_pop(&s));
  ASSERT_EQ(I(3), stack_pop(&s));
  ASSERT_TRUE(stack_size(&s) == 0);
}

TEST(stack_size_and_shrink) {
  stack_t s;
  stack_init(&s);
  ASSERT_TRUE(stack_size(&s) == 0);
  stack_push(&s, I(1));
  ASSERT_TRUE(stack_size(&s) == 1);
  stack_push(&s, I(2));
  ASSERT_TRUE(stack_size(&s) == 2);
  stack_shrink(&s, 2);
  ASSERT_TRUE(stack_size(&s) == 0);
}

TEST(stack_peek_at_top) {
  stack_t s;
  stack_init(&s);
  ASSERT_TRUE(stack_size(&s) == 0);
  stack_push(&s, I(42));
  stack_push(&s, I(43));
  stack_push(&s, I(44));
  ASSERT_EQ(I(44), stack_peek(&s));
  stack_shrink(&s, 3);
  ASSERT_TRUE(stack_size(&s) == 0);
}

TEST(stack_peek_at_depth) {
  stack_t s;
  stack_init(&s);
  ASSERT_TRUE(stack_size(&s) == 0);
  stack_push(&s, I(42));
  stack_push(&s, I(43));
  stack_push(&s, I(44));
  ASSERT_EQ(I(44), stack_peek_at(&s, 1));
  ASSERT_EQ(I(42), stack_peek_at(&s, 3));
  stack_shrink(&s, 3);
  ASSERT_TRUE(stack_size(&s) == 0);
}

extern void interpreter_tests() {
  TESTRUN(stack_simple);
  TESTRUN(stack_size_and_shrink);
  TESTRUN(stack_peek_at_top);
  TESTRUN(stack_peek_at_depth);
}
