
#include "value.h"

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

TEST(interpret_load_value) {
  init_interpreter();
  char* code = {
    BC_LOAD_VALUE, 0x00, 0x00, 0x00, 0x11,  // I(8)
    BC_HALT
  };
  interpreter_state.ip = code;
  interpret();
  ASSERT_EQ(I(8), interpreter_state.reg_acc);
}

extern void interpreter_tests() {
  TESTRUN(stack_simple);
}
