
#include "value.h"
#include "memory.h"

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
  unsigned char code[] = {
    BC_LOAD_VALUE, 0x11, 0x00, 0x00, 0x00,  // I(8)
    BC_HALT
  };
  interpreter_state.ip = code;
  interpret();
  ASSERT_EQ(I(8), interpreter_state.reg_acc);
}

TEST(interpret_load_var) {
  init_interpreter();
  oop outer_frame = make_frame(3, NULL, NIL, NIL);
  set_var(outer_frame, 2, I(99));
  oop inner_frame = make_frame(4, NULL, NIL, outer_frame);
  set_var(inner_frame, 3, I(42));
  interpreter_state.reg_frm = inner_frame;

  unsigned char code_a[] = {
    BC_LOAD_VAR, 0x00, 0x03,
    BC_HALT
  };
  interpreter_state.ip = code_a;
  interpret();
  ASSERT_EQ(I(42), interpreter_state.reg_acc);

  unsigned char code_b[] = {
    BC_LOAD_VAR, 0x01, 0x02,
    BC_HALT
  };
  interpreter_state.ip = code_b;
  interpret();
  ASSERT_EQ(I(99), interpreter_state.reg_acc);
}

TEST(interpret_write_var) {
  init_interpreter();
  oop frame = make_frame(3, NULL, NIL, NIL);
  interpreter_state.reg_frm = frame;

  unsigned char code_a[] = {
    BC_LOAD_VALUE, 0x11, 0x00, 0x00, 0x00,
    BC_WRITE_VAR, 0x00, 0x01,
    BC_HALT
  };
  interpreter_state.ip = code_a;
  ASSERT_EQ(NIL, get_var(frame, 1));
  interpret();
  ASSERT_EQ(I(8), get_var(frame, 1));
}

TEST(interpret_make_lambda) {
  init_interpreter();
  oop frame = make_frame(3, NULL, NIL, NIL);
  interpreter_state.reg_frm = frame;

  unsigned char code[] = {
    BC_MAKE_LAMBDA, 0x00, 0x11, 0x22, 0x33,
    BC_HALT
  };
  interpreter_state.ip = code;
  interpret();
  oop result = interpreter_state.reg_acc;
  ASSERT_EQ(frame, mem_get(result, 1));
  ASSERT_TRUE(0x33221100 == mem_get(result, 2).smallint);
}

extern void interpreter_tests() {
  TESTRUN(stack_simple);
  TESTRUN(interpret_load_value);
  TESTRUN(interpret_load_var);
  TESTRUN(interpret_write_var);
  TESTRUN(interpret_make_lambda);
}
