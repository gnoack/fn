
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

TEST(interpret_load_value) {
  init_interpreter();
  oop code[] = {
    symbols._bc_load_value, I(8),
    symbols._bc_halt
  };
  state.ip = code;
  interpret();
  ASSERT_EQ(I(8), state.reg_acc);
}

TEST(interpret_load_var) {
  init_interpreter();
  oop outer_frame = make_frame(3, NULL, NIL, NIL);
  set_var(outer_frame, 2, I(99));
  oop inner_frame = make_frame(4, NULL, NIL, outer_frame);
  set_var(inner_frame, 3, I(42));
  state.reg_frm = inner_frame;

  oop code_a[] = {
    symbols._bc_load_var, I(0), I(3),
    symbols._bc_halt
  };
  state.ip = code_a;
  interpret();
  ASSERT_EQ(I(42), state.reg_acc);

  oop code_b[] = {
    symbols._bc_load_var, I(1), I(2),
    symbols._bc_halt
  };
  state.ip = code_b;
  interpret();
  ASSERT_EQ(I(99), state.reg_acc);
}

TEST(interpret_write_var) {
  init_interpreter();
  oop frame = make_frame(3, NULL, NIL, NIL);
  state.reg_frm = frame;

  oop code[] = {
    symbols._bc_load_value, I(8),
    symbols._bc_write_var, I(0), I(1),
    symbols._bc_halt
  };
  state.ip = code;
  ASSERT_EQ(NIL, get_var(frame, 1));
  interpret();
  ASSERT_EQ(I(8), get_var(frame, 1));
}

TEST(interpret_make_lambda) {
  init_interpreter();
  oop frame = make_frame(3, NULL, NIL, NIL);
  state.reg_frm = frame;

  oop code[] = {
    symbols._bc_make_lambda, I(12345),
    symbols._bc_halt
  };
  state.ip = code;
  interpret();
  oop result = state.reg_acc;
  ASSERT_EQ(frame, mem_get(result, 1));
  ASSERT_EQ(I(12345), mem_get(result, 2));
}

extern void interpreter_tests() {
  TESTRUN(stack_simple);
  TESTRUN(interpret_load_value);
  TESTRUN(interpret_load_var);
  TESTRUN(interpret_write_var);
  TESTRUN(interpret_make_lambda);
}
