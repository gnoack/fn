
#include "cons.h"
#include "tests.h"
#include "eval.h"

#define I make_uint
#define S make_string

TEST(eval_if_true) {
  ASSERT_EQ(I(5), eval(LIST(S("if"), I(1), I(5), I(50))));
}

TEST(eval_if_false) {
  ASSERT_EQ(I(50), eval(LIST(S("if"), NIL, I(5), I(50))));
}

TEST(eval_if_cascaded) {
  ASSERT_EQ(I(5), eval(LIST(S("if"),
			    LIST(S("if"), NIL, NIL, I(4)),
			    I(5),
			    I(50))));
}

// TODO(gnoack): Tests for: Lambda, Cond.
// TODO(gnoack): Differ between true, false, nil and 0.

extern
void eval_tests() {
  TESTRUN(eval_if_true);
  TESTRUN(eval_if_false);
  TESTRUN(eval_if_cascaded);
}
