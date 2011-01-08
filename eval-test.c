
#include "cons.h"
#include "tests.h"
#include "eval.h"

#define I make_smallint
#define S make_symbol

TEST(eval_if_true) {
  // (if true 5 50) --> 5
  ASSERT_EQ(I(5), eval_global(LIST(S("if"), S("true"), I(5), I(50))));
}

TEST(eval_if_false) {
  // (if false 5 50) --> 50
  ASSERT_EQ(I(50), eval_global(LIST(S("if"), S("false"), I(5), I(50))));
}

TEST(eval_if_cascaded) {
  //     (if (if false false true) 5 50)
  // --> (if true 5 50)
  // --> 5
  ASSERT_EQ(I(5),
	    eval_global(LIST(S("if"),
			     LIST(S("if"), S("false"), S("false"), S("true")),
			     I(5),
			     I(50))));
}

TEST(eval_globally_bound_function) {
  // (+ 3 7) --> 10
  ASSERT_EQ(I(10), eval_global(LIST(S("+"), I(3), I(7))));
}

TEST(eval_lambda_simple) {
  // ((lambda (x) (+ x 1)) 5) --> 6
  ASSERT_EQ(I(6),
	    eval_global(LIST(LIST(S("lambda"), LIST(S("x")),
				  LIST(S("+"), S("x"), I(1))),
			     I(5))));
}

TEST(eval_let_simple) {
  ASSERT_EQ(I(3),
	    eval_global(LIST(S("let"), LIST(LIST(S("x"), I(3))),
			     S("x"))));
}

TEST(eval_symbol_in_env) {
  oop env = LIST(make_cons(S("x"), I(2)));
  ASSERT_EQ(I(2), eval(S("x"), env));
}

// TODO(gnoack): Tests for: Cond.

extern
void eval_tests() {
  TESTRUN(eval_lambda_simple);
  TESTRUN(eval_symbol_in_env);
  TESTRUN(eval_if_true);
  TESTRUN(eval_if_false);
  TESTRUN(eval_if_cascaded);
  TESTRUN(eval_let_simple);
}
