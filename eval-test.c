
#include "cons.h"
#include "tests.h"
#include "eval.h"

#define I make_smallint
#define S make_symbol

// Most of these tests moved to lang-test.fn

TEST(eval_def_simple) {
  eval_global(LIST(S("def"), S("__gaaaa__"),
		   LIST(S("+"), I(2), I(3))));
  // Variable is set now in the global environment.
  ASSERT_EQ(I(5), eval_global(S("__gaaaa__")));
}

TEST(eval_symbol_in_env) {
  oop env = LIST(make_cons(S("x"), I(2)));
  ASSERT_EQ(I(2), eval(S("x"), env));
}

extern
void eval_tests() {
  TESTRUN(eval_def_simple);
  TESTRUN(eval_symbol_in_env);
}
