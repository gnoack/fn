
#include "tests.h"
#include "value.h"
#include "string-interning-test.h"
#include "cons-test.h"
#include "eval.h"
#include "symbols.h"

#include <stdio.h>

unsigned int assertion_count = 0;
unsigned int failure_count = 0;

void init_assert() {
  printf(".");
  assertion_count++;
}

extern
void fail(const char* filename,
	  unsigned int line,
	  const char* msg) {
  printf("\n%s:%d: FAIL -- %s\n", filename, line, msg);
  failure_count++;
}

extern
void assert_true(const char* filename,
		 unsigned int line,
		 bool b) {
  init_assert();
  if (!b) {
    fail(filename, line, "Expected true, got false.");
  }
}

extern
void assert_nil(const char* filename,
		unsigned int line,
		oop value) {
  init_assert();
  if (!is_nil(value)) {
    fail(filename, line, "Expected true, got false.");
  }
}

extern
void assert_eq(const char* filename,
	       unsigned int line,
	       oop a,
	       oop b) {
  init_assert();
  if (value_eq(a, b) == NO) {
    fail(filename, line, "Values not equal.");
    printf("expected %d, got %d\n", a.smallint >> 1, b.smallint >> 1);
  }
}

void init() {
  init_symbols();
  init_eval();
}

int main(int argc, char* argv) {
  init();
  printf("Test execution:\n");
  /* Register tests here. */
  cons_tests();
  interning_tests();
  eval_tests();
  value_tests();
  /* Summing up. */
  printf("\n%d assertions executed, %d failures.\n",
	 assertion_count, failure_count);
  if (failure_count == 0) {
    printf("Well done!\n");
  }
  return 0;
}
