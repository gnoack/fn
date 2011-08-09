
#include "tests.h"

#include "arrays.h"
#include "env-test.h"
#include "eval.h"
#include "lang.h"
#include "macros.h"
#include "objects.h"
#include "parser.h"
#include "pegs-parser.h"
#include "pegs.h"
#include "pprint.h"
#include "primitives.h"
#include "strings.h"
#include "symbols.h"
#include "utils.h"
#include "value.h"
#include "x86.h"
#include "compiler.h"

#include "arrays-test.h"
#include "compiler-test.h"
#include "cons-test.h"
#include "env-test.h"
#include "macros-test.h"
#include "memory-test.h"
#include "objects-test.h"
#include "parser-test.h"
#include "pegs-parser-test.h"
#include "pegs-test.h"
#include "pprint-test.h"
#include "primitives-test.h"
#include "string-interning-test.h"
#include "strings-test.h"
#include "utils-test.h"
#include "x86-test.h"

#include "carcdr.h"

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
void assert_false(const char* filename,
		  unsigned int line,
		  bool b) {
  init_assert();
  if (b) {
    fail(filename, line, "Expected false, got true.");
  }
}

extern
void assert_nil(const char* filename,
		unsigned int line,
		oop value) {
  init_assert();
  if (!is_nil(value)) {
    fail(filename, line, "Expected nil.");
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
    // TODO: Use nice object representation instead!
    printf("  Expected: ");
    print_value(a);
    printf("       Got: ");
    print_value(b);
  }
}

extern
void run_lisp_tests(oop tests) {
  while (!is_nil(tests)) {
    oop test = car(tests);
    oop result = eval_global(test);
    init_assert();
    if (!value_eq(symbols._true, result)) {
      fail("from lisp", 0, "Expected true for expression:");
      print_value(test);
      printf("  --> ");
      print_value(result);
    }

    tests = cdr(tests);
  }
}

void init() {
  init_symbols();
  init_eval();
  init_primitives();
  load_decls(lang_decls());
  load_decls(macros_decls());
  load_decls(utils_decls());
  load_decls(objects_decls());
  load_decls(arrays_decls());
  load_decls(pegs_decls());
  load_decls(parser_decls());
  load_decls(pegs_parser_decls());
  load_decls(pprint_decls());
  load_decls(x86_decls());
  load_decls(compiler_decls());
}

int main(int argc, char* argv[]) {
  init();
  if (argc > 1) {
    printf("%s:\n", argv[1]);
    oop cmd = make_string(argv[1]);
    oop sexpr = eval_global(LIST(make_symbol("read-all"),
				 LIST(make_symbol("quote"), cmd)));
    print_value(sexpr);
    printf("==> ");
    print_value(eval_global(sexpr));
    exit(0);
  }
  printf("Test execution:\n");
  /* Register tests here. */
  cons_tests();
  interning_tests();
  eval_tests();
  value_tests();
  strings_tests();
  memory_tests();
  primitives_tests();
  utils_tests();
  lang_tests();
  env_tests();
  macros_tests();
  pegs_tests();
  parser_tests();
  pegs_parser_tests();
  pprint_tests();
  objects_tests();
  arrays_tests();
  x86_tests();
  compiler_tests();
  /* Summing up. */
  printf("\n%d assertions executed, %d failures.\n",
	 assertion_count, failure_count);
  if (failure_count == 0) {
    printf("Well done!\n");
  }
  return 0;
}
