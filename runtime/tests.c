#include "runtime/debug.h"
#include "runtime/gc.h"
#include "runtime/runtime.h"

#include "runtime/cons-test.h"
#include "runtime/interpreter-test.h"
#include "runtime/memory-test.h"
#include "runtime/strings-test.h"
#include "runtime/value-test.h"

#include "tests.h"

#include <string.h>

unsigned int assertion_count = 0;
unsigned int failure_count = 0;

void init_assert() {
  putchar('.');
  fflush(stdout);
  assertion_count++;
}

extern
void fail(const char* filename,
          unsigned int line,
          const char* msg) {
  printf("\n%s:%u: FAIL -- %s\n", filename, line, msg);
  failure_count++;
}

extern
void assert_true(const char* filename,
                 unsigned int line,
                 boolean b) {
  init_assert();
  if (!b) {
    fail(filename, line, "Expected true, got false.");
  }
}

extern
void assert_false(const char* filename,
                  unsigned int line,
                  boolean b) {
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
    printf("  Expected: ");
    println_value(a);
    printf("       Got: ");
    println_value(b);
  }
}

void run_all_c_tests() {
  interpreter_tests();
  cons_tests();
  value_tests();
  strings_tests();
  memory_tests();
  /* Summing up. */
  printf("\n%u assertions executed, %u failures.\n",
         assertion_count, failure_count);
  if (failure_count == 0) {
    printf("Well done!\n");
  }
}

boolean deserialize_arg = NO;
void parse_args(int argc, char* argv[]) {
  int i;
  for (i=1; i<argc; i++) {
    if (strcmp(argv[i], "-S") == 0) {
      deserialize_arg = YES; continue;
    }
  }
}

int main(int argc, char* argv[]) {
  parse_args(argc, argv);
  
  fn_runtime_init();
  
  if (deserialize_arg) {
    gc_deserialize_from_file("fn.img");
  } else {
    fn_runtime_init_lisp_decls();
  }

  run_all_c_tests();
  return 0;
}

