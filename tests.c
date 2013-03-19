
#include "tests.h"

#include "arrays.h"
#include "byte-buffer.h"
#include "compiler.h"
#include "continuations.h"
#include "data.h"
#include "debug.h"
#include "dispatcher.h"
#include "eval.h"
#include "gc.h"
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

#include "arrays-test.h"
#include "byte-buffer-test.h"
#include "compiler-test.h"
#include "cons-test.h"
#include "continuations-test.h"
#include "data-test.h"
#include "dispatcher-test.h"
#include "eval-test.h"
#include "interpreter-test.h"
#include "lang-test.h"
#include "macros-test.h"
#include "memory-test.h"
#include "objects-test.h"
#include "parser-test.h"
#include "pegs-parser-test.h"
#include "pegs-test.h"
#include "pprint-test.h"
#include "primitives-test.h"
#include "strings-test.h"
#include "utils-test.h"
#include "value-test.h"

#include "carcdr.h"

#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

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
  printf("\n%s:%d: FAIL -- %s\n", filename, line, msg);
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

extern
void run_lisp_tests(oop tests) {
  gc_protect_counter++;
  while (!is_nil(tests)) {
    oop test = car(tests);
    oop result = eval_global(test);
    init_assert();
    if (!value_eq(symbols._true, result)) {
      fail("from lisp", 0, "Expected true for expression:");
      println_value(test);
      printf("  --> ");
      println_value(result);
    }

    tests = cdr(tests);
  }
  gc_protect_counter--;
  gc_run();
}

void init() {
  init_procedures();
  init_gc();
  init_symbols();
  init_eval();
  init_primitives();
  init_interpreter();
  init_data();
}

void init_decls() {
  load_decls(lang_decls());
  load_decls(macros_decls());
  load_decls(utils_decls());
  load_decls(dispatcher_decls());
  load_decls(objects_decls());
  load_decls(arrays_decls());
  load_decls(pegs_decls());
  load_decls(parser_decls());
  load_decls(pegs_parser_decls());
  load_decls(pprint_decls());
  load_decls(byte_buffer_decls());
  load_decls(compiler_decls());
  load_decls(continuations_decls());
}

#define HISTORY_FILE "/.fn_history"

char* get_histfile() {
  char* homedir = getenv("HOME");
  char* result = malloc(strlen(homedir) + strlen(HISTORY_FILE) + 1);
  strcpy(result, homedir);
  strcat(result, HISTORY_FILE);
  return result;
}

char* symbol_completion_entry(const char* text, int state) {
  oop result =
    apply(LIST(lookup_globally(make_symbol("readline-completion-entry")),
               make_string(text), make_smallint(state)));
  if (is_nil(result)) {
    return NULL;
  } else {
    return c_string(result);
  }
}

void repl() {
  char* histfile = get_histfile();
  read_history(histfile);
  rl_completion_entry_function = symbol_completion_entry;
  char* input;
  while (1) {
    input = readline("fn> ");

    if (input == NULL) {
      puts("\nGoodbye.");
      write_history(histfile);
      return;
    }
    if (*input) {
      add_history(input);
    }
    oop cmd = make_string(input);
    oop sexpr = eval_global(LIST(make_symbol("read-one"), cmd));
    println_value(eval_global(sexpr));

    free(input);
  }
}

void run_all_tests() {
  printf("Test execution:\n");
  /* Register tests here. */
  interpreter_tests();
  cons_tests();
  eval_tests();
  value_tests();
  strings_tests();
  memory_tests();
  primitives_tests();
  lang_tests();
  utils_tests();
  macros_tests();
  pegs_tests();
  parser_tests();
  pegs_parser_tests();
  pprint_tests();
  dispatcher_tests();
  objects_tests();
  arrays_tests();
  byte_buffer_tests();
  compiler_tests();
  data_tests();
  continuations_tests();
  /* Summing up. */
  printf("\n%d assertions executed, %d failures.\n",
	 assertion_count, failure_count);
  if (failure_count == 0) {
    printf("Well done!\n");
  }
}

void load_file(const char* filename) {
  apply(LIST(lookup_globally(make_symbol("load-file")),
	     make_string(filename)));
}

boolean deserialize_arg = NO;
boolean exit_arg = NO;
boolean load_twice_arg = NO;
boolean serialize_arg = NO;
boolean test_arg = NO;
const char* file_to_load = NULL;
void parse_args(int argc, char* argv[]) {
  int i;
  for (i=1; i<argc; i++) {
    if (strcmp(argv[i], "-2") == 0) {
      load_twice_arg = YES; continue;
    }
    if (strcmp(argv[i], "-s") == 0) {
      serialize_arg = YES; continue;
    }
    if (strcmp(argv[i], "-S") == 0) {
      deserialize_arg = YES; continue;
    }
    if (strcmp(argv[i], "-t") == 0) {
      test_arg = YES; continue;
    }
    if (strcmp(argv[i], "-x") == 0) {
      exit_arg = YES; continue;
    }
    CHECK(file_to_load == NULL, "Only one file can be loaded at once.");
    file_to_load = argv[i];
  }
}

int main(int argc, char* argv[]) {
  parse_args(argc, argv);
  init();
  if (deserialize_arg) {
    gc_deserialize_from_file("fn.img");
  } else {
    init_decls();
    if (load_twice_arg) {
      /*
       * Evaluate top level forms again.  The second time, they will
       * all be compiled.  This compiled even the more dubious stuff
       * like functions that haven't been originally defined at
       * top-level.
       */
      init_decls();
    }
  }
  if (serialize_arg) {
    gc_serialize_to_file("fn.img");
  }
  if (exit_arg) {
    exit(0);
  }
  if (test_arg) {
    CHECK(file_to_load == NULL, "Can't run tests *and* load a file.");
    run_all_tests();
  } else if (file_to_load != NULL) {
    load_file(file_to_load);
  } else {
    puts("FN " __DATE__ ".");
    repl();
    exit(0);
  }
  return 0;
}

