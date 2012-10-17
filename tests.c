
#include "tests.h"

#include "arrays.h"
#include "compiler.h"
#include "data.h"
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
#include "compiler-test.h"
#include "cons-test.h"
#include "data-test.h"
#include "dispatcher-test.h"
#include "gc-test.h"
#include "interpreter-test.h"
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
  global_env = gc_run(global_env);
}

void init() {
  init_procedures();
  init_gc();
  init_symbols();
  init_eval();
  init_primitives();
  init_interpreter();
  init_data();
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
  load_decls(compiler_decls());
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

void compile_system() {
  char* status = "-\\|/";
  int statusidx = 0;

  oop uncompiled_functions = NIL;
  for (;;) {
    // TODO: Querying all of this everytime from scratch is slow.
    uncompiled_functions =
      apply(LIST(lookup_globally(make_symbol("uncompiled-functions"))));
    if (is_nil(uncompiled_functions)) {
      break;
    }
    oop symbol = first(uncompiled_functions);
    printf("\rCompiling [%c] %-60s", status[statusidx], symbol.symbol);
    fflush(stdout);

    eval_global(LIST(make_symbol("c!"), symbol));
    
    global_env = gc_run(global_env);
    statusidx = (statusidx + 1) & 3;
  }
  printf("\rCompiling done.                                \n");
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
    oop sexpr = eval_global(LIST(make_symbol("read-all"), cmd));
    print_value(eval_global(sexpr));
    
    // GC if needed.
    global_env = gc_run(global_env);

    free(input);
  }
}

boolean test_arg = NO;
boolean compile_arg = NO;

void parse_args(int argc, char* argv[]) {
  int i;
  for (i=1; i<argc; i++) {
    if (strcmp(argv[i], "-t") == 0) {
      test_arg = YES;
    }
    if (strcmp(argv[i], "-c") == 0) {
      compile_arg = YES;
    }
  }
}

int main(int argc, char* argv[]) {
  init();
  parse_args(argc, argv);
  if (compile_arg) {
    compile_system();
  }
  if (test_arg == NO) {
    puts("FN " __DATE__ ".");
    repl();
    exit(0);
  }
  printf("Test execution:\n");
  /* Register tests here. */
  gc_tests();
  interpreter_tests();
  cons_tests();
  interning_tests();
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
  compiler_tests();
  data_tests();
  /* Summing up. */
  printf("\n%d assertions executed, %d failures.\n",
	 assertion_count, failure_count);
  if (failure_count == 0) {
    printf("Well done!\n");
  }
  return 0;
}
