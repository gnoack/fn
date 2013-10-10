#include "runtime/value.h"
#include "runtime/eval.h"
#include "runtime/primitives.h"
#include "runtime/cons.h"
#include "runtime/strings.h"

#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

static
char* symbol_completion_entry(const char* text, int state) {
  oop result =
    apply(LIST(lookup_globally(make_symbol("readline-completion-entry")),
               make_string(text), make_smallint(state), global_env));
  if (is_nil(result)) {
    return NULL;
  } else {
    return c_string(result);
  }
}

static
oop primitive_add_history(oop args) {
  PARSE_ONE_ARG(item);
  char* c_item = c_string(item);
  add_history(c_item);
  free(c_item);
  return item;
}

static
oop primitive_read_history(oop args) {
  PARSE_ONE_ARG(filename);
  char* c_filename = c_string(filename);
  read_history(c_filename);
  free(c_filename);
  return filename;
}

static
oop primitive_write_history(oop args) {
  PARSE_ONE_ARG(filename);
  char* c_filename = c_string(filename);
  write_history(c_filename);
  free(c_filename);
  return filename;
}

static
oop primitive_readline(oop args) {
  PARSE_ONE_ARG(prompt);
  char* c_prompt = c_string(prompt);
  char* c_result = readline(c_prompt);
  oop result = NIL;
  if (c_result) {
    result = make_string(c_result);
  }
  free(c_prompt);
  free(c_result);
  return result;
}

static
oop primitive_get_home_directory(oop args) {
  CHECKV(is_nil(args), args, "Expected 0, but got args.");
  return make_string(getenv("HOME"));
}

extern
void init_readline() {
  rl_completion_entry_function = symbol_completion_entry;

  register_globally_fn("readline", primitive_readline);
  register_globally_fn("read-history", primitive_read_history);
  register_globally_fn("add-history", primitive_add_history);
  register_globally_fn("write-history", primitive_write_history);
  register_globally_fn("get-home-directory", primitive_get_home_directory);
}
