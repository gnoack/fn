/*
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#include "runtime/value.h"
#include "runtime/eval.h"
#include "runtime/primitives.h"
#include "runtime/cons.h"
#include "runtime/strings.h"

#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

static char* symbol_completion_entry(const char* text, int state) {
  oop result =
    apply(LIST(lookup_globally(make_symbol("readline-completion-entry")),
               make_string(text), make_smallint(state), global_env));
  if (is_nil(result)) {
    return NULL;
  } else {
    return c_string(result);
  }
}

static FUNC(primitive_add_history) {
  PARSE_ONE_ARG(item);
  char* c_item = c_string(item);
  add_history(c_item);
  free(c_item);
  return item;
}

static FUNC(primitive_read_history) {
  PARSE_ONE_ARG(filename);
  char* c_filename = c_string(filename);
  read_history(c_filename);
  free(c_filename);
  return filename;
}

static FUNC(primitive_write_history) {
  PARSE_ONE_ARG(filename);
  char* c_filename = c_string(filename);
  write_history(c_filename);
  free(c_filename);
  return filename;
}

static FUNC(primitive_readline) {
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

static FUNC(primitive_get_home_directory) {
  CHECK(argc == 0, "Expected 0, but got args.");
  return make_string(getenv("HOME"));
}

void init_readline() {
  rl_completion_entry_function = symbol_completion_entry;

  register_globally_fn("readline", primitive_readline);
  register_globally_fn("read-history", primitive_read_history);
  register_globally_fn("add-history", primitive_add_history);
  register_globally_fn("write-history", primitive_write_history);
  register_globally_fn("get-home-directory", primitive_get_home_directory);
}
