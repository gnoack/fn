#include "arrays.h"
#include "byte-buffer.h"
#include "compiler.h"
#include "continuations.h"
#include "data.h"
#include "debug.h"
#include "dispatcher.h"
#include "dl.h"
#include "eval.h"
#include "gc.h"
#include "lang.h"
#include "macros.h"
#include "modules.h"
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
#include "dl-test.h"
#include "eval-test.h"
#include "interpreter-test.h"
#include "lang-test.h"
#include "macros-test.h"
#include "memory-test.h"
#include "modules-test.h"
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
#include <string.h>

void fn_runtime_init() {
  init_procedures();
  init_gc();
  init_symbols();
  init_eval();
  init_primitives();
  init_interpreter();
  init_data();
  init_dl();
}

void fn_runtime_init_lisp_decls() {
  load_decls(lang_decls());
  load_decls(macros_decls());
  load_decls(utils_decls());
  load_decls(modules_decls());
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

void fn_load_file(const char* filename) {
  apply(LIST(lookup_globally(make_symbol("load-file")),
             make_string(filename)));
}
