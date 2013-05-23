#include "arrays.h"
#include "byte-buffer.h"
#include "compiler.h"
#include "cons.h"
#include "continuations.h"
#include "data.h"
#include "debug.h"
#include "deserialize.h"
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
#include "serialize.h"
#include "strings.h"
#include "symbols.h"
#include "utils.h"
#include "value.h"

#include <stdio.h>
#include <string.h>

void fn_runtime_init() {
  init_gc();
  init_symbols();
  init_eval();
  init_primitives();
  init_interpreter();
  init_data();
  init_dl();
  init_deserialize();
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
  load_decls(serialize_decls());
  load_decls(continuations_decls());
}

void fn_load_file(const char* filename) {
  apply(LIST(lookup_globally(make_symbol("load-file")),
             make_string(filename)));
}
