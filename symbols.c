
#include "value.h"
#include "memory.h"
#include "symbols.h"

symbols_t symbols;

void init_symbols() {
  static bool initialized = NO;
  if (initialized) return;
  symbols._if = make_symbol("if");
  symbols._def = make_symbol("def");
  symbols._lambda = make_symbol("lambda");
  symbols._let = make_symbol("let");
  symbols._true = make_symbol("true");
  symbols._false = make_symbol("false");
  symbols._quote = make_symbol("quote");
  symbols._rest = make_symbol("&rest");
  symbols._macroexpand = make_symbol("macroexpand");
  symbols._native_procedure_marker = make_symbol("native");
  symbols._lisp_procedure_marker = make_symbol("procedure");
  // TODO: This is not actually a symbol, but a type.
  // The @cons type is finished when types are initialized.
  symbols._cons = mem_alloc(2);
  initialized = YES;
}
