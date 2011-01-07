
#include "value.h"

#include "symbols.h"

symbols_t symbols;

void init_symbols() {
  static bool initialized = NO;
  if (initialized) return;
  symbols._if = make_symbol("if");
  symbols._lambda = make_symbol("lambda");
  symbols._let = make_symbol("let");
  symbols._native_procedure_marker = make_symbol("native");
  symbols._lisp_procedure_marker = make_symbol("procedure");
  initialized = YES;
}
