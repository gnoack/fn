
#include "gc.h"
#include "value.h"
#include "memory.h"
#include "symbols.h"

symbols_t symbols;

void init_symbols() {
  static boolean initialized = NO;
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
  symbols._set = make_symbol("set!");
  // TODO: These are not actually symbols, but types.
  // The types are finished when types are initialized.
  symbols._array = mem_alloc(2);
  gc_register_persistent_ref(&symbols._array);
  symbols._cons = mem_alloc(2);
  gc_register_persistent_ref(&symbols._cons);
  symbols._dict = mem_alloc(2);
  gc_register_persistent_ref(&symbols._dict);
  symbols._frame = mem_alloc(2);
  gc_register_persistent_ref(&symbols._frame);
  symbols._string = mem_alloc(2);
  gc_register_persistent_ref(&symbols._string);
  symbols._procedure = mem_alloc(2);
  gc_register_persistent_ref(&symbols._procedure);
  symbols._native_procedure = mem_alloc(2);
  gc_register_persistent_ref(&symbols._native_procedure);
  symbols._compiled_procedure = mem_alloc(2);
  gc_register_persistent_ref(&symbols._compiled_procedure);

  initialized = YES;
}
