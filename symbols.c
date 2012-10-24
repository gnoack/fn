
#include "gc.h"
#include "value.h"
#include "memory.h"
#include "symbols.h"

symbols_t symbols;

void symbols_enumerate_oop_places(void (*accept)(oop* place)) {
  // Symbols.
  accept(&symbols._if);
  accept(&symbols._def);
  accept(&symbols._lambda);
  accept(&symbols._let);
  accept(&symbols._true);
  accept(&symbols._false);
  accept(&symbols._quote);
  accept(&symbols._rest);
  accept(&symbols._macroexpand);
  accept(&symbols._set);
  // Types.
  accept(&symbols._array);
  accept(&symbols._cons);
  accept(&symbols._dict);
  accept(&symbols._frame);
  accept(&symbols._string);
  accept(&symbols._procedure);
  accept(&symbols._native_procedure);
  accept(&symbols._compiled_procedure);
}

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
  symbols._cons = mem_alloc(2);
  symbols._dict = mem_alloc(2);
  symbols._frame = mem_alloc(2);
  symbols._string = mem_alloc(2);
  symbols._procedure = mem_alloc(2);
  symbols._native_procedure = mem_alloc(2);
  symbols._compiled_procedure = mem_alloc(2);

  gc_register_persistent_refs(symbols_enumerate_oop_places);
  initialized = YES;
}
