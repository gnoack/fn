
#include <string.h>  // strdup

#include "debug.h"
#include "gc.h"
#include "memory.h"
#include "strings.h"
#include "symbols.h"
#include "value.h"

/* Hash map for looking up symbols. */
#define HASH_MAP_SIZE 8192
fn_uint number_of_symbols = 0;
char* interned_symbols_keys[HASH_MAP_SIZE];
oop interned_symbols_values[HASH_MAP_SIZE];

fn_uint string_to_hash(const char* str) {
  fn_uint value = 1;
  while (*str) {
    value = value * 7 + *str;
    str++;
  }
  return value;
}

fn_uint find_place(const char* key) {
  fn_uint i = string_to_hash(key);
  for (;;) {
    i = (i + 1) % HASH_MAP_SIZE;
    char* current_key = interned_symbols_keys[i];
    if (!current_key) {
      break;
    }
    if (!strcmp(current_key, key)) {
      break;
    }
  }
  return i;
}

oop construct_symbol(const char* str) {
  int len = strlen(str);

  fn_uint hash = string_to_hash(str);
  hash = (hash << 1) >> 1;  // Strip most significant bit.

  oop raw_mem = mem_raw_mem_alloc(len + 1);
  char* target = (char*) raw_mem.mem;
  memcpy(target, str, len);
  target[len] = '\0';

  oop result = mem_alloc(3);
  MEM_SET(result, 0, symbols._symbol);
  MEM_SET(result, 1, raw_mem);
  MEM_SET(result, 2, make_smallint(hash));
  return result;
}

oop make_or_lookup_symbol(const char* str) {
  fn_uint idx = find_place(str);
  if (interned_symbols_keys[idx]) {
    return interned_symbols_values[idx];
  } else {
    number_of_symbols++;
    // TODO: Implement resizing.
    CHECK(number_of_symbols < HASH_MAP_SIZE, "Too many symbols.");
    interned_symbols_keys[idx] = strdup(str);
    interned_symbols_values[idx] = construct_symbol(str);
    return interned_symbols_values[idx];
  }
}

void symbol_hashmap_clear() {
  bzero(interned_symbols_keys, sizeof(char*) * HASH_MAP_SIZE);
  bzero(interned_symbols_values, sizeof(oop) * HASH_MAP_SIZE);
}

/*
 * Register a new symbol in the symbol map.  The passed symbol is not
 * created through make_or_lookup_symbol, but through a side-channel
 * like deserializing the complete heap from a file.
 *
 * You may also pass other objects, in which case this will return
 * without doing any work.
 */
void symbol_hashmap_register(oop symbol) {
  if (!is_symbol(symbol)) {
    return;
  }
  // TODO: Use hash value from symbol itself.
  const char* str = get_symbol(symbol);
  fn_uint idx = find_place(str);
  if (interned_symbols_keys[idx]) {
    CHECKV(value_eq(interned_symbols_values[idx], symbol),
           symbol, "Conflicting symbol instances; should not happen.");
    return;
  } else {
    interned_symbols_keys[idx] = strdup(str);
    interned_symbols_values[idx] = symbol;
    return;
  }
}



symbols_t symbols;

void symbols_enumerate_oop_places(void (*accept)(oop* place)) {
  // Symbols.
  accept(&symbols._if);
  accept(&symbols._def);
  accept(&symbols._lambda);
  accept(&symbols._let);
  accept(&symbols._true);
  accept(&symbols._false);
  accept(&symbols._progn);
  accept(&symbols._quote);
  accept(&symbols._rest);
  accept(&symbols._macroexpand);
  accept(&symbols._set);
  // Types.
  accept(&symbols._array);
  accept(&symbols._cons);
  accept(&symbols._continuation);
  accept(&symbols._dframe);
  accept(&symbols._dict);
  accept(&symbols._frame);
  accept(&symbols._stack);
  accept(&symbols._string);
  accept(&symbols._symbol);
  accept(&symbols._procedure);
  accept(&symbols._native_procedure);
  accept(&symbols._compiled_procedure);
  // C Types.
  accept(&symbols._c_int);
  accept(&symbols._c_str);
  accept(&symbols._c_void);
  // Interned symbols.
  fn_uint i;
  for (i=0; i<HASH_MAP_SIZE; i++) {
    accept(&interned_symbols_values[i]);
  }
}

void init_symbols() {
  static boolean initialized = NO;
  if (initialized) return;
  // Note: Symbol map must be zeroed before creating symbols.
  symbol_hashmap_clear();

  // TODO: These are not actually symbols, but types.
  // The types are finished when types are initialized.
  symbols._array = mem_alloc(3);
  symbols._cons = mem_alloc(3);
  symbols._continuation = mem_alloc(3);
  symbols._dframe = mem_alloc(3);
  symbols._dict = mem_alloc(3);
  symbols._frame = mem_alloc(3);
  symbols._stack = mem_alloc(3);
  symbols._string = mem_alloc(3);
  symbols._symbol = mem_alloc(3);  // Must be known before creating symbols.
  symbols._procedure = mem_alloc(3);
  symbols._native_procedure = mem_alloc(3);
  symbols._compiled_procedure = mem_alloc(3);

  symbols._if = make_symbol("if");
  symbols._def = make_symbol("def");
  symbols._lambda = make_symbol("lambda");
  symbols._let = make_symbol("let");
  symbols._true = make_symbol("true");
  symbols._false = make_symbol("false");
  symbols._progn = make_symbol("progn");
  symbols._quote = make_symbol("quote");
  symbols._rest = make_symbol("&rest");
  symbols._macroexpand = make_symbol("macroexpand");
  symbols._set = make_symbol("set!");

  symbols._c_int = make_symbol("int");
  symbols._c_str = make_symbol("str");
  symbols._c_void = make_symbol("void");

  gc_register_persistent_refs(symbols_enumerate_oop_places);

  initialized = YES;
}
