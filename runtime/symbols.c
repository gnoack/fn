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
symbol_t* interned_symbols_values[HASH_MAP_SIZE];

// TODO: Strip most significant bit directly?
fn_uint string_to_hash(const char* str) {
  fn_uint value = 1;
  while (*str) {
    value = value * 7 + *str;
    str++;
  }
  return value;
}

static inline
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

static inline
symbol_t* construct_symbol(const char* str) {
  int len = strlen(str);

  fn_uint hash = string_to_hash(str);
  hash = (hash << 1) >> 1;  // Strip most significant bit.

  oop raw_mem = mem_raw_mem_alloc(len + 1);
  char* target = (char*) raw_mem.mem;
  memcpy(target, str, len);
  target[len] = '\0';

  symbol_t* result = MemAlloc(symbol_t);
  *result = (symbol_t) {
    .type    = symbols._symbol,
    .raw_mem = raw_mem,
    .hash    = make_smallint(hash)
  };
  return result;
}

symbol_t* make_or_lookup_symbol(const char* str) {
  fn_uint idx = find_place(str);
  if (interned_symbols_keys[idx] == NULL) {
    number_of_symbols++;
    // TODO: Implement resizing.
    CHECK(number_of_symbols < HASH_MAP_SIZE, "Too many symbols.");
    interned_symbols_keys[idx] = strdup(str);
    interned_symbols_values[idx] = construct_symbol(str);
  }
  return interned_symbols_values[idx];
}

void symbol_hashmap_clear() {
  bzero(interned_symbols_keys, sizeof(char*) * HASH_MAP_SIZE);
  bzero(interned_symbols_values, sizeof(symbol_t*) * HASH_MAP_SIZE);
}

/*
 * Register a new symbol in the symbol map.  The passed symbol is not
 * created through make_or_lookup_symbol, but through a side-channel
 * like deserializing the complete heap from a file.
 *
 * You may also pass other objects, in which case this will return
 * without doing any work.
 */
void symbol_hashmap_register(oop obj) {
  if (!is_symbol(obj)) { return; }

  symbol_t* symbol = to_symbol(obj);
  // TODO: Use hash value from symbol itself.
  const char* str = get_symbol(symbol);
  fn_uint idx = find_place(str);
  if (interned_symbols_keys[idx]) {
    CHECKV(interned_symbols_values[idx] == symbol, symbol_to_oop(symbol),
	   "Conflicting symbol instances; should not happen.");
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
  accept((oop*) &symbols._if);
  accept((oop*) &symbols._def);
  accept((oop*) &symbols._lambda);
  accept((oop*) &symbols._let);
  accept(&symbols._true);
  accept(&symbols._false);
  accept((oop*) &symbols._progn);
  accept((oop*) &symbols._quote);
  accept((oop*) &symbols._rest);
  accept((oop*) &symbols._macroexpand);
  accept((oop*) &symbols._set);
  accept((oop*) &symbols._mem_get);
  accept((oop*) &symbols._mem_set);
  accept((oop*) &symbols._fields);
  // Types.
  accept(&symbols._array);
  accept(&symbols._cons);
  accept(&symbols._dict);
  accept(&symbols._frame);
  accept(&symbols._stack);
  accept(&symbols._string);
  accept(&symbols._symbol);
  accept(&symbols._defined_var);
  accept(&symbols._undefined_var);
  // Boolean types.
  accept(&symbols._True);
  accept(&symbols._False);
  // Procedure types.
  accept(&symbols._native_procedure);
  accept(&symbols._compiled_procedure);
  // C types.
  accept((oop*) &symbols._c_int);
  accept((oop*) &symbols._c_str);
  accept((oop*) &symbols._c_void);
  // Interned symbols.
  fn_uint i;
  for (i=0; i<HASH_MAP_SIZE; i++) {
    accept((oop*) &interned_symbols_values[i]);
  }
}

void init_symbols() {
  static boolean initialized = NO;
  if (initialized) return;
  // Note: Symbol map must be zeroed before creating symbols.
  symbol_hashmap_clear();

  // TODO: These are not actually symbols, but types.
  // The types are finished when types are initialized.
  symbols._array = mem_alloc(4);
  symbols._cons = mem_alloc(4);
  symbols._dict = mem_alloc(4);
  symbols._frame = mem_alloc(4);
  symbols._stack = mem_alloc(4);
  symbols._string = mem_alloc(4);
  symbols._symbol = mem_alloc(4);  // Must be known before creating symbols.
  symbols._defined_var = mem_alloc(4);
  symbols._undefined_var = mem_alloc(4);
  symbols._native_procedure = mem_alloc(4);
  symbols._compiled_procedure = mem_alloc(4);
  symbols._True = mem_alloc(4);
  symbols._False = mem_alloc(4);

  symbols._if = make_symbol("if");
  symbols._def = make_symbol("def");
  symbols._lambda = make_symbol("lambda");
  symbols._let = make_symbol("let");
  symbols._progn = make_symbol("progn");
  symbols._quote = make_symbol("quote");
  symbols._rest = make_symbol("&rest");
  symbols._macroexpand = make_symbol("macroexpand");
  symbols._set = make_symbol("set!");
  symbols._mem_get = make_symbol("$mem-get");
  symbols._mem_set = make_symbol("$mem-set!");
  symbols._fields = make_symbol("$fields");

  symbols._true = mem_alloc(1);
  mem_set(symbols._true, 0, symbols._True);
  symbols._false = mem_alloc(1);
  mem_set(symbols._false, 0, symbols._False);

  symbols._c_int = make_symbol("int");
  symbols._c_str = make_symbol("str");
  symbols._c_void = make_symbol("void");

  gc_register_persistent_refs(symbols_enumerate_oop_places);

  initialized = YES;
}
