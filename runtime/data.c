#include "data.h"

#include "cons.h"
#include "debug.h"
#include "eval.h"
#include "memory.h"
#include "primitives.h"
#include "strings.h"
#include "symbols.h"
#include "value.h"


fn_uint symbol_to_hash(oop symbol) {
  CHECKV(is_symbol(symbol), symbol, "Must be a symbol for hashing it.");
  return get_smallint(to_symbol(symbol)->hash);
}


oop make_array(fn_uint array_size) {
  fn_uint header_size = 1;
  oop result = mem_alloc(header_size + array_size);
  MEM_SET(result, 0, symbols._array);
  fn_uint i;
  for (i = 0; i < array_size; i++) {
    MEM_SET(result, header_size + i, NIL);
  }
  return result;
}

fn_uint array_size(oop array) {
  return mem_size(array) - 1;
}

oop array_set(oop array, fn_uint index, oop value) {
  // mem_set catches index out of bounds already.
  return mem_set(array, 1 + index, value);
}

oop array_get(oop array, fn_uint index) {
  // mem_get catches index out of bounds already.
  return mem_get(array, 1 + index);
}

boolean is_array(oop array) {
  return TO_BOOL(is_mem(array) && value_eq(symbols._array, MEM_GET(array, 0)));
}


/*
 * Dictionary
 *
 * +------+------+-------+
 * | Dict | size | table |------> (array)
 * +------+------+-------+
 *
 * `size' is the number of elements in the dictionary.
 * The table size is saved in the table itself.
 */

oop make_dict(fn_uint table_size) {
  oop result = mem_alloc(3);
  MEM_SET(result, 0, symbols._dict);
  MEM_SET(result, 1, make_smallint(0L));
  MEM_SET(result, 2, make_array(table_size * 2));
  return result;
}

static inline
fn_uint dict_size(oop dict) {
  return get_smallint(mem_get(dict, 1));
}

static inline
oop dict_table(oop dict) {
  return mem_get(dict, 2);
}


/**
 * Inner hash table.
 *
 * Table is an array of size 2n where the first n elements are keys,
 * the second n elements are the corresponding values.
 */

// Returns 1 if element was put, 0 if it was replced.
int dict_table_put(oop table, oop key, oop value) {
  int size = (int) array_size(table) >> 1;
  int i = symbol_to_hash(key) % size;
  oop current_key;
  do {
    i = (i + 1) % size;
    current_key = array_get(table, i);
    if (value_eq(current_key, key)) {
      array_set(table, size + i, value);
      return 0;
    }
  } while (!is_nil(current_key));
  array_set(table, i, key);
  array_set(table, size + i, value);
  return 1;
}

oop dict_table_get(oop table, oop key) {
  int size = (int) array_size(table) >> 1;
  int i = symbol_to_hash(key) % size;
  oop current_key;
  do {
    i = (i + 1) % size;
    current_key = array_get(table, i);
    if (value_eq(current_key, key)) {
      return array_get(table, size + i);
    }
  } while (!is_nil(current_key));
  CHECKV(1==0, key, "Key not found.");
}

boolean dict_table_has_key(oop table, oop key) {
  int size = (int) array_size(table) >> 1;
  int i = symbol_to_hash(key) % size;
  oop current_key;
  do {
    i = (i + 1) % size;
    current_key = array_get(table, i);
    if (value_eq(current_key, key)) {
      return YES;
    }
  } while (!is_nil(current_key));
  return NO;
}

oop dict_table_key_value_pairs(oop table) {
  int size = array_size(table) >> 1;
  int i;
  oop result = NIL;
  for (i=0; i<size; i++) {
    oop key = array_get(table, i);
    if (is_nil(key)) {
      continue;
    }
    oop value = array_get(table, size + i);
    result = make_cons(make_cons(key, value), result);
  }
  return result;
}

// Destructively resizes the dictionary to a new size.
void dict_resize(oop dict, fn_uint new_table_size) {
  oop old_table = dict_table(dict);
  oop new_table = make_array(new_table_size * 2);
  fn_uint old_table_size = array_size(old_table) >> 1;
  int i;
  for (i=0; i<old_table_size; i++) {
    oop key = array_get(old_table, i);
    if (is_nil(key)) {
      continue;
    }
    oop value = array_get(old_table, old_table_size + i);
    dict_table_put(new_table, key, value);
  }
  mem_set(dict, 2, new_table);
}

void dict_change_count(oop dict, int amount) {
  fn_uint old_count = dict_size(dict);
  fn_uint table_size = array_size(dict_table(dict)) >> 1;
  fn_uint new_count = old_count + amount;
  // TODO: Tweak factor.
  if (new_count > table_size / 2) {
    dict_resize(dict, table_size * 2);
  }
  mem_set(dict, 1, make_smallint(new_count));
}


/* Operations on dictionaries. */

// TODO: Implement remove operation.
// See http://en.wikipedia.org/wiki/Open_addressing

oop dict_get(oop dict, oop key) {
  return dict_table_get(dict_table(dict), key);
}

boolean dict_has_key(oop dict, oop key) {
  return dict_table_has_key(dict_table(dict), key);
}

oop dict_put(oop dict, oop key, oop value) {
  // TODO: Only resize when a new entry had to be added.
  int added_amount = dict_table_put(dict_table(dict), key, value);
  dict_change_count(dict, added_amount);
  return value;
}

oop dict_key_value_pairs(oop dict) {
  return dict_table_key_value_pairs(dict_table(dict));
}

boolean is_dict(oop dict) {
  return TO_BOOL(is_mem(dict) && value_eq(symbols._dict, MEM_GET(dict, 0)));
}


/*
 * Lisp interface.
 */

FUNC(primitive_symbol_to_hash) {
  PARSE_ONE_ARG(sym);
  return make_smallint(symbol_to_hash(sym));
}

FUNC(primitive_dict_get) {
  PARSE_TWO_ARGS(dict, key);
  return dict_get(dict, key);
}

FUNC(primitive_dict_has_key) {
  PARSE_TWO_ARGS(dict, key);
  return lisp_bool(dict_has_key(dict, key));
}

FUNC(primitive_dict_put) {
  PARSE_THREE_ARGS(dict, key, value);
  dict_put(dict, key, value);
  return value;
}

FUNC(primitive_dict_key_value_pairs) {
  PARSE_ONE_ARG(dict);
  return dict_key_value_pairs(dict);
}

FUNC(primitive_make_dict) {
  CHECK(argc == 0, "make-dict takes no arguments.");
  return make_dict(5);
}



void init_data() {
  // Some simple utilities defined in utils.fn.
  register_globally_fn("symbol->hash", primitive_symbol_to_hash);
  register_globally_fn("make-dict", primitive_make_dict);
  register_globally_fn("dict-get", primitive_dict_get);
  register_globally_fn("dict-put!", primitive_dict_put);
  register_globally_fn("dict-has-key?", primitive_dict_has_key);
  register_globally_fn("dict-key-value-pairs",
                       primitive_dict_key_value_pairs);
}
