
#include "data.h"

#include "cons.h"
#include "value.h"
#include "memory.h"
#include "primitives.h"
#include "strings.h"
#include "symbols.h"



fn_uint symbol_to_hash(oop symbol) {
  CHECKV(is_symbol(symbol), symbol, "Must be a symbol for hashing it.");
  return (symbol.smallint >> 1);
}

oop primitive_symbol_to_hash(oop args) {
  PARSE_ONE_ARG(sym);
  return make_smallint(symbol_to_hash(sym));
}



oop make_array(fn_uint array_size) {
  fn_uint header_size = 2;
  oop result = mem_alloc(header_size + array_size);
  mem_set(result, 0, symbols._dict);
  mem_set(result, 1, make_smallint(array_size));  // Size
  fn_uint i;
  for (i = 0; i < array_size; i++) {
    mem_set(result, header_size + i, NIL);
  }
  return result;
}

fn_uint array_size(oop array) {
  return get_smallint(mem_get(array, 1));
}

oop array_set(oop array, fn_uint index, oop value) {
  // mem_set catches index out of bounds already.
  return mem_set(array, 2 + index, value);
}

oop array_get(oop array, fn_uint index) {
  // mem_get catches index out of bounds already.
  return mem_get(array, 2 + index);
}


/*
 * Dictionary
 *
 * +-------+------+-------+
 * | @dict | size | table |------> (array)
 * +-------+------+-------+
 *
 * `size' is the number of elements in the dictionary.
 * The table size is saved in the table itself.
 */

fn_uint dict_size(oop dict) {
  return get_smallint(mem_get(dict, 1));
}

oop dict_table(oop dict) {
  return mem_get(dict, 2);
}

fn_uint dict_table_size(oop dict) {
  return array_size(dict_table(dict));
}

oop primitive_make_dict(oop args) {
  CHECK(is_nil(args), "make-dict takes no arguments.");
  fn_uint table_size = 5;
  oop result = mem_alloc(3);
  mem_set(result, 0, NIL);  // TODO: Type marker.
  mem_set(result, 1, make_smallint(0L));
  mem_set(result, 2, make_array(table_size));
  return result;
}

// In `oop's, not in items.
#define INNER_TABLE_SIZE 14

// Returns 1 if element was put, 0 if it was replced.
int inner_table_put(oop inner_table, oop key, oop value) {
  int i;
  int first_good_index = -1;
  for (i=0; i<INNER_TABLE_SIZE; i+=2) {
    oop stored_key = array_get(inner_table, i);
    if (value_eq(stored_key, key)) {
      array_set(inner_table, i + 1, value);
      return 0;
    } else if (first_good_index == -1 && is_nil(stored_key)) {
      first_good_index = i;
    }
  }
  if (first_good_index == -1) {
    printf("No space to put key in dictionary.");
    exit(1);
  }
  array_set(inner_table, first_good_index, key);
  array_set(inner_table, first_good_index + 1, value);
  return 1;
}

void inner_table_remove(oop inner_table, oop key) {
  int i;
  for (i=0; i<INNER_TABLE_SIZE; i+=2) {
    oop stored_key = array_get(inner_table, i);
    if (value_eq(stored_key, key)) {
      array_set(inner_table, i, NIL);
      return;
    }
  }
  CHECKV(1==0, key, "Key not found in dictionary.");
}

oop inner_table_get(oop inner_table, oop key) {
  int i;
  for (i=0; i<INNER_TABLE_SIZE; i+=2) {
    oop stored_key = array_get(inner_table, i);
    if (value_eq(stored_key, key)) {
      return array_get(inner_table, i + 1);
    }
  }
  CHECKV(1==0, key, "Key not found in dictionary.");
}

oop inner_table_key_value_pairs(oop inner_table, oop accumulator) {
  int i;
  for (i=0; i<INNER_TABLE_SIZE; i+=2) {
    oop key = array_get(inner_table, i);
    if (!is_nil(key)) {
      oop value = array_get(inner_table, i + 1);
      accumulator = make_cons(make_cons(key, value), accumulator);
    }
  }
  return accumulator;
}


oop dict_table_get_inner_table(oop table, oop key, boolean create_if_needed) {
  fn_uint idx = symbol_to_hash(key) % array_size(table);
  oop result = array_get(table, idx);
  if (is_nil(result)) {
    CHECKV(create_if_needed, key, "Key not found in dictionary.");
    result = make_array(INNER_TABLE_SIZE);
    array_set(table, idx, result);
  }
  return result;
}

// Returns 1 if element was put, 0 if it was replced.
int dict_table_put(oop table, oop key, oop value) {
  oop inner_table = dict_table_get_inner_table(table, key, YES);
  return inner_table_put(inner_table, key, value);
}

oop dict_table_get(oop table, oop key) {
  oop inner_table = dict_table_get_inner_table(table, key, NO);
  return inner_table_get(inner_table, key);
}

void dict_table_remove(oop table, oop key) {
  oop inner_table = dict_table_get_inner_table(table, key, NO);
  inner_table_remove(inner_table, key);
}

oop dict_table_key_value_pairs(oop table) {
  fn_uint table_size = array_size(table);
  oop result = NIL;
  int i;
  for (i=0; i<table_size; i++) {
    oop inner_table = array_get(table, i);
    if (!is_nil(inner_table)) {
      result = inner_table_key_value_pairs(inner_table, result);
    }
  }
  return result;
}

// Destructively resizes the dictionary to a new size.
void dict_resize(oop dict, fn_uint new_table_size) {
  oop old_table = dict_table(dict);
  oop new_table = make_array(new_table_size);
  fn_uint old_table_size = array_size(old_table);
  int i, j;
  for (i=0; i<old_table_size; i++) {
    oop inner_table = array_get(old_table, i);
    if (is_nil(inner_table)) {
      continue;
    }
    for (j=0; j<INNER_TABLE_SIZE; j+=2) {
      oop key = array_get(inner_table, j);
      if (!is_nil(key)) {
        oop value = array_get(inner_table, j + 1);
        dict_table_put(new_table, key, value);
      }
    }
  }
  mem_set(dict, 2, new_table);
}

void dict_change_count(oop dict, int amount) {
  fn_uint old_count = dict_size(dict);
  fn_uint table_size = dict_table_size(dict);
  fn_uint new_count = old_count + amount;
  // TODO: Tweak factor.
  if (new_count > table_size * 4) {
    dict_resize(dict, table_size * 2);
  }
  mem_set(dict, 1, make_smallint(new_count));
}



oop dict_get(oop dict, oop key) {
  return dict_table_get(dict_table(dict), key);
}

void dict_remove(oop dict, oop key) {
  dict_change_count(dict, -1);
  dict_table_remove(dict_table(dict), key);
}

oop dict_put(oop dict, oop key, oop value) {
  int added_amount = dict_table_put(dict_table(dict), key, value);
  dict_change_count(dict, added_amount);
}



oop primitive_dict_get(oop args) {
  PARSE_TWO_ARGS(dict, key);
  return dict_get(dict, key);
}

oop primitive_dict_remove(oop args) {
  PARSE_TWO_ARGS(dict, key);
  dict_remove(dict, key);
  return dict;
}

oop primitive_dict_put(oop args) {
  PARSE_THREE_ARGS(dict, key, value);
  dict_put(dict, key, value);
  return value;
}

oop primitive_dict_key_value_pairs(oop args) {
  PARSE_ONE_ARG(dict);
  return dict_table_key_value_pairs(dict_table(dict));
}

void init_data() {
  // Some simple utilities defined in utils.fn.
  register_globally_fn("symbol->hash", primitive_symbol_to_hash);
  register_globally_fn("make-dict", primitive_make_dict);
  register_globally_fn("dict-get", primitive_dict_get);
  register_globally_fn("dict-put!", primitive_dict_put);
  register_globally_fn("dict-remove!", primitive_dict_remove);
  register_globally_fn("dict-key-value-pairs",
                       primitive_dict_key_value_pairs);
}

