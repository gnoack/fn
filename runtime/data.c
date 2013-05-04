
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
  return get_smallint(symbol.mem[2]);
}


oop make_array(fn_uint array_size) {
  fn_uint header_size = 1;
  oop result = mem_alloc(header_size + array_size);
  mem_set(result, 0, symbols._array);
  fn_uint i;
  for (i = 0; i < array_size; i++) {
    mem_set(result, header_size + i, NIL);
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
  if (!is_mem(array)) {
    return NO;
  }
  return value_eq(symbols._array, mem_get(array, 0));
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

oop make_dict(fn_uint table_size) {
  oop result = mem_alloc(3);
  mem_set(result, 0, symbols._dict);
  mem_set(result, 1, make_smallint(0L));
  mem_set(result, 2, make_array(table_size * 2));
  return result;
}

fn_uint dict_size(oop dict) {
  return get_smallint(mem_get(dict, 1));
}

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
  if (!is_mem(dict)) {
    return NO;
  }
  return value_eq(symbols._dict, mem_get(dict, 0));
}


/*
 * Lisp interface.
 */

oop primitive_symbol_to_hash(oop args) {
  PARSE_ONE_ARG(sym);
  return make_smallint(symbol_to_hash(sym));
}

oop primitive_dict_get(oop args) {
  PARSE_TWO_ARGS(dict, key);
  return dict_get(dict, key);
}

oop primitive_dict_has_key(oop args) {
  PARSE_TWO_ARGS(dict, key);
  return lisp_bool(dict_has_key(dict, key));
}

oop primitive_dict_put(oop args) {
  PARSE_THREE_ARGS(dict, key, value);
  dict_put(dict, key, value);
  return value;
}

oop primitive_dict_key_value_pairs(oop args) {
  PARSE_ONE_ARG(dict);
  return dict_key_value_pairs(dict);
}

oop primitive_make_dict(oop args) {
  CHECK(is_nil(args), "make-dict takes no arguments.");
  return make_dict(5);
}


/*
 * Fixed-size dynamic frames.
 *
 * +---------+------+------+----+----+-----+----+----+----+-----+----+
 * | @dframe | size | next | k1 | k2 | ... | kn | v1 | v2 | ... | vn |
 * +---------+------+------+----+----+-----+----+----+----+-----+----+
 *
 * `size' is the number of variables.
 * `next' is the next frame.
 *
 * In order to set a variable in this frame, it first needs to be registered.
 */

#define DFRAME_HEADER_SIZE 3

// Construct a fixed-size dynamic frame.
oop make_dframe(oop next_frame, fn_uint size) {
  oop result = mem_alloc(DFRAME_HEADER_SIZE + size * 2);
  mem_set(result, 0, symbols._dframe);
  mem_set(result, 1, make_smallint(size));
  mem_set(result, 2, next_frame);
  return result;
}

void dframe_register_key(oop dframe, fn_uint pos, oop key, oop value) {
  fn_uint frame_size = get_smallint(mem_get(dframe, 1));
  CHECK(pos < frame_size, "Index out of bounds.");
  mem_set(dframe, DFRAME_HEADER_SIZE + pos, key);
  mem_set(dframe, DFRAME_HEADER_SIZE + frame_size + pos, value);
}

void dframe_set(oop dframe, oop key, oop value) {
  for (;;) {
    if (is_global_env(dframe)) {
      set_globally_oop(key, value);
      return;
    }

    fn_uint size = get_smallint(mem_get(dframe, 1));
    int i;
    for (i=0; i<size; i++) {
      oop current_key = mem_get(dframe, DFRAME_HEADER_SIZE + i);
      if (value_eq(key, current_key)) {
        mem_set(dframe, DFRAME_HEADER_SIZE + size + i, value);
        return;
      }
    }
    // Not found, try next dframe.
    dframe = mem_get(dframe, 2);
  }
}

oop dframe_get(oop dframe, oop key) {
  for (;;) {
    if (is_global_env(dframe)) {
      return lookup_globally(key);
    }

    fn_uint size = get_smallint(mem_get(dframe, 1));
    int i;
    for (i=0; i<size; i++) {
      oop current_key = mem_get(dframe, DFRAME_HEADER_SIZE + i);
      if (value_eq(key, current_key)) {
        return mem_get(dframe, DFRAME_HEADER_SIZE + size + i);
      }
    }
    // Not found, try next dframe.
    dframe = mem_get(dframe, 2);
  }
}


// Lisp interface for dframes.

oop primitive_make_dframe(oop args) {
  oop next_frame = first(args);
  args = rest(args);

  if (is_nil(next_frame)) {
    next_frame = global_env;
  }

  fn_uint size = length_int(args);
  oop result = make_dframe(next_frame, size);
  fn_uint i;
  for (i=0; i<size; i++) {
    oop key = first(args);
    CHECKV(is_symbol(key), key, "Needs to be a symbol.");
    dframe_register_key(result, i, key, NIL);
    args = rest(args);
  }
  return result;
}

oop primitive_dframe_set(oop args) {
  PARSE_THREE_ARGS(dframe, key, value);
  dframe_set(dframe, key, value);
  return value;
}

oop primitive_dframe_get(oop args) {
  PARSE_TWO_ARGS(dframe, key);
  return dframe_get(dframe, key);
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
  register_globally_fn("make-dframe", primitive_make_dframe);
  register_globally_fn("dframe-get", primitive_dframe_get);
  register_globally_fn("dframe-set!", primitive_dframe_set);
}
