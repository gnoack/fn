#include "data.h"

#include "cons.h"
#include "debug.h"
#include "eval.h"
#include "memory.h"
#include "primitives.h"
#include "strings.h"
#include "symbols.h"
#include "value.h"


static fn_uint get_hash(oop obj) {
  if (unlikely(is_char(obj))) {
    return get_char(obj);
  } else if (unlikely(is_cons(obj))) {
    // Look at first 10 items of linked list.
    fn_uint h = 0;
    for (int i = 0; i < 10; i++) {
      if (unlikely(is_nil(obj))) {
        break;
      }
      h = h * 31 + get_hash(first(obj));
      obj = rest(obj);
    }
    // TODO: This is a hacky way to make it fit into smallint.
    return h & 0x7fffff;
  } else if (likely(is_symbol(obj))) {
    return get_smallint(to_symbol(obj)->hash);
  } else {
    FATALV(obj, "Object does not support hashing");
  }
}

static bool key_eq(oop a, oop b) {
  // TODO: Should this use deep equality?
  return value_eq(a, b);
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

bool is_array(oop array) {
  return is_mem(array) && value_eq(symbols._array, MEM_GET(array, 0));
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

static fn_uint dict_size(oop dict) {
  return get_smallint(mem_get(dict, 1));
}

static oop dict_table(oop dict) {
  return mem_get(dict, 2);
}


/*
 * Inner hash table.
 *
 * Table is an array of size 2n where the first n elements are keys,
 * the second n elements are the corresponding values.
 */

// Returns 1 if element was put, 0 if it was replaced.
static int dict_table_put(oop table, oop key, oop value) {
  int size = (int) array_size(table) >> 1;
  int i = get_hash(key) % size;
  oop current_key;
  do {
    i = (i + 1) % size;
    current_key = array_get(table, i);
    if (key_eq(current_key, key)) {
      array_set(table, size + i, value);
      return 0;
    }
  } while (!is_nil(current_key));
  array_set(table, i, key);
  array_set(table, size + i, value);
  return 1;
}

static oop dict_table_get(oop table, oop key) {
  int size = (int) array_size(table) >> 1;
  int i = get_hash(key) % size;
  oop current_key;
  do {
    i = (i + 1) % size;
    current_key = array_get(table, i);
    if (key_eq(current_key, key)) {
      return array_get(table, size + i);
    }
  } while (!is_nil(current_key));
  FATALV(key, "Key not found.");
}

static bool dict_table_has_key(oop table, oop key) {
  int size = (int) array_size(table) >> 1;
  int i = get_hash(key) % size;
  oop current_key;
  do {
    i = (i + 1) % size;
    current_key = array_get(table, i);
    if (key_eq(current_key, key)) {
      return true;
    }
  } while (!is_nil(current_key));
  return false;
}

static oop dict_table_key_value_pairs(oop table) {
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
static void dict_resize(oop dict, fn_uint new_table_size) {
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

static void dict_change_count(oop dict, int amount) {
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

bool dict_has_key(oop dict, oop key) {
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

bool is_dict(oop dict) {
  return is_mem(dict) && value_eq(symbols._dict, MEM_GET(dict, 0));
}


/*
 * Lisp interface.
 */

FUNC(primitive_get_hash) {
  PARSE_ONE_ARG(sym);
  return make_smallint(get_hash(sym));
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
  register_globally_fn("hash", primitive_get_hash);
  register_globally_fn("make-dict", primitive_make_dict);
  register_globally_fn("dict-get", primitive_dict_get);
  register_globally_fn("dict-put!", primitive_dict_put);
  register_globally_fn("dict-has-key?", primitive_dict_has_key);
  register_globally_fn("dict-key-value-pairs",
                       primitive_dict_key_value_pairs);
}
