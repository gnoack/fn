#ifndef _DATA_H_
#define _DATA_H_

#include "value.h"

// Arrays.
oop make_array(fn_uint array_size);
fn_uint array_size(oop array);
oop array_set(oop array, fn_uint index, oop value);
oop array_get(oop array, fn_uint index);
bool is_array(oop array);

// Dictionaries.
oop make_dict(fn_uint table_size);
oop dict_get(oop dict, oop key);
bool dict_has_key(oop dict, oop key);
oop dict_put(oop dict, oop key, oop value);
oop dict_key_value_pairs(oop dict);
bool is_dict(oop dict);

// Initialization.
void init_data();

#endif  // _DATA_H_
