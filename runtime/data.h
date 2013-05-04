#ifndef _DATA_H_
#define _DATA_H_

#include "value.h"

// Arrays.
extern oop make_array(fn_uint array_size);
extern fn_uint array_size(oop array);
extern oop array_set(oop array, fn_uint index, oop value);
extern oop array_get(oop array, fn_uint index);
extern boolean is_array(oop array);

// Dictionaries.
extern oop make_dict(fn_uint table_size);
extern oop dict_get(oop dict, oop key);
extern boolean dict_has_key(oop dict, oop key);
extern oop dict_put(oop dict, oop key, oop value);
extern oop dict_key_value_pairs(oop dict);
extern boolean is_dict(oop dict);

// Dframes.
extern oop make_dframe(oop next_frame, fn_uint size);
extern void dframe_register_key(oop dframe, fn_uint pos, oop key, oop value);
extern void dframe_set(oop dframe, oop key, oop value);
extern oop dframe_get(oop dframe, oop key);

extern void init_data();

#endif  // _DATA_H_
