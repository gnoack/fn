#ifndef _DATA_H_

#include "value.h"

// Dictionaries.
extern oop make_dict(fn_uint table_size);
extern oop dict_get(oop dict, oop key);
extern boolean dict_has_key(oop dict, oop key);
extern oop dict_put(oop dict, oop key, oop value);

// Dframes.
extern oop make_dframe(oop next_frame, fn_uint size);
extern void dframe_register_key(oop dframe, fn_uint pos, oop key, oop value);
extern void dframe_set(oop dframe, oop key, oop value);
extern oop dframe_get(oop dframe, oop key);

extern void init_data();

#define _DATA_H_ 0
#endif  // _DATA_H_
