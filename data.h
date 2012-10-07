#ifndef _DATA_H_

#include "value.h"

extern oop make_dframe(oop next_frame, fn_uint size);
extern void dframe_register_key(oop dframe, fn_uint pos, oop key);
extern void dframe_set(oop dframe, oop key, oop value);
extern oop dframe_get(oop dframe, oop key);

extern void init_data();

#define _DATA_H_ 0
#endif  // _DATA_H_
