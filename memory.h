#ifndef _MEMORY_H_

#include "value.h"

extern oop mem_alloc(fn_uint amount);
extern oop mem_set(oop target, fn_uint index, oop value);
extern oop mem_get(oop target, fn_uint index);

#define _MEMORY_H_ 0
#endif  // _MEMORY_H_
