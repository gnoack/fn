#ifndef _MEMORY_H_

#include "value.h"

extern oop mem_alloc(fn_uint amount);
extern oop mem_set(oop target, fn_uint index, oop value);
extern oop mem_get(oop target, fn_uint index);
extern fn_uint mem_size(oop target);

extern oop mem_raw_mem_alloc(fn_uint amount);
extern oop mem_raw_mem_get(oop target, fn_uint index);
extern void mem_raw_mem_set(oop target, fn_uint index, fn_uint value);

#define _MEMORY_H_ 0
#endif  // _MEMORY_H_
