#ifndef _MEMORY_H_
#define _MEMORY_H_

#include "value.h"

// #define MEM_SET_DEBUG

#ifdef MEMORY_DEBUG
#define MEM_GET(obj,idx) mem_get((obj),(idx))
#define MEM_SET(obj,idx,val) mem_set((obj),(idx),(val))
#else  // MEMORY_DEBUG
#define MEM_GET(obj,idx) (obj).mem[(idx)]
#define MEM_SET(obj,idx,val) ((obj).mem[(idx)] = (val))
#endif  // MEMORY_DEBUG

extern oop mem_alloc(fn_uint amount);
extern oop mem_set(oop target, fn_uint index, oop value);
extern oop mem_get(oop target, fn_uint index);
extern fn_uint mem_size(oop target);

extern oop mem_raw_mem_alloc(fn_uint amount);
extern oop mem_raw_mem_get(oop target, fn_uint index);
extern void mem_raw_mem_set(oop target, fn_uint index, fn_uint value);
// Actual size will usually be higher than originally requested.
extern fn_uint mem_raw_mem_size(oop target);
extern void* mem_raw_get_ptr(oop target);

#endif  // _MEMORY_H_
