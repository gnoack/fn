#ifndef _MEMORY_H_
#define _MEMORY_H_

#include "value.h"

// #define MEMORY_DEBUG

#ifdef MEMORY_DEBUG
#define MEM_GET(obj,idx) mem_get((obj),(idx))
#define MEM_SET(obj,idx,val) mem_set((obj),(idx),(val))
#else  // MEMORY_DEBUG
#define MEM_GET(obj,idx) (obj).mem[(idx)]
#define MEM_SET(obj,idx,val) ((obj).mem[(idx)] = (val))
#endif  // MEMORY_DEBUG

#define MemAlloc(type) ((type*) mem_alloc(sizeof(type) / sizeof(oop)).mem)

oop mem_alloc(fn_uint amount);
oop mem_set(oop target, fn_uint index, oop value);
oop mem_get(oop target, fn_uint index);
fn_uint mem_size(oop target);

oop mem_raw_mem_alloc(fn_uint amount);
oop mem_raw_mem_make(const void* buf, fn_uint size);
oop mem_raw_mem_get(oop target, fn_uint index);
void mem_raw_mem_set(oop target, fn_uint index, fn_uint value);
// Actual size will usually be higher than originally requested.
fn_uint mem_raw_mem_size(oop target);
void* mem_raw_get_ptr(oop target);

#endif  // _MEMORY_H_
