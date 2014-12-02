#include <malloc.h>
#include <string.h>  // memcpy

#include "debug.h"
#include "gc.h"
#include "memory.h"
#include "value.h"

oop mem_alloc(fn_uint amount) {
  CHECK(amount > 0, "Allocated memory size need to be larger than 0.");
  return gc_object_alloc(amount);
}

oop mem_set(oop target, fn_uint index, oop value) {
  CHECKV(is_mem(target), target, "Must be memory object.");
  CHECKV(index < get_smallint(target.mem[-1]),
         target.mem[-1], "Index out of bounds.");
  target.mem[index] = value;
  return value;
}

oop mem_get(oop target, fn_uint index) {
  CHECKV(is_mem(target), target, "Must be memory object.");
  CHECKV(index < get_smallint(target.mem[-1]),
         target.mem[-1], "Index out of bounds.");
  return target.mem[index];
}

fn_uint mem_size(oop target) {
  return get_smallint(target.mem[-1]);
}

oop mem_raw_mem_alloc(fn_uint amount) {
  return gc_raw_memory_alloc(amount);  // in gc.c
}

oop mem_raw_mem_make(void* buf, fn_uint size) {
  oop result = mem_raw_mem_alloc(size);
  memcpy(result.mem, buf, size);
  return result;
}

oop mem_raw_mem_get(oop target, fn_uint index) {
  // Type check for target done in mem_raw_mem_size.
  CHECK(0 <= index, "Need 0 <= index.");
  CHECKV(index < mem_raw_mem_size(target),
         make_smallint(mem_raw_mem_size(target)),
         "Need index < range. (showing range / sizeof(oop))");
  unsigned char* ptr = (unsigned char*) target.mem;
  return make_smallint((fn_uint) ptr[index]);
}

void mem_raw_mem_set(oop target, fn_uint index, fn_uint value) {
  // Type check for target done in mem_raw_mem_size.
  CHECK(0 <= value && value <= 255, "Need value in byte range.");
  CHECK(0 <= index, "Need 0 <= index.");
  CHECKV(index < mem_raw_mem_size(target),
         make_smallint(mem_raw_mem_size(target)),
         "Need index < range. (showing range / sizeof(oop))");
  unsigned char* ptr = (unsigned char*) target.mem;
  ptr[index] = (unsigned char) value;
}

void* mem_raw_get_ptr(oop target) {
  CHECKV(is_raw_mem(target), target, "Expected raw memory block.");
  return (void*) target.mem;
}

// NOTE: This returns the actual allocated size, so that may be
// higher than what was requested on allocation.
fn_uint mem_raw_mem_size(oop target) {
  CHECKV(is_raw_mem(target), target, "Must be raw memory object.");
  return get_smallint(target.mem[-1]);
}
