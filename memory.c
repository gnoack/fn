
#include "malloc.h"

#include "value.h"
#include "memory.h"
#include "gc.h"

oop mem_alloc(fn_uint amount) {
  CHECK(amount > 0, "Allocated memory regions need to be larger than 0.");
  return gc_object_alloc(amount);
}

oop mem_set(oop target, fn_uint index, oop value) {
  target.mem[index] = value;
  return value;
}

oop mem_get(oop target, fn_uint index) {
  return target.mem[index];
}

oop mem_primitive_mem_alloc(fn_uint amount) {
  return gc_primitive_memory_alloc(amount);  // in gc.c
}

oop mem_primitive_mem_get(oop target, fn_uint index) {
  unsigned char* ptr = (unsigned char*) target.mem;
  return make_smallint((fn_uint) ptr[index]);
}

void mem_primitive_mem_set(oop target, fn_uint index, fn_uint value) {
  unsigned char* ptr = (unsigned char*) target.mem;
  CHECK(0 <= value && value <= 255, "Must be in byte range.");
  ptr[index] = (unsigned char) value;
}
