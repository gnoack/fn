
#include "malloc.h"

#include "value.h"
#include "memory.h"
#include "gc.h"

oop mem_alloc(fn_uint amount) {
  CHECK(amount > 0, "Allocated memory regions need to be larger than 0.");
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

oop mem_primitive_mem_alloc(fn_uint amount) {
  return gc_primitive_memory_alloc(amount);  // in gc.c
}

oop mem_primitive_mem_get(oop target, fn_uint index) {
  CHECKV(is_primitive_mem(target), target, "Must be primitive memory object.");
  CHECK(0 <= index, "Need 0 <= index.");
  CHECKV(index < sizeof(oop) * (get_smallint(target.mem[-1])),
         target.mem[-1],
         "Need index < range. (showing range / sizeof(oop))");
  unsigned char* ptr = (unsigned char*) target.mem;
  return make_smallint((fn_uint) ptr[index]);
}

void mem_primitive_mem_set(oop target, fn_uint index, fn_uint value) {
  CHECKV(is_primitive_mem(target), target, "Must be primitive memory object.");
  CHECK(0 <= value && value <= 255, "Need value in byte range.");
  CHECK(0 <= index, "Need 0 <= index.");
  CHECKV(index < sizeof(oop) * (get_smallint(target.mem[-1])),
         target.mem[-1],
         "Need index < range. (showing range / sizeof(oop))");
  unsigned char* ptr = (unsigned char*) target.mem;
  ptr[index] = (unsigned char) value;
}
