
#include "malloc.h"

#include "value.h"
#include "memory.h"
#include "gc.h"

extern oop mem_alloc(uint amount) {
  CHECK(amount > 0, "Allocated memory regions need to be larger than 0.");
  return object_alloc(amount);
}

extern oop mem_set(oop target, uint index, oop value) {
  target.mem[index] = value;
  return value;
}

extern oop mem_get(oop target, uint index) {
  return target.mem[index];
}
