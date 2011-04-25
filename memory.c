
#include "malloc.h"

#include "value.h"
#include "memory.h"

extern oop mem_alloc(uint amount) {
  oop value;
  value.mem = malloc(sizeof(oop) * amount);
  return value;
}

extern oop mem_set(oop target, uint index, oop value) {
  target.mem[index] = value;
  return value;
}

extern oop mem_get(oop target, uint index) {
  return target.mem[index];
}
