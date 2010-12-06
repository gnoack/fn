
#include "value.h"
#include "cons.h"

extern
value_t make_uint(uint64 i) {
  value_t a;
  a.uint = i;
  return a;
}

bool value_eq(value_t a, value_t b) {
  return TO_BOOL(a.uint == b.uint);
}

bool is_nil(value_t a) {
  // TODO(gnoack): What's the definition of nil?
  return TO_BOOL(a.uint == 0L);
}
