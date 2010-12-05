
#include "intreg.h"

void make_intreg(unsigned long long value, value_t* dst) {
  dst->uint = value;
}

// TODO(gnoack): Overflow handling.
void intreg_add(value_t a, value_t b, value_t* dst) {
  make_intreg(a.uint + b.uint, dst);
}
