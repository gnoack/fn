
#include "intreg.h"

void make_intreg(unsigned long long value, oop* dst) {
  dst->uint = value;
}

// TODO(gnoack): Overflow handling.
void intreg_add(oop a, oop b, oop* dst) {
  make_intreg(a.uint + b.uint, dst);
}
