/* Primitives for using 64-bit registers as unsigned integers. */

#include "value.h"

extern
void intreg_add(oop a, oop b, oop* dst);

extern
void make_intreg(unsigned long long value, oop* dst);
