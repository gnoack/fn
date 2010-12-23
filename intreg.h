/* Primitives for using 64-bit registers as unsigned integers. */

#include "value.h"

extern
void intreg_add(value_t a, value_t b, value_t* dst);

extern
void make_intreg(unsigned long long value, value_t* dst);
