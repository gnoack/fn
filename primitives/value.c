
#include "value.h"
#include "cons.h"

bool value_eq(value_t a, value_t b) {
  return TO_BOOL(a.uint == b.uint);
}
