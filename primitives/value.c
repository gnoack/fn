
#include "value.h"
#include "cons.h"
#include "string-interning.h"

extern
value_t make_uint(uint64 i) {
  value_t a;
  a.uint = i;
  return a;
}

extern
value_t make_string(const char* str) {
  value_t a;
  a.string = intern_string(str);
  return a;
}

/* Value identity.  Returns true for equal integers and string as
 * well.  (This works because string values are always interned.)
 *
 * TODO(gnoack): May return true in rare cases if an integer value
 * happens to equal a pointer value.
 */
bool value_eq(value_t a, value_t b) {
  return TO_BOOL(a.uint == b.uint);
}

bool is_nil(value_t a) {
  // TODO(gnoack): What's the definition of nil?
  return TO_BOOL(a.uint == 0L);
}
