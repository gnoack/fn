
#include "value.h"
#include "cons.h"
#include "string-interning.h"

extern
oop make_uint(uint64 i) {
  oop a;
  a.uint = i;
  return a;
}

extern
oop make_string(const char* str) {
  oop a;
  a.string = intern_string(str);
  return a;
}

/* Value identity.  Returns true for equal integers and string as
 * well.  (This works because string values are always interned.)
 *
 * TODO(gnoack): May return true in rare cases if an integer value
 * happens to equal a pointer value.
 */
bool value_eq(oop a, oop b) {
  if (is_string(a) && is_string(b)) {
    return TO_BOOL(a.string == b.string);
  } else if (is_uint(a) && is_uint(b)) {
    return TO_BOOL(a.uint == b.uint);
  } else if (is_cons(a) && is_cons(a)) {
    return TO_BOOL(a.cons == b.cons);
  } else {
    return NO;  // a and b have different types.
  }
}

extern
bool is_nil(oop a) {
  // TODO(gnoack): What's the definition of nil?
  return TO_BOOL(a.uint == 0L);
}

extern
bool is_uint(oop v) {
  return !is_string(v) && !is_cons(v);
}

extern
bool is_string(oop v) {
  return is_interned(v.string);
}

extern
bool is_cons(oop v) {
  // TODO(gnoack): Oooh, this is major guesswork... :)
  return v.uint > 0xffffff;
}
