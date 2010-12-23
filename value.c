
#include <stdio.h>

#include "value.h"
#include "cons.h"
#include "string-interning.h"

extern
oop make_uint(uint i) {
  oop a;
  a.smallint = (i << 1) | 1L;
  if (!is_uint(a) || is_nil(a) || is_cons(a) || is_string(a)) {
    printf("An int is an int is an int...\n");
    return NIL;
  }
  return a;
}

extern
oop make_string(const char* str) {
  oop a;
  a.string = intern_string(str);
  if (is_uint(a) || is_nil(a) || is_cons(a) || !is_string(a)) {
    printf("A string is a string is a string...\n");
    return NIL;
  }
  return a;
}

/* Value identity.  Returns true for equal integers and string as
 * well.  (This works because string values are always interned.)
 */
bool value_eq(oop a, oop b) {
  if (is_string(a) && is_string(b)) {
    return TO_BOOL(a.string == b.string);
  } else if (is_uint(a) && is_uint(b)) {
    return TO_BOOL(a.smallint == b.smallint);
  } else if (is_cons(a) && is_cons(a)) {
    return TO_BOOL(a.cons == b.cons);
  } else {
    return NO;  // a and b have different types.
  }
}

extern
bool is_nil(oop a) {
  return TO_BOOL(a.smallint == 0L);
}

extern
bool is_uint(oop v) {
  return TO_BOOL(((v.smallint) & 1) != 0);
}

extern
bool is_string(oop v) {
  return is_interned(v.string);
}

extern
bool is_cons(oop v) {
  return !is_uint(v) && !is_string(v) && !is_nil(v);
}


// Prints values, for debugging.
void print_value_internal(oop v) {
  if (is_uint(v)) {
    printf("%d", (v.smallint >> 1));
  } else if (is_string(v)) {
    printf("%s", v.string);
  } else if (is_nil(v)) {
    printf("nil");
  } else {
    CHECK(is_cons(v), "Can only be cons");
    printf("(");
    while (!is_nil(v)) {
      // is_cons(v) holds.
      if (is_cons(v)) {
	print_value_internal(first(v));
	v = rest(v);
	if (!is_nil(v)) {
	  printf(" ");
	}
      } else {
	// not a cons, print it.
	printf(" . ");
	print_value_internal(v);
	v = NIL;
      }
    }
    printf(")");
  }
}

extern
void print_value(oop v) {
  print_value_internal(v);
  printf("\n");
}

