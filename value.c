
#include <stdio.h>

#include "value.h"
#include "cons.h"
#include "string-interning.h"

oop make_smallint(uint i) {
  // TODO: Upper bounds check!
  oop a;
  a.smallint = (i << 1) | 1L;
  CHECK(a.smallint >> 1 == i, "Integer doesn't fit into smallint.");
  CHECK(is_smallint(a), "Couldn't produce a smallint.");
  return a;
}

oop make_symbol(const char* str) {
  oop a;
  a.symbol = intern_string(str);
  CHECK(is_symbol(a), "Couldn't construct string.");
  return a;
}

/* Value identity.  Returns true for equal integers and string as
 * well.  (This works because string values are always interned.)
 */
bool value_eq(oop a, oop b) {
  if (is_symbol(a) && is_symbol(b)) {
    return TO_BOOL(a.symbol == b.symbol);
  } else if (is_smallint(a) && is_smallint(b)) {
    return TO_BOOL(a.smallint == b.smallint);
  } else if (is_cons(a) && is_cons(a)) {
    return TO_BOOL(a.cons == b.cons);
  } else {
    return NO;  // a and b have different types.
  }
}

bool is_nil(oop a) {
  return TO_BOOL(a.smallint == 0L);
}

bool is_smallint(oop v) {
  return TO_BOOL(((v.smallint) & 1) != 0);
}

bool is_symbol(oop v) {
  return is_interned(v.symbol);
}

bool is_cons(oop v) {
  return !is_smallint(v) && !is_symbol(v) && !is_nil(v);
}

uint get_smallint(oop v) {
  CHECK(is_smallint(v), "Must be a smallint.");
  return v.smallint >> 1;
}

// Prints values, for debugging.
void print_value_internal(oop v) {
  if (is_smallint(v)) {
    printf("%d", get_smallint(v));
  } else if (is_symbol(v)) {
    printf("%s", v.symbol);
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

