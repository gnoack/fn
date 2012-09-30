
#include <stdio.h>

#include "gc.h"
#include "eval.h"
#include "value.h"
#include "cons.h"
#include "memory.h"
#include "procedures.h"
#include "string-interning.h"

// Characters are mapped to char_start in memory;
// the nth ASCII character corresponds to the value
// char_start + (n << 2).
fn_uint char_start = 0xcccc0000;

oop make_smallint(fn_uint i) {
  oop a;
  a.smallint = (i << 1) | 1L;
  CHECK(a.smallint >> 1 == i, "Integer doesn't fit into smallint.");
  CHECKV(is_smallint(a), a, "Couldn't produce a smallint.");
  return a;
}

oop make_symbol(const char* str) {
  oop a;
  a.symbol = intern_string(str);
  CHECK(is_symbol(a), "Couldn't construct string.");
  return a;
}

oop make_char(const char c) {
  oop a;
  a.smallint = char_start + ((fn_uint)c << 2);
  return a;
}

/* Value identity.  Returns true for equal integers and symbols as
 * well.  (This works because symbols are always interned.)
 */
boolean value_eq(oop a, oop b) {
  // TODO(gnoack): Consider TO_BOOL(a.smallint == a.smallint),
  //   we don't need these types.
  if (is_symbol(a) && is_symbol(b)) {
    return TO_BOOL(a.symbol == b.symbol);
  } else if (is_smallint(a) && is_smallint(b)) {
    return TO_BOOL(a.smallint == b.smallint);
  } else if (is_mem(a) && is_mem(a)) {
    return TO_BOOL(a.mem == b.mem);
  } else if (is_char(a) && is_char(b)) {
    return TO_BOOL(a.mem == b.mem);
  } else if (is_nil(a) && is_nil(b)) {
    return YES;
  } else if (is_primitive_mem(a) && is_primitive_mem(b)) {
    return YES;
  } else {
    return NO;  // a and b have different types.
  }
}

// TODO(gnoack): Reduce the is_* to a function that returns the
//   primitive type from the value.  This would ensure a value
//   can have only one type.
boolean is_nil(oop a) {
  return TO_BOOL(a.smallint == 0L);
}

boolean is_smallint(oop v) {
  return TO_BOOL(((v.smallint) & 1) != 0);
}

boolean is_symbol(oop v) {
  return is_interned(v.symbol);
}

boolean is_mem(oop v) {
  return gc_is_object(v);
}

boolean is_primitive_mem(oop v) {
  return gc_is_primitive_memory(v);
}

boolean is_char(oop v) {
  return TO_BOOL(char_start <= v.smallint &&
		 v.smallint < char_start + (256 << 2));
}

fn_uint get_smallint(oop v) {
  CHECKNUMBER(v);
  return v.smallint >> 1;
}

const char* get_symbol(oop v) {
  CHECKV(is_symbol(v), v, "Must be a symbol");
  return v.symbol;
}

char get_char(oop v) {
  CHECKV(is_char(v), v, "Must be a character");
  return (char) ((v.smallint - char_start) >> 2);
}
