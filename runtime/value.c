#include <stdio.h>

#include "cons.h"
#include "debug.h"
#include "eval.h"
#include "gc.h"
#include "memory.h"
#include "procedures.h"
#include "symbols.h"
#include "value.h"

// Characters are mapped to char_start in memory;
// the nth ASCII character corresponds to the value
// char_start + (n << 2).
fn_uint char_start = 0xcccc0000;

oop make_smallint(fn_uint i) {
  oop a;
  a.smallint = (i << 1) | 1;
  CHECK(a.smallint >> 1 == i, "Integer doesn't fit into smallint.");
  CHECKV(is_smallint(a), a, "Couldn't produce a smallint.");
  return a;
}

oop make_symbol(const char* str) {
  return make_or_lookup_symbol(str);
}

oop make_char(const unsigned char c) {
  oop a;
  a.smallint = char_start + ((fn_uint)c << 2);
  return a;
}

/* Value identity.  Returns true for equal integers and symbols as
 * well.  (This works because symbols are always interned.)
 */
boolean value_eq(oop a, oop b) {
  return TO_BOOL(a.mem == b.mem);
}

// TODO: Reduce the is_* to a function that returns the
// primitive type from the value.  This would ensure a value
// can have only one type.
boolean is_nil(oop a) {
  return TO_BOOL(a.smallint == 0L);
}

boolean is_smallint(oop v) {
  return TO_BOOL(((v.smallint) & 1) != 0);
}

boolean is_symbol(oop v) {
  if (is_mem(v)) {
    return value_eq(MEM_GET(v, 0), symbols._symbol);
  }
  return NO;
}

boolean is_mem(oop v) {
  return gc_is_object(v);
}

boolean is_raw_mem(oop v) {
  return gc_is_raw_memory(v);
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
  return (char*) mem_get(v, 1).mem;
}

unsigned char get_char(oop v) {
  CHECKV(is_char(v), v, "Must be a character");
  return (unsigned char) ((v.smallint - char_start) >> 2);
}
