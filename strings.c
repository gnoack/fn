
#include "malloc.h"

#include <string.h>

#include "carcdr.h"
#include "cons.h"
#include "memory.h"
#include "symbols.h"
#include "value.h"

#include "strings.h"

/* +---------+------+--------+--------+
 * | @string | size | offset | memptr |----> raw ASCII string
 * +---------+------+--------+--------+
 */
oop make_string(const char* str) {
  fn_uint len = strlen(str);
  oop result = mem_alloc(4);
  oop raw_string = mem_primitive_mem_alloc(len);
  memcpy((void*) raw_string.mem, (void*) str, len);
  mem_set(result, 0, symbols._string);
  mem_set(result, 1, make_smallint(len));
  mem_set(result, 2, make_smallint(0));
  mem_set(result, 3, raw_string);
  return result;
}

char* c_string(oop str) {
  CHECKV(is_string(str), str, "Must be string");
  fn_uint len = get_smallint(mem_get(str, 1));
  fn_uint offset = get_smallint(mem_get(str, 2));
  oop raw_string = mem_get(str, 3);
  char* original_raw = (char*) raw_string.mem;
  char* result = malloc(len + 1);
  CHECK(result != NULL, "Can't allocate memory.");
  memcpy((void*) result, (void*) original_raw + offset, len);
  result[len] = '\0';
  return result;
}

boolean is_string(oop str) {
  if (!is_mem(str)) {
    return NO;
  }
  return value_eq(symbols._string, mem_get(str, 0));
}
