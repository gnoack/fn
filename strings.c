
#include "malloc.h"

#include <string.h>

#include "carcdr.h"
#include "cons.h"
#include "memory.h"
#include "symbols.h"
#include "value.h"

#include "strings.h"

/* +---------+------+--------+
 * | @string | size | memptr |----> raw ASCII string
 * +---------+------+--------+
 */
oop make_string(const char* str) {
  fn_uint len = strlen(str);
  oop result = mem_alloc(3);
  oop raw_string = mem_primitive_mem_alloc(len);
  memcpy((void*) raw_string.mem, (void*) str, len);
  mem_set(result, 0, symbols._string);
  mem_set(result, 1, make_smallint(len));
  mem_set(result, 2, raw_string);
  return result;
}

char* c_string(oop str) {
  CHECKV(is_string(str), str, "Must be string");
  fn_uint len = get_smallint(mem_get(str, 1));
  char* result = malloc(len + 1);
  oop raw_string = mem_get(str, 2);
  memcpy((void*) result, (void*) raw_string.mem, len);
  result[len] = '\0';
  return result;
}

boolean is_string(oop str) {
  return is_mem(str) && value_eq(symbols._string, mem_get(str, 0));
}
