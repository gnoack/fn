
#include <malloc.h>
#include <string.h>

#include "carcdr.h"
#include "cons.h"
#include "debug.h"
#include "memory.h"
#include "symbols.h"
#include "value.h"

#include "strings.h"

/* +--------+------+--------+--------+
 * | String | size | offset | memptr |----> raw ASCII string
 * +--------+------+--------+--------+
 */
oop make_string_from_mem_block(oop raw_string, fn_uint size) {
  oop result = mem_alloc(4);
  MEM_SET(result, 0, symbols._string);
  MEM_SET(result, 1, make_smallint(size));
  MEM_SET(result, 2, make_smallint(0));
  MEM_SET(result, 3, raw_string);
  return result;
}

oop make_string(const char* str) {
  fn_uint len = strlen(str);
  oop raw_string = mem_raw_mem_alloc(len);
  memcpy((void*) raw_string.mem, (void*) str, len);
  return make_string_from_mem_block(raw_string, len);
}

char* c_string(oop str) {
  CHECKV(is_string(str), str, "Must be string");
  fn_uint len = get_smallint(MEM_GET(str, 1));
  fn_uint offset = get_smallint(MEM_GET(str, 2));
  oop raw_string = MEM_GET(str, 3);
  char* original_raw = (char*) raw_string.mem;
  char* result = malloc(len + 1);
  CHECK(result != NULL, "Can't allocate memory.");
  memcpy((void*) result, (void*) original_raw + offset, len);
  result[len] = '\0';
  return result;
}

boolean is_string(oop str) {
  return TO_BOOL(is_mem(str) && value_eq(symbols._string, MEM_GET(str, 0)));
}
