
#include "malloc.h"

#include "cons.h"
#include "value.h"
#include "carcdr.h"

#include "strings.h"

// Currently, strings are implemented as lists.
oop make_string(const char* str) {
  if (*str) {
    return make_cons(make_char(*str), make_string(str+1));
  } else {
    return NIL;
  }
}

const char* c_string(oop str) {
  unsigned len = length_int(str);
  char* buf = malloc(len + 1);
  char* ptr = buf;
  while (is_cons(str)) {
    *ptr = get_char(car(str));
    ptr++;
    str = cdr(str);
  }
  *ptr = '\0';
  return buf;
}
