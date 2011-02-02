
#include "cons.h"

#include "strings.h"

// Currently, strings are implemented as lists.
oop make_string(const char* str) {
  if (*str) {
    return make_cons(make_char(*str), make_string(str+1));
  } else {
    return NIL;
  }
}
