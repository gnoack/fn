
#include "parser.h"

 // TODO: Rm dependency when real parsing is written.
#include "strings.h"

extern
oop parse_sexpr(oop string) {
  // TODO: Real parsing... :)
  return make_symbol(c_string(string));
}
