
#include "parser.h"

 // TODO: Rm dependency when real parsing is written.
#include "carcdr.h"
#include "strings.h"

bool is_digit(oop character) {
  return TO_BOOL(isdigit(get_char(character)));
}

// Converts a digit representation of an integer to a smallint.
oop parse_integer(oop input) {
  return make_smallint(123);
}

extern
oop parse_sexpr(oop input) {
  if (is_digit(car(input))) {
    return parse_integer(input);
  } else {
    return make_symbol(c_string(input));
  }
}
