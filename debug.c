
#include "debug.h"

#include "value.h"

/* Debugging helpers. */

unsigned char extract_byte(fn_uint value, char shift) {
  unsigned char result = (value >> (8 * shift)) & 0xff;
  if (result < ' ' || result >= 127) {
    result = '.';
  }
  return result;
}

void print_zone(oop obj) {
  int i;
  for (i=-5; i<5; i++) {
    fn_uint value = obj.mem[i].smallint;
    printf("%s %016lx %c%c%c%c%c%c%c%c",
           (i == 0 ? "--> " : "    "),
           value,
           extract_byte(value, 0),
           extract_byte(value, 1),
           extract_byte(value, 2),
           extract_byte(value, 3),
           extract_byte(value, 4),
           extract_byte(value, 5),
           extract_byte(value, 6),
           extract_byte(value, 7));
    if (value & 1) {
      printf(" %lu\n", value >> 1);
    } else {
      putchar('\n');
    }
  }
}
