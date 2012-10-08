
#include "debug.h"

#include "value.h"
#include "eval.h"
#include "cons.h"
#include "strings.h"
#include "symbols.h"

/* Debugging helpers. */

unsigned char extract_byte(fn_uint value, char shift) {
  unsigned char result = (value >> (8 * shift)) & 0xff;
  if (result < ' ' || result >= 127) {
    result = '.';
  }
  return result;
}

void print_zone(oop obj) {
  printf("\nZone around 0x%016lx\n", (fn_uint) obj.mem);
  int i;
  for (i=-10; i<10; i++) {
    fn_uint value = obj.mem[i].smallint;
    printf("%s%016lx:  %016lx %c%c%c%c%c%c%c%c",
           (i == 0 ? "--> " : "    "),
           (fn_uint) (obj.mem + i),
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
      printf("  %lu\n", value >> 1);
    } else {
      putchar('\n');
    }
  }
}


// Prints values, for debugging.
void print_value_internal(oop v) {
  if (is_global_env(v)) {
    printf("[global-env]");
  } else if (is_smallint(v)) {
    printf("%d", (unsigned int) get_smallint(v));
  } else if (is_char(v)) {
    printf("\\%c", get_char(v));
  } else if (is_symbol(v)) {
    printf("%s", v.symbol);
  } else if (is_nil(v)) {
    printf("nil");
  } else if (is_cons(v)) {
    printf("(");
    while (!is_nil(v)) {
      // is_cons(v) holds.
      if (is_cons(v)) {
	print_value_internal(first(v));
	v = rest(v);
	if (!is_nil(v)) {
	  printf(" ");
	}
      } else {
	// not a cons, print it.
	printf(" . ");
	print_value_internal(v);
	v = NIL;
      }
    }
    printf(")");
  } else if (is_primitive_mem(v)) {
    printf("<PRIMITIVE-MEMORY #%08llx>", (unsigned long long) v.smallint);
  } else if (is_procedure(v)) {
    print_procedure(v);
  } else if (is_string(v)) {
    char* c_str = c_string(v);
    printf("\"%s\"", c_str);
    free(c_str);
  } else {
    CHECK(is_mem(v), "Must be an allocated object.");
    if (value_eq(symbols._array, v)) {
      printf("@array");
    } else if (value_eq(symbols._cons, v)) {
      printf("@cons");
    } else if (value_eq(symbols._dict, v)) {
      printf("@dict");
    } else if (value_eq(symbols._frame, v)) {
      printf("@frame");
    } else if (value_eq(symbols._string, v)) {
      printf("@string");
    } else if (value_eq(symbols._procedure, v)) {
      printf("@procedure");
    } else if (value_eq(symbols._compiled_procedure, v)) {
      printf("@compiled-procedure");
    } else if (value_eq(symbols._native_procedure, v)) {
      printf("@native-procedure");
    } else {
      printf("#[OBJECT]");
    }
  }
}

extern
void print_value(oop v) {
  print_value_internal(v);
  printf("\n");
}

