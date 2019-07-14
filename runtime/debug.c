
#include "debug.h"

#include "value.h"
#include "data.h"
#include "cons.h"
#include "eval.h"
#include "interpreter.h"
#include "memory.h"
#include "strings.h"
#include "symbols.h"
#include "vars.h"

/* Debugging helpers. */

static unsigned char extract_byte(fn_uint value, char shift) {
  unsigned char result = (value >> (8 * shift)) & 0xff;
  if (result < ' ' || result >= 127) {
    result = '.';
  }
  return result;
}

void print_zone(oop obj) {
  printf("\nZone around 0x%p\n", obj.mem);
  int i;
  for (i=-10; i<10; i++) {
    fn_uint value = obj.mem[i].smallint;
    printf("%s%p:  %p %c%c%c%c%c%c%c%c",
           (i == 0 ? "--> " : "    "),
           obj.mem + i,
           obj.mem[i].mem,
           extract_byte(value, 0),
           extract_byte(value, 1),
           extract_byte(value, 2),
           extract_byte(value, 3),
           extract_byte(value, 4),
           extract_byte(value, 5),
           extract_byte(value, 6),
           extract_byte(value, 7));
    if (value & 1) {
      printf("  %u\n", value >> 1);
    } else {
      putchar('\n');
    }
  }
}


// Prints values, for debugging.
void print_value(oop v) {
  if (is_global_env(v)) {
    printf("#<GLOBAL ENVIRONMENT>");
  } else if (is_smallint(v)) {
    printf("%u", (fn_uint) get_smallint(v));
  } else if (is_char(v)) {
    char c = get_char(v);
    switch (c) {
    case '\n': printf("#\\Newline"); break;
    case '\t': printf("#\\Tab"); break;
    case ' ': printf("#\\Space"); break;
    default: printf("#\\%c", get_char(v)); break;
    }
  } else if (value_eq(symbols._true, v)) {
    printf("true");
  } else if (value_eq(symbols._false, v)) {
    printf("false");
  } else if (is_symbol(v)) {
    printf("%s", get_symbol(to_symbol(v)));
  } else if (is_nil(v)) {
    printf("nil");
  } else if (is_cons(v)) {
    printf("(");
    while (!is_nil(v)) {
      // is_cons(v) holds.
      if (is_cons(v)) {
	print_value(first(v));
	v = rest(v);
	if (!is_nil(v)) {
	  printf(" ");
	}
      } else {
	// not a cons, print it.
	printf(" . ");
	print_value(v);
	v = NIL;
      }
    }
    printf(")");
  } else if (is_raw_mem(v)) {
    printf("<RAW-MEMORY #%p ", v.mem);
    fn_uint size = mem_raw_mem_size(v);
    fn_uint i;
    for (i=0; i<size; i++) {
      printf(" %02x", ((unsigned char*) v.mem)[i]);
    }
    printf(">");
  } else if (is_procedure(v)) {
    print_procedure(v);
  } else if (is_string(v)) {
    char* c_str = c_string(v);
    printf("\"%s\"", c_str);
    free(c_str);
  } else if (is_array(v)) {
    fn_uint i;
    fn_uint size = array_size(v);
    printf("#[");
    for (i=0; i<size; i++) {
      putchar(' ');
      print_value(array_get(v, i));
    }
    putchar(']');
  } else if (is_dict(v)) {
    oop kv_pairs = dict_key_value_pairs(v);
    printf("#{");
    while (is_cons(kv_pairs)) {
      print_value(first(first(kv_pairs)));
      printf(": ");
      print_value(rest(first(kv_pairs)));
      if (!is_nil(rest(kv_pairs))) {
        printf(", ");
      }
      kv_pairs = rest(kv_pairs);
    }
    putchar('}');
  } else if (is_frame(v)) {
    print_frame(to_frame(v));
  } else if (is_var(v)) {
    print_var(v);
  } else {
    CHECK(is_mem(v), "Must be an allocated object.");
    if (value_eq(symbols._array, v)) {
      printf("Array");
    } else if (value_eq(symbols._cons, v)) {
      printf("Cons");
    } else if (value_eq(symbols._dict, v)) {
      printf("Dict");
    } else if (value_eq(symbols._frame, v)) {
      printf("Frame");
    } else if (value_eq(symbols._stack, v)) {
      printf("Stack");
    } else if (value_eq(symbols._string, v)) {
      printf("String");
    } else if (value_eq(symbols._symbol, v)) {
      printf("Symbol");
    } else if (value_eq(symbols._defined_var, v)) {
      printf("DefinedVar");
    } else if (value_eq(symbols._undefined_var, v)) {
      printf("UndefinedVar");
    } else if (value_eq(symbols._compiled_procedure, v)) {
      printf("CompiledProcedure");
    } else if (value_eq(symbols._native_procedure, v)) {
      printf("NativeProcedure");
    } else {
      printf("#<OBJECT>");
    }
  }
}

void println_value(oop v) {
  print_value(v);
  printf("\n");
}
