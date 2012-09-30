
#ifndef _VALUE_H_
#define _VALUE_H_ 1

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#if __WORDSIZE == 64
  typedef uint64_t fn_uint;
#else
  typedef uint32_t fn_uint;
#endif  // __WORDSIZE == 64

typedef char boolean;
#define YES 1
#define NO 0
#define TO_BOOL(b) ((b) ? YES : NO)

#define NIL ((oop) (fn_uint) 0L)

void (*print_stack_frame)();

#define CHECK(x, msg)                                \
  if (!(x)) {                                        \
    printf("%s:%d: %s\n", __FILE__, __LINE__, msg);  \
    print_stack_frame();                             \
    exit(1);                                         \
  }

#define CHECKV(x, value, msg)                        \
  if (!(x)) {                                        \
    printf("%s:%d: %s\n", __FILE__, __LINE__, msg);  \
    printf("Offending value: ");                     \
    print_value(value);                              \
    print_stack_frame();                             \
    exit(1);                                         \
  }

// A check where only the error message is printed out.
// Useful during GC.
#define GC_CHECK(x, msg)                             \
  if (!(x)) {                                        \
    printf("%s:%d: %s\n", __FILE__, __LINE__, msg);  \
    exit(1);                                         \
  }

#define CHECKNUMBER(value) \
  CHECKV(is_smallint(value), value, "Must be a number");

typedef union value_u {
  /* A smallint if the least significant bit is set.
   * Actual integer value is then oop.smallint >> 1.
   */
  fn_uint smallint;

  /* Pointer to a series of values. */
  union value_u* mem;

  /* Pointer to a symbol. */
  const char* symbol;
} oop;

extern oop make_smallint(fn_uint i);

extern oop make_symbol(const char* str);

extern oop make_char(const char c);

extern boolean is_nil(oop a);

extern boolean is_smallint(oop v);

extern boolean is_symbol(oop v);

extern boolean is_mem(oop v);

extern boolean is_primitive_mem(oop v);

extern boolean is_char(oop v);

extern fn_uint get_smallint(oop v);

extern char get_char(oop v);

extern const char* get_symbol(oop v);

extern boolean value_eq(oop a, oop b);

#endif // _VALUE_H_
