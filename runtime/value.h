#ifndef _VALUE_H_
#define _VALUE_H_

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#if __WORDSIZE == 64
  typedef uint64_t fn_uint;
#else
  typedef uint32_t fn_uint;
#endif  // __WORDSIZE == 64

typedef int boolean;
#define NO 0
#define YES 1
#define TO_BOOL(b) ((b) ? YES : NO)

#define NIL ((oop) (fn_uint) NULL)

extern void (*print_stack_trace)();

// GCC only.
#define unlikely(x) __builtin_expect(x, 0)
#define likely(x) __builtin_expect(x, 1)


#define CHECK(x, msg)                                \
  if (unlikely(!(x))) {                              \
    printf("%s:%d: %s\n", __FILE__, __LINE__, msg);  \
    print_stack_trace();                             \
    exit(1);                                         \
  }

#define CHECKV(x, value, msg)                        \
  if (unlikely(!(x))) {                              \
    printf("%s:%d: %s\n", __FILE__, __LINE__, msg);  \
    printf("Offending value: ");                     \
    println_value(value);                            \
    print_stack_trace();                             \
    exit(1);                                         \
  }

// A check where only the error message is printed out.
// Useful during GC.
#define GC_CHECK(x, msg)                             \
  if (unlikely(!(x))) {                              \
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
} oop;

static inline oop to_oop(void* obj) { return *((oop*) &obj); }

// Forward type definitions.
typedef struct procedure proc_t;
typedef struct frame frame_t;
typedef struct symbol symbol_t;

#define CONVERTERS(type, convertername)					\
  static inline type* to_ ## convertername(oop obj) { return (type*) obj.mem; } \
  static inline oop convertername ## _to_oop(type* obj) { return to_oop(obj); } \

CONVERTERS(frame_t, frame);
CONVERTERS(proc_t, proc);
CONVERTERS(symbol_t, symbol);

extern oop make_smallint(fn_uint i);

extern symbol_t* make_symbol(const char* str);

extern oop make_char(const unsigned char c);

extern boolean is_nil(oop a);

extern boolean is_smallint(oop v);

extern boolean is_symbol(oop v);

extern boolean is_mem(oop v);

extern boolean is_raw_mem(oop v);

extern boolean is_char(oop v);

extern fn_uint get_smallint(oop v);

extern unsigned char get_char(oop v);

// Returned object is immutable, forever, and owned by the symbol.
extern const char* get_symbol(symbol_t* v);

extern boolean value_eq(oop a, oop b);

#endif // _VALUE_H_
