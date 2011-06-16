
#ifndef _VALUE_H_
#define _VALUE_H_ 1

#include <stdio.h>
#include <stdlib.h>

typedef unsigned int uint;

typedef char bool;
#define YES 1
#define NO 0
#define TO_BOOL(b) ((b) ? YES : NO)

#define NIL ((oop) (uint) 0L)

#define CHECK(x, msg)                                \
  if (!(x)) {                                        \
    printf("%s:%d: %s\n", __FILE__, __LINE__, msg);  \
    exit(1);                                         \
  }

#define CHECKV(x, value, msg)                        \
  if (!(x)) {                                        \
    printf("%s:%d: %s\n", __FILE__, __LINE__, msg);  \
    printf("Offending value: ");                     \
    print_value(value);                              \
    exit(1);                                         \
  }

#define CHECKNUMBER(value) \
  CHECKV(is_smallint(value), value, "Must be a number");

typedef union value_u {
  /* A smallint if the least significant bit is set.
   * Actual integer value is then oop.smallint >> 1.
   */
  uint smallint;

  /* Pointer to a series of values. */
  union value_u* mem;

  /* Pointer to a symbol. */
  const char* symbol;
} oop;

extern
oop make_smallint(uint i);

extern
oop make_symbol(const char* str);

extern
oop make_char(const char c);

extern
bool is_nil(oop a);

extern
bool is_smallint(oop v);

extern
bool is_symbol(oop v);

extern
bool is_mem(oop v);

extern
bool is_char(oop v);

extern
uint get_smallint(oop v);

extern
char get_char(oop v);

extern
const char* get_symbol(oop v);

extern
bool value_eq(oop a, oop b);

/** For debugging. */
extern
void print_value(oop v);

#endif // _VALUE_H_
