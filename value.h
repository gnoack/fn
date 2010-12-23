
#ifndef _VALUE_H_
#define _VALUE_H_ 1

typedef unsigned int uint;

typedef char bool;
#define YES 1
#define NO 0
#define TO_BOOL(b) ((b) ? YES : NO)

#define NIL ((oop) (uint) 0L)

#define CHECK(x, msg)                              \
  if (!(x)) {                                      \
    printf("%s:%s: %s", __FILE__, __LINE__, msg);  \
  }

struct cons_s;

typedef union value_u {
  /* A smallint if the least significant bit is set.
   * Actual integer value is then oop.smallint >> 1.
   */
  uint smallint;

  /* Pointer to a data structure. */
  struct cons_s* cons;

  /* Pointer to a symbol. */
  const char* string;
} oop;

extern
oop make_smallint(uint i);

extern
oop make_string(const char* str);

extern
bool is_nil(oop a);

extern
bool is_smallint(oop v);

extern
bool is_string(oop v);

extern
bool is_cons(oop v);

extern
bool value_eq(oop a, oop b);

/** For debugging. */
extern
void print_value(oop v);

#endif // _VALUE_H_
