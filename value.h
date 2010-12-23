
#ifndef _VALUE_H_
#define _VALUE_H_ 1

typedef unsigned long long uint64;

typedef char bool;
#define YES 1
#define NO 0
#define TO_BOOL(b) ((b) ? YES : NO)

#define NIL ((oop) (uint64) 0L)

#define CHECK(x, msg)                              \
  if (!(x)) {                                      \
    printf("%s:%s: %s", __FILE__, __LINE__, msg);  \
  }

struct cons_s;

typedef union value_u {
  uint64 uint;
  struct cons_s* cons;
  const char* string;
} oop;

extern
oop make_uint(uint64 i);

extern
oop make_string(const char* str);

extern
bool is_nil(oop a);

extern
bool is_uint(oop v);

extern
bool is_string(oop v);

extern
bool is_cons(oop v);

extern
bool value_eq(oop a, oop b);

#endif // _VALUE_H_
