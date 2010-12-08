
#ifndef _VALUE_H_
#define _VALUE_H_ 1

typedef unsigned long long uint64;

typedef char bool;
#define YES 1
#define NO 0
#define TO_BOOL(b) ((b) ? YES : NO)

#define NIL ((value_t) (uint64) 0L)

#define CHECK(x, msg)                              \
  if (!(x)) {                                      \
    printf("%s:%s: %s", __FILE__, __LINE__, msg);  \
  }

struct cons_s;

typedef union value_u {
  uint64 uint;
  struct cons_s* cons;
  const char* string;
} value_t;

extern
value_t make_uint(uint64 i);

extern
value_t make_string(const char* str);

extern
bool is_nil(value_t a);

extern
bool is_uint(value_t v);

extern
bool is_string(value_t v);

extern
bool is_cons(value_t v);

extern
bool value_eq(value_t a, value_t b);

#endif // _VALUE_H_
