
#ifndef _VALUE_H_
#define _VALUE_H_ 1

typedef unsigned long long uint64;

typedef char bool;
#define YES 1
#define NO 0
#define TO_BOOL(b) ((b) ? YES : NO)

#define NIL ((value_t) (uint64) 0L)

struct cons_s;

typedef union value_u {
  uint64 uint;
  struct cons_s* cons;
} value_t;

extern
value_t make_uint(uint64 i);

extern
bool value_eq(value_t a, value_t b);

#endif // _VALUE_H_
