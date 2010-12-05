
#ifndef _VALUE_H_

typedef unsigned long long uint64;

typedef char bool;
#define YES 1
#define NO 0
#define TO_BOOL(b) ((b) ? YES : NO)

union value_u {
  uint64 uint;
};

typedef union value_u value_t;

extern
bool value_eq(value_t a, value_t b);

#define _VALUE_H_
#endif // _VALUE_H_
