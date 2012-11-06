#ifndef _DEBUG_H_

#include "value.h"

extern void print_zone(oop obj);

/** For debugging. */
// TODO: Rename to println_value() and print_value().
extern void print_value(oop v);
extern void print_value_internal(oop v);

#define _DEBUG_H_ 0
#endif  // _DEBUG_H_
