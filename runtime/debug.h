#ifndef _DEBUG_H_
#define _DEBUG_H_

#include "value.h"

extern void print_zone(oop obj);

/** For debugging. */
extern void println_value(oop v);
extern void print_value(oop v);

#endif  // _DEBUG_H_
