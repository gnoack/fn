
#ifndef __STRINGS_H__

#include "value.h"

/*
 * Creates a Lisp string from a plain ASCII C string.
 *
 * A string is a sequence of characters.
 */
extern
oop make_string(const char* str);

#define __STRINGS_H__ 0
#endif // __STRINGS_H__
