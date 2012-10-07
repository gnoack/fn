
#ifndef __STRINGS_H__

#include "value.h"

/*
 * Creates a Lisp string from a plain ASCII C string.
 */
extern oop make_string(const char* str);

extern char* c_string(oop str);

extern boolean is_string(oop str);

#define __STRINGS_H__ 0
#endif // __STRINGS_H__
