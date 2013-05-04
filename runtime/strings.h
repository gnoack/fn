#ifndef _STRINGS_H_
#define _STRINGS_H_

#include "value.h"

/*
 * Creates a Lisp string from a plain ASCII C string.
 */
extern oop make_string(const char* str);

/*
 * Converts a Lisp string into a plain ASCII C string.
 * The caller takes ownership of the returned string.
 */
extern char* c_string(oop str);

extern boolean is_string(oop str);

#endif // _STRINGS_H_
