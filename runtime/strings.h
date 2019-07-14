#ifndef _STRINGS_H_
#define _STRINGS_H_

#include "value.h"

// Creates a Lisp string from a plain ASCII C string.
oop make_string(const char* str);

// Same, but based on memory block and size.
oop make_string_from_mem_block(oop raw_string, fn_uint size);

// Converts a Lisp string into a plain ASCII C string.
// The caller takes ownership of the returned string.
char* c_string(oop str);

bool is_string(oop str);

#endif // _STRINGS_H_
