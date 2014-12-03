#ifndef _NATIVE_COMPILER_H_
#define _NATIVE_COMPILER_H_

#include "value.h"
#include "procedures.h"

extern proc_t* compile_top_level_expression(oop expr);

#endif // _NATIVE_COMPILER_H_
