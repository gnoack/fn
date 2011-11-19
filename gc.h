#ifndef _GC_H_

#include "value.h"

extern
oop object_alloc(uint size);

extern
void init_gc();

extern
oop garbage_collect(oop root);

#define _GC_H_ 0
#endif  // _GC_H_
