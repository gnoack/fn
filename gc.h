#ifndef _GC_H_

#include "value.h"

extern
oop object_alloc(fn_uint size);

extern
void init_gc();

/*
 * Register a reference to be updated
 * if the referenced object has been moved.
 */
extern
void gc_register_persistent_ref(oop* place);

extern
oop garbage_collect(oop root);

#define _GC_H_ 0
#endif  // _GC_H_
