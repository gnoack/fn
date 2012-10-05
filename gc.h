#ifndef _GC_H_

#include "value.h"

extern oop gc_object_alloc(fn_uint size);
extern boolean gc_is_object(oop obj);

extern oop gc_primitive_memory_alloc(fn_uint size);
extern boolean gc_is_primitive_memory(oop obj);

extern void init_gc();
extern void run_gc_soon();

/*
 * Register a reference to be updated
 * if the referenced object has been moved.
 */
extern
void gc_register_persistent_ref(oop* place);

extern
oop gc_run(oop root);

#define _GC_H_ 0
#endif  // _GC_H_
