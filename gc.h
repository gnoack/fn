#ifndef _GC_H_

#include "value.h"

extern oop gc_object_alloc(fn_uint size);
extern boolean gc_is_object(oop obj);

extern oop gc_primitive_memory_alloc(fn_uint size);
extern boolean gc_is_primitive_memory(oop obj);

extern void init_gc();
extern void run_gc_soon();

/*
 * For registering references to be updated
 * if the referenced object has been moved.
 */
typedef void (*enumerator_t)(void (*accept)(oop* ref));

extern
void gc_register_persistent_refs(enumerator_t enumerator);

extern
void gc_run();


#define _GC_H_ 0
#endif  // _GC_H_
