#ifndef _GC_H_
#define _GC_H_

#include "value.h"

// Number of nested functions which inhibit the GC from running.
extern unsigned int gc_protect_counter;

// Allocate objects in "object memory" and test for that property.
oop gc_object_alloc(fn_uint size);
bool gc_is_object(oop obj);

// Allocate objects in "raw memory" and test for that property.
oop gc_raw_memory_alloc(fn_uint byte_size);
bool gc_is_raw_memory(oop obj);

// See gc_register_persistent_refs below.
typedef void (*enumerator_t)(void (*accept)(oop* ref));

// Register "persistent" references.
//
// These are oop pointers in C code which need to be updated
// when the underlying object got moved by the GC.
//
// Note that the 'enumerator' function gets called after each GC,
// so that the underlying set of pointers can change every time.
void gc_register_persistent_refs(enumerator_t enumerator);

// Run the GC if it makes sense.
//
// Prerequisites are:
//  - the GC is not inhibited (see gc_protect_counter above)
//  - object memory is full enough *or* GC was marked to run "soon"
//
// This function is designed to be called at various "safe" points,
// when there are no heap-invisible oop references floating around
// in C code.
void gc_run();

// Mark the GC to run on the next occasion when gc_run() is called,
// even when the memory is not full enough yet.
void run_gc_soon();

// Serializing and deserializing.
void gc_serialize_to_file(char* filename);
void gc_deserialize_from_file(char* filename);

// Initialize this module.
void init_gc();

#endif  // _GC_H_
