
#include <string.h>

#include "value.h"
#include "gc.h"
#include "symbols.h"

typedef struct {
  void (*on_gc_start)();
  void (*on_gc_stop)();
  void (*save)(oop obj);
  boolean (*is_saved)(oop obj);
  oop (*update)(oop obj);
  void (*update_all_refs)();
  void (*enumerate_refs)(oop obj, void (*callback)(oop ref));
} region_t;

// Forward declaration.
region_t* region(oop obj);

// ------------------------------------------------------------------

void save_noop(oop obj) { /* Not needed. */ }
boolean is_saved_always_true(oop obj) { return YES; }
oop identity(oop obj) { return obj; }
void noop() { /* No-op. */ }
void enumerate_refs_none(oop obj, void (*callback)(oop ref)) { /* None */ }

region_t primitive_region;

void primitive_region_init() {
  primitive_region.on_gc_start = noop;
  primitive_region.on_gc_stop = noop;
  primitive_region.save = save_noop;
  primitive_region.is_saved = is_saved_always_true;
  primitive_region.update = identity;
  primitive_region.update_all_refs = noop;
  primitive_region.enumerate_refs = enumerate_refs_none;
}

// ------------------------------------------------------------------

typedef struct {
  oop* free;
  oop* start;
  oop* end;
  uint size;
} half_space;

void half_space_init(half_space* space, uint size) {
  space->start = (oop*) calloc(sizeof(oop), size);
  space->end = space->start + size;
  space->free = space->start;
  space->size = size;
}

void half_space_clear(half_space* space) {
  space->free = space->start;
  bzero(space->start, sizeof(oop) * space->size);
}

// Helper to swap pointers into oop regions.
void swap(oop** a, oop** b) {
  oop* tmp = *a;
  *a = *b;
  *b = tmp;
}

// Swaps two half-spaces.
void half_space_swap(half_space* a, half_space* b) {
  half_space tmp;
  memcpy(&tmp, a, sizeof(half_space));
  memcpy(a, b, sizeof(half_space));
  memcpy(b, &tmp, sizeof(half_space));
}

oop half_space_alloc(half_space* space, uint size) {
  oop result;
  result.mem = space->free;
  space->free += size;
  if (space->free >= space->end) {
    printf("Too little space in new half-space.");
    exit(1);
  }
  return result;
}

boolean half_space_contains(half_space* space, oop obj) {
  return TO_BOOL(space->start <= obj.mem && obj.mem <= space->end);
}

// Only for debugging.
void half_space_print_fill(const half_space* space, const char* name) {
  uint fill = space->free - space->start;
  uint size = space->size;
  printf("GC: %6s: Object fill: %d of %d (%d %%)\n",
	 name, fill, size, 100*fill / size);
}


// ---------------------------------------------------------

struct {
  half_space current;
  half_space old;
} object_memory;

region_t object_region;


void object_on_gc_start() {
  half_space_swap(&object_memory.current, &object_memory.old);
  half_space_clear(&object_memory.current);
}

// Allocate in new space.
oop object_alloc(uint size) {
  oop result = half_space_alloc(&object_memory.current, size + 1);
  result.mem[0] = make_smallint(size);
  result.mem = result.mem + 1;
  return result;
}

// True if obj references a broken heart.
boolean object_is_saved(oop obj) {
  return is_nil(obj.mem[-1]);
}

void object_save(oop obj) {
  CHECKV(!object_is_saved(obj), obj, "Must be unsaved.");
  CHECKV(half_space_contains(&object_memory.old, obj),
	 obj, "Object must be in old half-space to be saved.");
  // Move.
  uint size = get_smallint(obj.mem[-1]);
  oop newobj = object_alloc(size);
  uint i;
  for (i = 0; i < size; i++) {
    newobj.mem[i] = obj.mem[i];
  }

  // Mark as broken heart.
  obj.mem[-1] = NIL;
  obj.mem[0] = newobj;
}

oop object_update(oop obj) {
  if (object_is_saved(obj)) {
    return obj.mem[0];
  } else {
    return obj;
  }
}

void object_update_all_refs() {
  oop* ptr;
  for (ptr = object_memory.current.start;
       ptr < object_memory.current.free;
       ptr++) {
    *ptr = region(*ptr)->update(*ptr);
  }
}

void object_enumerate_refs(oop obj, void (*callback)(oop ref)) {
  // object_update only works because objects don't get moved into
  // different regions here.  In other regions, the memory layout may
  // be different?
  obj = object_update(obj);
  uint size = get_smallint(obj.mem[-1]);
  uint i;
  for (i = 0; i < size; i++) {
    callback(obj.mem[i]);
  }
}

void object_region_init(uint size) {
  half_space_init(&object_memory.current, size);
  half_space_init(&object_memory.old, size);
  // Hooks.
  object_region.on_gc_start = object_on_gc_start;
  object_region.on_gc_stop = noop;
  object_region.save = object_save;
  object_region.is_saved = object_is_saved;
  object_region.update = object_update;
  object_region.update_all_refs = object_update_all_refs;
  object_region.enumerate_refs = object_enumerate_refs;
}

// ------------------------------------------------------------------

// Region for object
region_t* region(oop obj) {
  // Could be done by asking the regions.
  if (is_smallint(obj) || is_char(obj) || is_symbol(obj) || is_nil(obj)) {
    return &primitive_region;
  } else {
    return &object_region;
  }
}

void init_gc() {
  object_region_init(1 << 22);  // TODO: Enough?
  primitive_region_init();
}

void traverse_object_graph(oop current) {
  region_t* current_region = region(current);
  if (current_region->is_saved(current)) {
    return;
  }

  current_region->save(current);
  current_region->enumerate_refs(current, &traverse_object_graph);
}

boolean should_skip_gc() {
  uint fill = object_memory.current.free - object_memory.current.start;
  return TO_BOOL((100*fill / object_memory.current.size) < 75);
}

oop garbage_collect(oop root) {
  if (should_skip_gc()) {
    return root;
  }
  // object_print_fill(&object_memory->current, "before");
  // TODO: Only one root?
  primitive_region.on_gc_start();
  object_region.on_gc_start();
  // Traverse roots
  traverse_object_graph(root);
  oop result = region(root)->update(root);
  // Tell regions to update all refs.
  primitive_region.update_all_refs();
  object_region.update_all_refs();
  // TODO: This hack makes the interpreter know the new location
  // of the cons type.
  symbols._cons = region(symbols._cons)->update(symbols._cons);
  // Tell regions that the collection has finished.
  primitive_region.on_gc_stop();
  object_region.on_gc_stop();
  // object_print_fill(&object_memory->current, "after");
  return result;
}
