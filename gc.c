
#include "value.h"
#include "gc.h"

typedef struct {
  // TODO: Start GC, Stop GC hook methods.
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
void update_all_refs_noop() { /* No-op. */ }
void enumerate_refs_none(oop obj, void (*callback)(oop ref)) { /* None */ }

region_t primitive_region;

void primitive_region_init() {
  primitive_region.save = save_noop;
  primitive_region.is_saved = is_saved_always_true;
  primitive_region.update = identity;
  primitive_region.update_all_refs = update_all_refs_noop;
  primitive_region.enumerate_refs = enumerate_refs_none;
}

// ------------------------------------------------------------------

struct {
  oop* old_space;
  oop* new_space;
  oop* free_new;
  oop* upper_new;
} object_memory;

region_t object_region;

// Allocate in new space.
oop object_alloc(uint size) {
  *(object_memory.free_new) = make_smallint(size);
  object_memory.free_new++;

  oop result;
  result.mem = object_memory.free_new;
  object_memory.free_new = object_memory.free_new + size;
  if (object_memory.free_new >= object_memory.upper_new) {
    printf("Too little space in new half-space.");
    exit(1);
  }
  return result;
}

void object_save(oop obj) {
  // Move.
  uint size = get_smallint(obj.mem[-1]);
  oop newobj = object_alloc(size);
  for (uint i = 0; i < size; i++) {
    newobj.mem[i] = obj.mem[i];
  }

  // Mark as broken heart.
  obj.mem[-1] = NIL;
  obj.mem[0] = newobj;
}

// True if obj references a broken heart.
boolean object_is_saved(oop obj) {
  return TO_BOOL(obj.mem[-1] == NIL);
}

oop object_update(oop obj) {
  if (object_is_saved(obj)) {
    return obj.mem[0];
  } else {
    return obj;
  }
}

void object_update_all_refs() {
  for (oop* ptr = object_memory.new_space;
       ptr < object_memory.free_new;
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
  for (uint i = 0; i < size; i++) {
    callback(obj.mem[i]);
  }
}

void object_region_init(uint size) {
  object_memory.old_space = (oop*) malloc(sizeof(oop) * size);
  object_memory.new_space = (oop*) malloc(sizeof(oop) * size);
  object_memory.free_new = object_memory.new_space;
  object_memory.upper_new = object_memory.free_new + size;
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
  object_region_init(1 << 18);  // TODO: Enough?
  primitive_region_init();
}

oop garbage_collect(oop root) {
  // TODO: Only one root? :-)  Sounds reasonable to me...
  // * Tell regions that GC is starting.
  // * Traverse roots
  // * Tell regions to update all refs.
  // * Tell regions that GC is stopping.
}

void traverse_object_graph(oop current) {
  region_t* current_region = region(current);
  if (current_region->is_saved(current)) {
    return;
  }

  current_region->save(current);
  current_region->enumerate_refs(current, &traverse_object_graph);
}
