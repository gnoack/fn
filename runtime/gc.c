#include <string.h>

#include "debug.h"
#include "eval.h"  // for global_env
#include "gc.h"
#include "symbols.h"
#include "value.h"

// TODO: Eliminate a lot of code duplication here!


// #define GC_SUMMARY 1
// #define GC_DEBUG 1
// #define GC_LOGGING 1

#ifdef GC_DEBUG
  #define SANE(x) (sane(x))
  // Forward declaration.
  oop sane(oop obj);
#else
  #define SANE(x) (x)
#endif  // GC_DEBUG


unsigned int gc_protect_counter;


// TODO: enumerate_refs only calls traverse_object_graph.  Hardcode it?
typedef struct {
  // Executed before a GC run.
  void (*on_gc_start)();
  // Executed after a GC run.
  void (*on_gc_stop)();
  // Rescue the object from deletion in this GC round.
  void (*save)(oop obj);
  // True if the object is guaranteed to exist after the GC round.
  bool (*is_saved)(oop obj);
  // Returns the new pointer for the given pointer.
  oop (*update)(oop obj);
  // Updates all object references within the memory region.
  void (*update_all_refs)();
  // Enumerates all outgoing references of the given object.
  void (*enumerate_refs)(oop obj, void (*callback)(oop ref));
} region_t;

// Forward declaration.
static region_t* region(oop obj);


/*
 * Immediate region.
 *
 * The immediate region handles immediate objects like characters and
 * smallints, whose pointers don't change after a GC.  All objects of
 * this type are automatically rescued into the next generation.
 */
static void save_noop(oop obj) { /* Not needed. */ }
static bool is_saved_always_true(oop obj) { return true; }
static oop identity(oop obj) { return obj; }
static void noop() { /* No-op. */ }
static void enumerate_refs_none(oop obj, void (*callback)(oop ref)) {}

region_t immediate_region;

void immediate_region_init() {
  immediate_region.on_gc_start = noop;
  immediate_region.on_gc_stop = noop;
  immediate_region.save = save_noop;
  immediate_region.is_saved = is_saved_always_true;
  immediate_region.update = identity;
  immediate_region.update_all_refs = noop;
  immediate_region.enumerate_refs = enumerate_refs_none;
}


/*
 * Half spaces.
 *
 * Half spaces are handlers for memory regions.
 */

typedef struct {
  oop* free;
  oop* start;
  oop* end;
  fn_uint size;
} half_space;

#ifdef GC_DEBUG
static void print_half_space(const char* name, half_space* a) {
  printf("Half space %s\n", name);
  printf("... start: %p\n", a->start);
  printf("... free : %p\n", a->free);
  printf("... end  : %p\n", a->end);
  printf("... size : %lux\n", a->size);
}
#define PRINT_HALF_SPACE(name, space) (print_half_space(name, space))
#else  // GC_DEBUG
#define PRINT_HALF_SPACE(name, space)
#endif  // GC_DEBUG

static void half_space_init(half_space* space, fn_uint size) {
  space->start = (oop*) calloc(sizeof(oop), size);
  space->end = space->start + size;
  space->free = space->start;
  space->size = size;
  #ifdef GC_DEBUG
  print_half_space("new half space", space);
  #endif
}

static void half_space_clear(half_space* space) {
  space->free = space->start;
  bzero(space->start, sizeof(oop) * space->size);
}

// Swaps two half-spaces.
static void half_space_swap(half_space* a, half_space* b) {
  half_space tmp;
  memcpy(&tmp, a, sizeof(half_space));
  memcpy(a, b, sizeof(half_space));
  memcpy(b, &tmp, sizeof(half_space));
}

// Write half space into file.
static void half_space_serialize_to_file(half_space* space, FILE* out) {
  fwrite(space, sizeof(half_space), 1, out);
  fwrite(space->start, sizeof(oop), space->free - space->start, out);
  PRINT_HALF_SPACE("written", space);
}

// Resize and clear half space.
static void half_space_resize(half_space* a, fn_uint size) {
  if (a->size != size) {
    a->size = size;
    a->start = realloc(a->start, sizeof(oop) * size);
    a->free = a->start;
    a->end = a->start + size;
  }
  half_space_clear(a);
}

static void half_space_deserialize_from_file(
    FILE* in, half_space* current, half_space* old,
    half_space* in_old_process) {
  fread(in_old_process, sizeof(half_space), 1, in);
  half_space_resize(current, in_old_process->size);
  half_space_resize(old, in_old_process->size);
  fread(current->start,
        sizeof(oop), in_old_process->free - in_old_process->start, in);
  current->free += (in_old_process->free - in_old_process->start);
  PRINT_HALF_SPACE("des old", old);
  PRINT_HALF_SPACE("des current", current);
}

static void pointered_half_space_enumerate_objects(
    half_space* a, void(*callback)(oop object)) {
  oop* current;
  for (current = a->start + 1;
       current < a->free;
       current += get_smallint(current[-1]) + 1) {
    oop object;
    object.mem = current;
    callback(object);
  }
}

/*
 * Allocate an object of the given size in the half space.
 * This doesn't care about storing object size within the half space.
 */
static oop half_space_alloc(half_space* space, fn_uint size) {
  if (size == 0L) {
    printf("You can't allocate empty objects in a half space.\n");
    exit(1);
  }
  oop result;
  result.mem = space->free;
  space->free += size;
  if (space->free >= space->end) {
    printf("Too little space in new half-space (%u needed).\n", size);
    exit(1);
  }
  return result;
}

static bool half_space_contains(half_space* space, oop obj) {
  return !is_smallint(obj) &&
    space->start <= obj.mem &&
    obj.mem < space->end;
}

#ifdef GC_LOGGING
static void half_space_print_fill(
    const half_space* space, const char* name) {
  fn_uint fill = space->free - space->start;
  fn_uint size = space->size;
  printf("GC: %6s: Fill: %d of %d (%d %%)\n",
         name, (int) fill, (int) size, (int) (100*fill / size));
}
#endif  // GC_LOGGING


/*
 * Object memory.
 *
 * Structure of objects in memory:
 *
 *           Object f
 *    ,---------^---------.
 *  --+----+----+----+----+----+----+--
 *    |  3 | f0 | f1 | f2 |  4 | g0 |
 *  --+----+----+----+----+----+----+--
 *           ^
 *   Object pointer points at first field.
 *
 * To allocate an object of size n, allocate n+1
 * oops from the half space, put the smallint n into
 * the first oop, return a pointer to the second oop.
 */

struct {
  half_space current;
  half_space old;
} object_memory;

region_t object_region;

static void object_on_gc_start() {
  half_space_swap(&object_memory.current, &object_memory.old);
  half_space_clear(&object_memory.current);
}

// Allocate in new space, size in numbers of `oop's.
oop gc_object_alloc(fn_uint size) {
  // XXX: This is a hack to make sure we have enough to run the GC.
  if (size == 0L) {
    size = 1;
  }
  oop result = half_space_alloc(&object_memory.current, size + 1);
  result.mem[0] = make_smallint(size);
  result.mem = result.mem + 1;
  return result;
}

// True if obj references a broken heart.
static bool object_is_saved(oop obj) {
  GC_CHECK(is_nil(obj.mem[-1]) || is_smallint(obj.mem[-1]),
           "Must be int or nil (broken heart).");
  return is_nil(obj.mem[-1]);
}

static void object_save(oop obj) {
  CHECKV(!object_is_saved(obj), obj, "Must be unsaved.");
  CHECKV(half_space_contains(&object_memory.old, obj),
         obj, "Object must be in old half-space to be saved.");
  // Move.
  fn_uint size = get_smallint(obj.mem[-1]);
  oop newobj = gc_object_alloc(size);
  fn_uint i;
  for (i = 0; i < size; i++) {
    newobj.mem[i] = obj.mem[i];
  }

  // Mark as broken heart.
  obj.mem[-1] = NIL;
  obj.mem[0] = SANE(newobj);
}

static oop object_update(oop obj) {
  if (object_is_saved(obj)) {
    return obj.mem[0];
  } else {
    return obj;
  }
}

static void object_update_all_refs() {
  oop* ptr;
  for (ptr = object_memory.current.start;
       ptr < object_memory.current.free;
       ptr++) {
    *ptr = region(*ptr)->update(*ptr);
  }
}

// TODO: Move to a better place.
static void relocate_ref(
    oop* ptr, half_space* old_space, half_space* new_space) {
  if (half_space_contains(old_space, *ptr)) {
    ptr->mem = ptr->mem - old_space->start + new_space->start;
#ifdef GC_DEBUG
    // Note: ptr *may* be in the old space at the same time now.  This
    // is the case when we deserialize from a file and the serialized
    // memory regions in the original process have overlapping address
    // range to ours.  (Happened on ARM.)
    CHECK(half_space_contains(new_space, *ptr), "Bad relocation.");
#endif  // GC_DEBUG
  }
}

static void object_relocate_all_refs(
    half_space* old_space_a, half_space* new_space_a,
    half_space* old_space_b, half_space* new_space_b) {
  if (old_space_a->start == new_space_a->start &&
      old_space_b->start == new_space_b->start) {
    return;
  }
  oop* ptr;
  for (ptr = object_memory.current.start;
       ptr < object_memory.current.free;
       ptr++) {
    relocate_ref(ptr, old_space_a, new_space_a);
    relocate_ref(ptr, old_space_b, new_space_b);
  }
}

static void object_enumerate_refs(oop obj, void (*callback)(oop ref)) {
  // object_update only works because objects don't get moved into
  // different regions here.  In other regions, the memory layout may
  // be different?
  obj = object_update(obj);
  fn_uint size = get_smallint(obj.mem[-1]);
  fn_uint i;
  for (i = 0; i < size; i++) {
    callback(obj.mem[i]);
  }
}

// Only valid outside of GC run.
extern bool gc_is_object(oop obj) {
  return half_space_contains(&object_memory.current, obj);
}

static void object_region_init(fn_uint size) {
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


/*
 * Raw byte memory.
 *
 * The most dangerous form of memory available.  Each oop in raw
 * memory is prepended with the object size before allocation.
 */
struct {
  half_space current;
  half_space old;
} raw_memory;

region_t raw_memory_region;

static void raw_memory_on_gc_start() {
  half_space_swap(&raw_memory.current, &raw_memory.old);
  half_space_clear(&raw_memory.current);
}

// Size in oops for size in bytes.
// This rounds up the oop size.
static inline fn_uint raw_memory_oop_size(fn_uint byte_size) {
  // Round up to oop size and express in number of oops.
  fn_uint size = (byte_size + sizeof(oop) - 1) / sizeof(oop);
  // This is a hack to make sure we're always allocating enough for GC.
  if (size == 0) {
    size = 1;
  }
  return size;
}

// Size in bytes
oop gc_raw_memory_alloc(fn_uint byte_size) {
  fn_uint size = raw_memory_oop_size(byte_size);
  oop result = half_space_alloc(&raw_memory.current, size + 1);
  result.mem[0] = make_smallint(byte_size);
  result.mem = result.mem + 1;
  return result;
}

static void raw_memory_save(oop obj) {
  // Sharing the same is_saved method with the object allocator.
  GC_CHECK(!object_is_saved(obj), "Must be unsaved.");
  GC_CHECK(half_space_contains(&raw_memory.old, SANE(obj)), "obj is old");
  // Move.
  fn_uint byte_size = get_smallint(obj.mem[-1]);
  oop newobj = gc_raw_memory_alloc(byte_size);
  memcpy(newobj.mem, obj.mem, byte_size);

  // Mark as broken heart.
  obj.mem[-1] = NIL;
  obj.mem[0] = newobj;

  GC_CHECK(object_is_saved(obj), "Must be saved now.");
  GC_CHECK(half_space_contains(&raw_memory.current, SANE(newobj)),
           "newobj is current");
  GC_CHECK(half_space_contains(&raw_memory.old, SANE(obj)), "obj is old");
}

// Only valid outside of GC run.
bool gc_is_raw_memory(oop obj) {
  return half_space_contains(&raw_memory.current, SANE(obj));
}

static void raw_memory_region_init(fn_uint size) {
  half_space_init(&raw_memory.current, size);
  half_space_init(&raw_memory.old, size);
  // Hooks.
  raw_memory_region.on_gc_start = raw_memory_on_gc_start;
  raw_memory_region.on_gc_stop = noop;
  raw_memory_region.save = raw_memory_save;
  raw_memory_region.is_saved = object_is_saved;  // Shared.
  raw_memory_region.update = object_update;  // Shared.
  raw_memory_region.update_all_refs = noop;
  raw_memory_region.enumerate_refs = enumerate_refs_none;
}


// Forward declaration.
static void traverse_object_graph(oop current);

/*
 * Persistent references are references in C code that need to be
 * updated when objects move in a GC run.  A persistent ref enumerator
 * is a function that's calling another function for each place where
 * an oop is referenced from C.
 */

#define MAX_ENUMERATOR_COUNT 8
fn_uint enumerator_count = 0;
enumerator_t enumerators[MAX_ENUMERATOR_COUNT];

void gc_register_persistent_refs(enumerator_t enumerator) {
  CHECK(enumerator_count < MAX_ENUMERATOR_COUNT,
        "Too many persistent ref enumerators.");
  enumerators[enumerator_count] = enumerator;
  enumerator_count++;
}

static void traverse_object_in_place(oop* place) {
  traverse_object_graph(*place);
  *place = region(*place)->update(*place);
}

static void traverse_persistent_refs() {
  fn_uint i;
  for (i=0; i<enumerator_count; i++) {
    enumerators[i](traverse_object_in_place);
  }
}


/*
 * Core garbage collection functionality:
 * - Traversing the object graph.
 * - Figuring out the region responsible for an oop.
 */

// Finds the region for an object
static region_t* region(oop obj) {
  // Could be done by asking the regions.
  if (is_smallint(obj) || is_char(obj) || is_symbol(obj) || is_nil(obj)) {
    return &immediate_region;
  } else if (half_space_contains(&object_memory.old, obj) ||
             half_space_contains(&object_memory.current, obj)) {
    return &object_region;
  } else {
    CHECK(half_space_contains(&raw_memory.old, obj) ||
          half_space_contains(&raw_memory.current, obj),
          "Must be a raw object.");
    return &raw_memory_region;
  }
}

static void traverse_object_graph(oop current) {
  region_t* current_region = region(current);
  if (current_region->is_saved(current)) {
    return;
  }

  current_region->save(current);
  current_region->enumerate_refs(current, &traverse_object_graph);
}

bool _run_gc_soon;

extern void run_gc_soon() {
  _run_gc_soon = true;
}

extern void init_gc() {
  _run_gc_soon = false;
  object_region_init(1 << 27);  // TODO: Enough?
  raw_memory_region_init(1 << 17);  // TODO: Enough?
  immediate_region_init();
  gc_protect_counter = 0;
}

// Decide whether to do the garbage collection at all.
static bool should_skip_gc() {
  if (gc_protect_counter > 0) {
    return true;
  }
  if (_run_gc_soon) {
    _run_gc_soon = false;
    return false;
  }
  fn_uint obj_fill = object_memory.current.free - object_memory.current.start;
  fn_uint pri_fill = raw_memory.current.free - raw_memory.current.start;
  return (100*obj_fill / object_memory.current.size) < 50 &&
         (100*pri_fill / raw_memory.current.size) < 80;
}

#ifdef GC_DEBUG
// Forward declaration.
static void pointered_half_space_sanity_check(half_space* space);
#endif  // GC_DEBUG

#ifdef GC_SUMMARY
// Forward declaration.
static void pointered_half_space_print_object_types(half_space* a);
#endif

extern void gc_run() {
  if (should_skip_gc()) {
    return;
  }
  #ifdef GC_LOGGING
  half_space_print_fill(&object_memory.current, "object before");
  half_space_print_fill(&raw_memory.current, "raw before");
  #endif  // GC_LOGGING
  #ifdef GC_SUMMARY
  pointered_half_space_print_object_types(&object_memory.current);
  #endif  // GC_SUMMARY
  #ifdef GC_DEBUG
  printf("OBJ MEM (before gc):\n");
  pointered_half_space_sanity_check(&object_memory.current);
  printf("RAW MEM (before gc):\n");
  pointered_half_space_sanity_check(&raw_memory.current);
  #endif  // GC_DEBUG
  raw_memory_region.on_gc_start();
  immediate_region.on_gc_start();
  object_region.on_gc_start();
  // Traverse roots
  traverse_persistent_refs();
  // Tell regions to update all refs.
  raw_memory_region.update_all_refs();
  immediate_region.update_all_refs();
  object_region.update_all_refs();

  // TODO: Also update refs from within symbol hash map,
  // but only if they point to a broken heart.  If they don't,
  // discard the entry in the symbol hash map.  (It's not used.)
  // We then have 'compacted' symbols.  Don't forget to unregister the
  // symbol hash map from the regular persistent refs list.

  // Tell regions that the collection has finished.
  raw_memory_region.on_gc_stop();
  immediate_region.on_gc_stop();
  object_region.on_gc_stop();
  #ifdef GC_LOGGING
  half_space_print_fill(&object_memory.current, "object after");
  half_space_print_fill(&raw_memory.current, "raw after");
  #endif  // GC_LOGGING
  #ifdef GC_DEBUG
  printf("OBJ MEM (after gc):\n");
  pointered_half_space_sanity_check(&object_memory.current);
  printf("RAW MEM (after gc):\n");
  pointered_half_space_sanity_check(&raw_memory.current);
  #endif  // GC_DEBUG
}

void gc_serialize_to_file(char* filename) {
  run_gc_soon();
  gc_run();
  FILE* out = fopen(filename, "w");
  size_t items_written = fwrite(&global_env, sizeof(oop), 1, out);
  CHECK(items_written == 1, "Write error.");
  items_written = fwrite(&symbols, sizeof(symbols), 1, out);
  CHECK(items_written == 1, "Write error.");

  half_space_serialize_to_file(&object_memory.current, out);
  half_space_serialize_to_file(&raw_memory.current, out);
  fclose(out);
}

static void relocate_symbols_struct(
    half_space* old_space, half_space* new_space) {
  oop* ptr = (oop*) &symbols;
  oop* end = (oop*) ((&symbols) + 1);
  while (ptr < end) {
    relocate_ref(ptr, old_space, new_space);
    ptr++;
  }
}

/*
 * Deserializes the program state from a file.
 */
extern void gc_deserialize_from_file(char* filename) {
  FILE* in = fopen(filename, "r");

  // Copy file contents over existing symbols map and global env ptr.
  size_t items_read = fread(&global_env, sizeof(oop), 1, in);
  CHECK(items_read == 1, "Read error.");
  items_read = fread(&symbols, sizeof(symbols), 1, in);
  CHECK(items_read == 1, "Read error.");

  // Copy file contents over existing half spaces.
  half_space object_space_in_old_process;
  half_space_deserialize_from_file(
      in, &object_memory.current, &object_memory.old,
      &object_space_in_old_process);

  half_space raw_space_in_old_process;
  half_space_deserialize_from_file(
      in, &raw_memory.current, &raw_memory.old,
      &raw_space_in_old_process);

  // Update pointers in object space.
  object_relocate_all_refs(&object_space_in_old_process,
                           &object_memory.current,
                           &raw_space_in_old_process,
                           &raw_memory.current);

  // Update pointers in symbols struct in C. (Save it.)
  relocate_symbols_struct(&object_space_in_old_process, &object_memory.current);

  // Update pointers in symbol hashmap.
  // Note: We need to have structs._symbol updated for this to work!
  symbol_hashmap_clear();
  pointered_half_space_enumerate_objects(
      &object_memory.current, symbol_hashmap_register);

  relocate_ref(&global_env, &object_space_in_old_process,
               &object_memory.current);

  fclose(in);
}



#ifdef GC_DEBUG
// SANE checks for absurdly big object sizes.
// This is likely to be a sign that something
// has been mispointered along the way.

static oop sane(oop obj) {
  bool is_mem = (half_space_contains(&object_memory.old, obj) ||
                 half_space_contains(&object_memory.current, obj));
  bool is_raw = (half_space_contains(&raw_memory.old, obj) ||
                 half_space_contains(&raw_memory.current, obj));
  if (is_mem || is_raw) {
    const char* allocation_type = is_mem ? "Pointer" : "Raw";
    if (is_nil(obj.mem[-1])) {
      return obj;  // Broken heart.
    }
    if (!is_smallint(obj.mem[-1])) {
      printf("%s object at address %p is missing a size.\n",
             allocation_type, obj.mem);
      print_zone(obj);
      exit(1);
    }
    if (get_smallint(obj.mem[-1]) > 0xffff) {
      printf("%s object size %lu is too large.\n",
             allocation_type, get_smallint(obj.mem[-1]));
      exit(1);
    }
  }
  return obj;
}

// Check consistency of pointered half space between GC runs.
static void pointered_half_space_sanity_check(half_space* space) {
  printf("Start: %p\n", space->start);
  printf(" Free: %p\n", space->free);
  printf("  End: %p\n", space->end);
  bool is_raw = (space == &raw_memory.current ||
                 space == &raw_memory.old);
  oop* current = space->start + 1;
  while (current < space->free) {
    oop size = current[-1];
    if (!is_smallint(size)) {
      printf("BROKEN HEART AT 0x%p\n", current);
      oop ptr;
      ptr.mem = current;
      print_zone(ptr);
      exit(1);
    }
    if (!is_raw) {
      // Checking values in raw space is a bad idea, they could contain
      // valid-looking adresses that aren't valid.  (Yes, it happened.)
      // In object space, this makes sure the types at index 0 exist.
      sane(*current);
    }
    // Raw memory space is allocated in bytes, not in oops.
    if (is_raw) {
      current += raw_memory_oop_size(get_smallint(size)) + 1;
    } else {
      current += get_smallint(size) + 1;
    }
  }
}
#endif  // GC_DEBUG


// Counting summaries about how many objects of what type exist.

#ifdef GC_SUMMARY
#define MAX_TYPE_COUNT 100
int seen_type_count;
struct { oop type; unsigned int count; } object_counts[MAX_TYPE_COUNT];

static void print_object_type(oop obj) {
  oop type = obj.mem[0];
  int i;
  for (i=0; i<seen_type_count; i++) {
    if (value_eq(object_counts[i].type, type)) {
      object_counts[i].count ++;
      return;
    }
  }
  seen_type_count++;
  object_counts[i].type = type;
  object_counts[i].count = 1;
  CHECK(seen_type_count < MAX_TYPE_COUNT, "Too many object types found.");
}

static void print_summary() {
  int i;
  for (i=0; i<seen_type_count; i++) {
    printf("%9u ", object_counts[i].count);
    println_value(object_counts[i].type);
  }
}

// Output of this can be piped into | sort | uniq -c to count.
static void pointered_half_space_print_object_types(half_space* a) {
  seen_type_count = 0;
  pointered_half_space_enumerate_objects(a, print_object_type);
  print_summary();
}
#endif  // GC_SUMMARY
