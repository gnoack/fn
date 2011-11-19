
extern "C" {

#include "value.h"
#include "gc.h"

}

class Region {
 public:
  // TODO: Start GC, Stop GC hook methods.
  virtual void save(oop obj) = 0;
  virtual boolean is_saved(oop obj) = 0;
  virtual oop update(oop obj) = 0;
  virtual void upate_all_refs() = 0;
  virtual void enumerate_refs(oop obj, void (*callback)(oop ref)) = 0;
};

// Forward declaration.
Region& region(oop obj);

// Numbers, symbols, characters.
class PrimitiveRegion : Region {
 public:
  virtual void save(oop obj) { /* Not needed. */ }
  virtual boolean is_saved(oop obj) { return YES; }
  virtual oop update(oop obj) { return obj; }
  virtual void update_all_refs() { /* No-op. */ }
  virtual void enumerate_refs(oop obj, void (*callback)(oop ref)) { /* None. */ };
};

// Objects.
class ObjectRegion : Region {
 private:
  oop* old_space;
  oop* new_space;
  oop* free_new;
  oop* upper_new;

  // Allocate in new space.
  oop allocate(uint size) {
    *free_new = make_smallint(size);
    free_new++;

    oop result;
    result.mem = free_new;
    free_new = free_new + size;
    if (free_new >= upper_new) {
      printf("Too little space in new half-space.");
      exit(1);
    }
    return result;
  }

 public:
  ObjectRegion(uint size) {
    this->old_space = (oop*) malloc(sizeof(oop) * size);
    this->new_space = (oop*) malloc(sizeof(oop) * size);
    this->free_new = new_space;
    this->upper_new = free_new + size;
  }

  virtual void save(oop obj) {
    // Move.
    uint size = get_smallint(obj.mem[-1]);
    oop newobj = this->allocate(size);
    for (uint i = 0; i < size; i++) {
      newobj.mem[i] = obj.mem[i];
    }

    // Mark as broken heart.
    obj.mem[-1].mem = NULL;
    obj.mem[0] = newobj;
  }

  virtual boolean is_saved(oop obj) {
    return TO_BOOL(obj.mem[-1].mem == NULL);
  }

  virtual oop update(oop obj) {
    if (this->is_saved(obj)) {
      return obj.mem[0];
    } else {
      return obj;
    }
  }

  virtual void update_all_refs() {
    for (oop* ptr = new_space; ptr < free_new; ptr++) {
      *ptr = region(*ptr).update(*ptr);
    }
  }

  virtual void enumerate_refs(oop obj, void (*callback)(oop ref)) {
    obj = this->update(obj);
    uint size = get_smallint(obj.mem[-1]);
    for (uint i = 0; i < size; i++) {
      callback(obj.mem[i]);
    }
  }
};

// Region for object
Region& region(oop obj) {
  // TODO
}

void garbage_collect() {
  // TODO: Only one root? :-)  Sounds reasonable to me...
  // * Tell regions that GC is starting.
  // * Traverse roots
  // * Tell regions to update all refs.
  // * Tell regions that GC is stopping.
}

void traverse_object_graph(oop current) {
  Region& currentRegion = region(current);
  if (currentRegion.is_saved(current)) {
    return;
  }

  currentRegion.save(current);
  currentRegion.enumerate_refs(current, &traverse_object_graph);
}
