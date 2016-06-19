/* A C macro for generating typesafe resizable array structures. */

#define DEFARRAY(name, type)                                            \
  struct name {                                                         \
    type* items;                                                        \
    int size;                                                           \
    int capacity;                                                       \
  };                                                                    \
                                                                        \
  void init_ ## name(struct name* arr, int initial_capacity) {          \
    CHECK(initial_capacity > 0, "Need an initial capacity > 0");        \
    *arr = (struct name) {                                              \
      .size = 0,                                                        \
      .capacity = initial_capacity,                                     \
      .items = calloc(initial_capacity, sizeof(type)),                  \
    };                                                                  \
  }                                                                     \
                                                                        \
  void free_ ## name(struct name* arr) {                                \
    free(arr->items);                                                   \
  }                                                                     \
                                                                        \
  /* Return the index of the item in the array, -1 if not found. */     \
  int name ## _find(struct name* arr, type item,                        \
                    bool (*equal)(type a, type b)) {                    \
    int i;                                                              \
    for (i = 0; i < arr->size; i++) {                                   \
      if (equal(arr->items[i], item)) {                                 \
        return i;                                                       \
      }                                                                 \
    }                                                                   \
    return -1;                                                          \
  }                                                                     \
                                                                        \
  type* name ## _append(struct name* arr) {                             \
    if (arr->size >= arr->capacity) {                                   \
      arr->capacity *= 2;                                               \
      /* TODO: Overflow check! */                                       \
      arr->items = realloc(arr->items, arr->capacity * sizeof(type));   \
    }                                                                   \
    type* result = arr->items + arr->size;                              \
    arr->size++;                                                        \
    return result;                                                      \
  }                                                                     \
                                                                        \
  /* Can also be used to set the value at this index. */                \
  type* name ## _at(struct name* arr, int index) {                      \
    CHECK(0 <= index && index < arr->size, "Index out of bounds");      \
    return arr->items + index;                                          \
  }                                                                     \
                                                                        \
  type* name ## _end(struct name* arr) {                                \
    return arr->items + arr->size;                                      \
  }
