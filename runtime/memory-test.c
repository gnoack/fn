
#include "tests.h"

#include "value.h"
#include "memory.h"

TEST(memory_alloc) {
  oop obj = mem_alloc(1);
  mem_set(obj, 0, make_smallint(99));
  ASSERT_EQ(make_smallint(99), mem_get(obj, 0));
}

TEST(memory_alloc_zero_by_default) {
  oop obj = mem_alloc(1);
  ASSERT_NIL(mem_get(obj, 0));
}

extern
void memory_tests() {
  TESTRUN(memory_alloc);
  TESTRUN(memory_alloc_zero_by_default);
}
