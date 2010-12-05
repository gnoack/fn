
#include "value.h"

/* Fail a tests. Use the FAIL macro to call this. */
extern void fail(const char* filename,
		 unsigned int line,
		 const char* msg);

extern void assert_eq(const char* filename,
		      unsigned int line,
		      value_t a,
		      value_t b);

#define FAIL(msg) \
  fail(__FILE__, __LINE__, msg)

#define ASSERT_EQ(expected, actual) \
  assert_eq(__FILE__, __LINE__, expected, actual)

#define TEST(name) void name()
#define TESTRUN(name) name();

