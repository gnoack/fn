
#include "value.h"

/* Fail a tests. Use the FAIL macro to call this. */
extern
void fail(const char* filename,
	  unsigned int line,
	  const char* msg);

extern
void assert_eq(const char* filename,
	       unsigned int line,
	       value_t a,
	       value_t b);

extern
void assert_true(const char* filename,
		 unsigned int line,
		 bool b);

#define FAIL(msg) \
  fail(__FILE__, __LINE__, msg)

#define ASSERT_EQ(expected, actual) \
  assert_eq(__FILE__, __LINE__, expected, actual)

#define ASSERT_TRUE(value) \
  assert_true(__FILE__, __LINE__, value)

#define TEST(name) void name()
#define TESTRUN(name) name();

