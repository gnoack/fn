#ifndef _TESTS_H_
#define _TESTS_H_

#include "runtime/value.h"

/* Fail a tests. Use the FAIL macro to call this. */
void fail(const char* filename,
	  unsigned int line,
	  const char* msg);

void assert_eq(const char* filename,
	       unsigned int line,
	       oop a,
	       oop b);

void assert_true(const char* filename,
		 unsigned int line,
		 bool b);

void assert_false(const char* filename,
		  unsigned int line,
		  bool b);

void assert_nil(const char* filename,
		unsigned int line,
		oop value);

#define FAIL(msg) \
  fail(__FILE__, __LINE__, msg)

#define ASSERT_EQ(expected, actual) \
  assert_eq(__FILE__, __LINE__, expected, actual)

#define ASSERT_TRUE(value) \
  assert_true(__FILE__, __LINE__, value)

#define ASSERT_FALSE(value)			\
  assert_false(__FILE__, __LINE__, value)

#define ASSERT_NIL(value) \
  assert_nil(__FILE__, __LINE__, value)

#define TEST(name) void name()
#define TESTRUN(name) name();

#endif  // _TESTS_H_
