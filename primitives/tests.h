
/* Fail a tests. Use the FAIL macro to call this. */
extern void fail(const char* filename,
		 unsigned int line,
		 const char* msg);

#define FAIL(msg) \
  fail(__FILE__, __LINE__, msg)

