
#import "tests.h"

#import <stdio.h>

extern
void fail(const char* filename,
	  unsigned int line,
	  const char* msg) {
  printf("%s, line %d: %s\n", filename, line, msg);
}

int main(int argc, char* argv) {
  /* Register tests here. */
  puts("Running tests...");
  return 0;
}
