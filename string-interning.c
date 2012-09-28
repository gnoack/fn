
#include <malloc.h>
#include <err.h>
#include <string.h>

#include "value.h"

/* This is an intentionally simple implementation of string interning.
 * Fast operations are:
 *   * Checking whether a value is interned (for is_symbol?).
 *   * Comparing two interned values (obviously).
 * Slow operations are:
 *   * Interning a string.  (TODO: Could be improved much.)
 */

// TODO: Capability to iterate over interned strings (for readline).
//  See http://cnswww.cns.cwru.edu/php/chet/readline/readline.html#SEC44

// TODO: Unlimited space for interned strings.
char* interned_strings = NULL;

#define INTERNED_DB_SIZE (8 * 1024)

boolean matches(const char* a, const char* b) {
  return TO_BOOL(strcmp(a, b) == 0);
}

boolean is_last(const char* s) {
  return TO_BOOL(s[0] == '\0');
}

const char* intern_string(const char* s) {
  if (interned_strings == NULL) {
    interned_strings = malloc(INTERNED_DB_SIZE);
    interned_strings[0] = '\0';
  }

  char* here_be_dragons = interned_strings + INTERNED_DB_SIZE;
  char* curr = interned_strings;

  while(!is_last(curr)) {
    if (matches(curr, s)) {
      return curr;
    } else {
      curr = curr + strlen(curr) + 1;
      if ((fn_uint) curr & 1) {
	curr++;
      }
    }
  }
  // Append.
  if (curr + strlen(s) + 1 >= here_be_dragons) {
    errx(-1, "String %s can't be interned (doesn't fit).\n", s);
  }
  strcpy(curr, s);
  curr[strlen(s) + 1] = '\0';
  return curr;
}

boolean is_interned(const char* s) {
  if (interned_strings == NULL) {
    return NO; // Nothing is interned yet.
  }

  return TO_BOOL(s >= interned_strings &&
		 s < interned_strings + INTERNED_DB_SIZE);
}
