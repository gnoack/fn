
#include <malloc.h>
#include <err.h>
#include <string.h>

#include "value.h"

/* This is an intentionally simple implementation of string
   interning. */

// 4 kb of interned strings should be enough for now.
char* interned_strings = NULL;

#define INTERNED_DB_SIZE (4 * 1024)

bool matches(const char* a, const char* b) {
  return TO_BOOL(strcmp(a, b) == 0);
}

bool is_last(const char* s) {
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
