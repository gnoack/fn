#include "runtime/value.h"  // boolean.
#include "runtime/gc.h"  // gc serialization functions.
#include "runtime/runtime.h"
#include "runtime/cons.h"  // make_cons.
#include "runtime/eval.h"  // register_globally.
#include "runtime/strings.h"  // make_string.

#include <string.h>  // strcmp.

boolean deserialize_arg = NO;
boolean exit_arg = NO;
boolean load_twice_arg = NO;
boolean serialize_arg = NO;
const char* file_to_load = NULL;

// Returns the first index of the remaining arguments,
// argc if no argument is given.
int parse_args(int argc, char* argv[]) {
  int i;
  for (i=1; i<argc; i++) {
    if (strcmp(argv[i], "-2") == 0) {
      load_twice_arg = YES; continue;
    }
    if (strcmp(argv[i], "-s") == 0) {
      serialize_arg = YES; continue;
    }
    if (strcmp(argv[i], "-S") == 0) {
      deserialize_arg = YES; continue;
    }
    if (strcmp(argv[i], "-x") == 0) {
      exit_arg = YES; continue;
    }
    file_to_load = argv[i];
    return i + 1;
  }
  return argc;
}

int main(int argc, char* argv[]) {
  int remainder_idx = parse_args(argc, argv);
  
  fn_runtime_init();
  
  if (deserialize_arg) {
    gc_deserialize_from_file("fn.img");
  } else {
    fn_runtime_init_lisp_decls();
    if (load_twice_arg) {
      /*
       * Evaluate top level forms again.  The second time, they will
       * all be compiled.  This compiles even the more dubious stuff
       * like functions that haven't been originally defined at
       * top-level.
       */
      fn_runtime_init_lisp_decls();
    }
  }
  if (serialize_arg) {
    gc_serialize_to_file("fn.img");
  }
  if (exit_arg) {
    exit(0);
  }

  oop args = NIL;
  int x;
  for (x = argc - 1; x >= remainder_idx; x--) {
    args = make_cons(make_string(argv[x]), args);
  }
  register_globally("*args*", args);
  
  if (file_to_load != NULL) {
    fn_load_file(file_to_load);
  } else {
    puts("FN " __DATE__ ".");
    puts("Usage:\n"
         "   ./fn FILENAME\n"
         "   ./fn -2 -S -x\n\n"
         "-2 load all main modules twice (compiling on the second go)\n"
         "-s save memory image after loading modules\n"
         "-S load memory image instead of loading modules\n"
         "-x quit without executing tests\n");
    exit(0);
  }
  return 0;
}

