/* Support for dynamic loading of C libraries via libdl. */

#include <dlfcn.h>
#include <unistd.h>

#include "strings.h"
#include "dl.h"
#include "primitives.h"


oop primitive_dlopen(oop args) {
  PARSE_ONE_ARG(name);
  char* c_name = c_string(name);
  void* handle = dlopen(c_name, RTLD_LOCAL | RTLD_NOW);
  free(c_name);

  char* error = dlerror();
  CHECK(error == NULL, error);
  return make_smallint((fn_uint) handle);
}

oop primitive_dlsym(oop args) {
  PARSE_TWO_ARGS(handle, symbolname);
  char* c_symbolname = c_string(symbolname);
  void* c_handle = (void*) get_smallint(handle);
  void* (*c_sym)(void* arg, ...) = dlsym(c_handle, c_symbolname);
  free(c_symbolname);

  char* error = dlerror();
  CHECK(error == NULL, error);
  return make_smallint((fn_uint) c_sym);
}

oop primitive_call_dlsym(oop args) {
  // TODO: Varargs.
  PARSE_TWO_ARGS(sym, arg);
  void* (*c_sym)(void* arg, ...) = (void* (*)(void*, ...)) get_smallint(sym);
  char* c_arg = c_string(arg);
  void* c_result = c_sym(c_arg);  // Call.
  free(c_arg);
  oop result;
  if (c_result) {
    result = make_string((const char*) c_result);
    // TODO: Memory leak: Some C functions like getenv() return
    // non-freeable c strings, we should distinguish between the two cases.
    if (((void*) c_result) < sbrk(0)) {  // On heap.  (Dirty hack :-/)
      free(c_result);
    }
  } else {
    result = NIL;
  }
  return result;
}

void init_dl() {
  register_globally_fn("_dlopen", primitive_dlopen);
  register_globally_fn("_dlsym", primitive_dlsym);
  register_globally_fn("_call_dlsym", primitive_call_dlsym);
}
