/* Support for dynamic loading of C libraries via libdl. */

#include <dlfcn.h>
#include <unistd.h>

#include "debug.h"  // for println_value.
#include "dl.h"
#include "eval.h"
#include "gc.h"
#include "memory.h"
#include "primitives.h"
#include "strings.h"
#include "symbols.h"
#include "carcdr.h"

static oop pack_aligned_cptr(void* i) {
  CHECK((((fn_uint) i) & 1) == 0, "Needs to be an aligned pointer.");
  return make_smallint(((fn_uint) i) >> 1);
}

static void* unpack_aligned_cptr(oop ptr) {
  return (void*) (get_smallint(ptr) << 1);
}

FUNC(primitive_dlopen) {
  PARSE_ONE_ARG(name);
  char* c_name = c_string(name);
  void* handle = dlopen(c_name, RTLD_LOCAL | RTLD_NOW);
  free(c_name);

  char* error = dlerror();
  CHECK(error == NULL, error);
  return pack_aligned_cptr(handle);
}

FUNC(primitive_dlsym) {
  PARSE_TWO_ARGS(handle, symbolname);
  char* c_symbolname = c_string(symbolname);
  void* c_handle = unpack_aligned_cptr(handle);
  void* (*c_sym)(void* arg, ...) = dlsym(c_handle, c_symbolname);
  free(c_symbolname);

  char* error = dlerror();
  CHECK(error == NULL, error);
  return pack_aligned_cptr(c_sym);
}

typedef struct {
  void** ptrs;
  int size;
} ptrs_to_free_t;

typedef union {
  void* ptr;
  int integer;
} c_value;

static void init_ptrs_to_free(ptrs_to_free_t* ptrs_to_free, int argnum) {
  ptrs_to_free->ptrs = malloc(argnum * sizeof(void*));
  ptrs_to_free->size = 0;

}

// No bounds checks performed here.
static void add_ptr(ptrs_to_free_t* ptrs_to_free, void* ptr) {
  ptrs_to_free->ptrs[ptrs_to_free->size] = ptr;
  ptrs_to_free->size++;
}

static void free_all(ptrs_to_free_t* ptrs_to_free) {
  int i;
  for(i = 0; i < ptrs_to_free->size; i++) {
    free(ptrs_to_free->ptrs[i]);
  }
  free(ptrs_to_free->ptrs);
  ptrs_to_free->size = 0;
}

static oop c_to_oop(c_value value, symbol_t* type, ptrs_to_free_t* ptrs_to_free) {
  if (type == symbols._c_int) {
    return make_smallint(value.integer);
  } else if (type == symbols._c_void) {
    return NIL;
  } else {
    CHECKV(type == symbols._c_str, symbol_to_oop(type),
           "Unsupported type specifier, use 'int, 'str or 'void.");
    if (value.ptr != NULL) {
      // TODO: Allow to specify whether the result needs to be free'd.
      // Some C functions (e.g. getenv) return pointers into funny memory areas.
      if (value.ptr <= sbrk(0)) {  // Hack: Object is on heap.
        add_ptr(ptrs_to_free, value.ptr);
      }
      // We simply trust that the C value returned is a pointer to a string.
      return make_string((char*) value.ptr);
    } else {
      return NIL;
    }
  }
}

static c_value oop_to_c(oop input, oop type, ptrs_to_free_t* ptrs_to_free) {
  CHECKV(is_symbol(type), type, "C type specifier must be a symbol.");
  c_value result;
  if (to_symbol(type) == symbols._c_int) {
    result.integer = get_smallint(input);
  } else {
    CHECKV(to_symbol(type) == symbols._c_str, type,
           "Unsupported type specifier, use 'int or 'str.");
    result.ptr = c_string(input);
    add_ptr(ptrs_to_free, result.ptr);
  }
  return result;
}

#define GET_ARG(name) \
  c_value name = oop_to_c(first(args_oop), first(argtypes), &ptrs_to_free); \
  args_oop = rest(args_oop);                                                \
  argtypes = rest(argtypes);                                                \

/*
 * Args: dlsym resulttype argnum argtype-list arg-list
 * e.g. (_call_dlsym strcat 'str 2 '(str str) (list "Hello" "World")).
 */
FUNC(primitive_call_dlsym) {
  PARSE_FIVE_ARGS(sym_oop, resulttype_oop, argnum_oop, argtypes, args_oop);
  fn_uint argnum = get_smallint(argnum_oop);

  // TODO: Is this the proper type for a generic C function?!?
  void* fn_addr = unpack_aligned_cptr(sym_oop);
  c_value (*c_function)(c_value arg, ...) = (c_value (*)(c_value, ...)) fn_addr;

  // Keep track of pointers to free after execution,
  // in case we have to create some temporary C objects.
  ptrs_to_free_t ptrs_to_free;
  init_ptrs_to_free(&ptrs_to_free, argnum);

  c_value c_result;
  switch (argnum) {
  case 0: {
    c_value (*c_function)() = (c_value (*)()) fn_addr;
    c_result = c_function();
  }
    break;
  case 1: {
    GET_ARG(arg0);
    c_result = c_function(arg0);
  }
    break;
  case 2: {
    GET_ARG(arg0);
    GET_ARG(arg1);
    c_result = c_function(arg0, arg1);
  }
    break;
  case 3: {
    GET_ARG(arg0);
    GET_ARG(arg1);
    GET_ARG(arg2);
    c_result = c_function(arg0, arg1, arg2);
  }
    break;
  case 4: {
    GET_ARG(arg0);
    GET_ARG(arg1);
    GET_ARG(arg2);
    GET_ARG(arg3);
    c_result = c_function(arg0, arg1, arg2, arg3);
  }
    break;
  default:
    FATAL("Too many arguments to dlsym-loaded function.");  // No!
    break;
  }

  // Convert result and free everything malloc'd along the way.
  CHECKV(is_symbol(resulttype_oop), resulttype_oop,
	 "Resulttype must be a symbol.");
  oop result = c_to_oop(c_result, to_symbol(resulttype_oop), &ptrs_to_free);
  free_all(&ptrs_to_free);
  return result;
}

void init_dl() {
  register_globally_fn("_dlopen", primitive_dlopen);
  register_globally_fn("_dlsym", primitive_dlsym);
  register_globally_fn("_call_dlsym", primitive_call_dlsym);
}
