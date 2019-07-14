#include "deserialize.h"

#include "cons.h"
#include "data.h"
#include "debug.h"
#include "eval.h"
#include "memory.h"
#include "primitives.h"
#include "procedures.h"
#include "strings.h"
#include "value.h"

// file utilities.
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

enum {
  S_ARRAY              = '[',
  S_CHARACTER          = 'c',
  S_COMPILED_PROCEDURE = 'B',
  S_CONS               = '.',
  S_MEM_BLOCK          = '#',
  S_NIL                = '0',
  S_SMALLINT           = 'i',
  S_STRING             = '\"',
  S_SYMBOL             = 'S',
  S_VAR                = 'v'
};

static inline unsigned char read_byte(FILE* input) {
  int result = fgetc(input);
  CHECK(result != EOF, "Tried to read beyond file.");
  return (unsigned char) result;
}

// Forward declaration.
static oop deserialize(FILE* input);

static fn_uint deserialize_int(FILE* input) {
  fn_uint result = 0;
  unsigned char c;
  do {
    c = read_byte(input);
    result = (result << 7) + (c & 0x7f);
  } while (c & 0x80);
  return result;
}

static oop deserialize_array(FILE* input) {
  fn_uint size = deserialize_int(input);
  oop result = make_array(size);
  int i;
  for (i=0; i<size; i++) {
    array_set(result, i, deserialize(input));
  }
  return result;
}

static oop deserialize_mem_block(FILE* input, fn_uint size) {
  oop result = mem_raw_mem_alloc(size);
  fn_uint read_bytes = 0;
  read_bytes = fread(result.mem + read_bytes, 1, size - read_bytes, input);
  CHECK(size == read_bytes, "Couldn't read all bytes.");
  return result;
}

static oop deserialize(FILE* input) {
  switch (read_byte(input)) {
  case S_ARRAY:
    return deserialize_array(input);
  case S_CHARACTER:
    return make_char(read_byte(input));
  case S_COMPILED_PROCEDURE:
    {
      oop name = deserialize(input);
      oop lambda_list = deserialize(input);
      oop bytecode = deserialize(input);
      oop ip = deserialize(input);
      oop lookup_table = deserialize(input);
      oop max_stack_depth = deserialize(input);
      proc_t* proc = make_compiled_procedure(lambda_list, NULL /* env */,
					     bytecode, ip, lookup_table,
					     get_smallint(max_stack_depth));
      proc->mutable_name = name;
      return to_oop(proc);
    }
  case S_CONS:
    {
      oop first = deserialize(input);
      return make_cons(first, deserialize(input));
    }
  case S_MEM_BLOCK:
    {
      fn_uint size = deserialize_int(input);
      return deserialize_mem_block(input, size);
    }
  case S_NIL:
    return NIL;
  case S_SMALLINT:
    return make_smallint(deserialize_int(input));
  case S_STRING:
    {
      fn_uint size = deserialize_int(input);
      return make_string_from_mem_block(deserialize_mem_block(input, size),
                                        size);
    }
  case S_SYMBOL:
    {
      // TODO: Don't go over strings, but deserialize symbols directly!
      char* c_str = c_string(deserialize(input));
      oop result = symbol_to_oop(make_symbol(c_str));
      free(c_str);
      return result;
    }
  case S_VAR:
    {
      oop sym = deserialize(input);
      CHECKV(is_symbol(sym), sym, "Expected symbol.");
      return lookup_var_object_globally(to_symbol(sym));
    }
  }
  CHECK(false, "Shouldn't reach end of deserialization function.");
}

void deserialize_from_bootstrap_file(const char* filename) {
  FILE* in = fopen(filename, "r");
  struct stat file_stats;
  int result = fstat(fileno(in), &file_stats);
  CHECK(result == 0, "Can't stat file.");

  while (ftell(in) < file_stats.st_size) {
    apply(make_cons(deserialize(in), NIL));
  }
  fclose(in);
}

FUNC(primitive_deserialize) {
  PARSE_ONE_ARG(file_handle);
  return deserialize((FILE*) get_smallint(file_handle));
}

void init_deserialize() {
  register_globally_fn("c-deserialize!", primitive_deserialize);
}
