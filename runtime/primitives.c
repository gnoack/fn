
#include <stdio.h>
#include <string.h>  // memcpy, memcmp
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "carcdr.h"
#include "cons.h"
#include "debug.h"
#include "eval.h"
#include "gc.h"
#include "memory.h"
#include "primitives.h"
#include "strings.h"
#include "symbols.h"

/* Utility. */
FUNC(primitive_first) {
  PARSE_ONE_ARG(cons_cell);
  return first(cons_cell);
}

FUNC(primitive_rest) {
  PARSE_ONE_ARG(cons_cell);
  return rest(cons_cell);
}

/* UNSAFE */
FUNC(primitive_mem_make) {
  oop result = mem_alloc(argc);
  fn_uint i;
  for (i = 0; i < argc; i++) {
    MEM_SET(result, i, argv[i]);
  }
  return result;
}

/* UNSAFE, except for index == 0. */
FUNC(primitive_mem_get) {
  PARSE_TWO_ARGS(obj, index);
  return mem_get(obj, get_smallint(index));
}

/* UNSAFE, except for index == 0.
 * This is an imperative function.  Avoid.
 */
FUNC(primitive_mem_set) {
  PARSE_THREE_ARGS(obj, index, value);
  return mem_set(obj, get_smallint(index), value);
}

FUNC(primitive_mem_size) {
  PARSE_ONE_ARG(obj);
  return make_smallint(mem_size(obj));
}

FUNC(primitive_char_to_num) {
  PARSE_ONE_ARG(c);
  CHECKV(is_char(c), c, "Must be a char");
  return make_smallint(get_char(c));
}

FUNC(primitive_num_to_char) {
  PARSE_ONE_ARG(i);
  CHECKNUMBER(i);
  return make_char(get_smallint(i));
}

FUNC(primitive_string_to_symbol) {
  PARSE_ONE_ARG(str);
  CHECKV(is_string(str), str, "Must be a string");
  char* c_str = c_string(str);
  oop result = make_symbol(c_str);
  free(c_str);
  return result;
}

FUNC(primitive_symbol_to_string) {
  PARSE_ONE_ARG(sym);
  CHECKV(is_symbol(sym), sym, "Must be a symbol");
  return make_string(get_symbol(sym));
}

// Integer addition.
oop primitive_add(oop* argv, size_t argc) {
  // TODO: Pretty inaccurate. This works on smallints only.
  fn_uint sum = 0;
  size_t i;
  for (i = 0; i < argc; i++) {
    sum += get_smallint(argv[i]);
  }
  return make_smallint(sum);
}

// TODO: This may crash, because we don't support negative numbers.
FUNC(primitive_sub) {
  // TODO: Allow more arguments.
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) - get_smallint(b));
}

FUNC(primitive_mul) {
  // TODO: Allow more arguments.
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) * get_smallint(b));
}

FUNC(primitive_div) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) / get_smallint(b));
}

FUNC(primitive_mod) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) % get_smallint(b));
}

// Helper function for predicate primitives.
oop lisp_bool(boolean b) {
  return b ? symbols._true : symbols._false;
}

FUNC(primitive_le) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return lisp_bool(get_smallint(a) <= get_smallint(b));
}

FUNC(primitive_eq) {
  PARSE_TWO_ARGS(a, b);
  return lisp_bool(value_eq(a, b));
}

UNARY_PREDICATE(primitive_mem_p, is_mem);
UNARY_PREDICATE(primitive_char_p, is_char);
UNARY_PREDICATE(primitive_number_p, is_smallint);
UNARY_PREDICATE(primitive_symbol_p, is_symbol);
UNARY_PREDICATE(primitive_raw_mem_p, is_raw_mem);
UNARY_PREDICATE(primitive_global_env_p, is_global_env);

oop primitive_list(oop* argv, size_t argc) {
  // Any argument number accepted, of course. :)
  oop result = NIL;
  while (argc) {
    result = make_cons(argv[argc-1], result);
    argc--;
  }
  return result;
}

FUNC(primitive_apply) {
  PARSE_TWO_ARGS(procedure, arguments);
  return apply(make_cons(procedure, arguments));
}

/* Attention: has side effect of printing, returns argument. */
FUNC(primitive_write_out) {
  PARSE_ONE_ARG(str);
  CHECKV(is_string(str), str, "Must be a string.");
  char* c_str = c_string(str);
  printf("%s", c_str);
  free(c_str);
  return str;
}

FUNC(primitive_kill_lisp) {
  PARSE_ONE_ARG(exit_status);
  CHECKNUMBER(exit_status);
  CHECK(1==0, "Gaa, Lisp was killed!");
  exit(get_smallint(exit_status));
}

FUNC(primitive_raw_mem_alloc) {
  PARSE_ONE_ARG(size);
  CHECKNUMBER(size);
  return mem_raw_mem_alloc(get_smallint(size));
}

FUNC(primitive_raw_mem_get) {
  PARSE_TWO_ARGS(target, index);
  CHECKNUMBER(index);
  return mem_raw_mem_get(target, get_smallint(index));
}

FUNC(primitive_raw_mem_set) {
  PARSE_THREE_ARGS(target, index, value);
  CHECKNUMBER(index);
  CHECKNUMBER(value);
  mem_raw_mem_set(target, get_smallint(index), get_smallint(value));
  return value;
}

FUNC(primitive_raw_mem_size) {
  PARSE_ONE_ARG(target);
  return make_smallint(mem_raw_mem_size(target));
}

FUNC(primitive_raw_memcpy) {
  PARSE_FIVE_ARGS(trg, trg_offset, src, src_offset, size);
  memcpy(mem_raw_get_ptr(trg) + get_smallint(trg_offset),
         mem_raw_get_ptr(src) + get_smallint(src_offset), get_smallint(size));
  return trg;
}

FUNC(primitive_raw_mem_eq) {
  PARSE_FIVE_ARGS(a, a_offset, b, b_offset, size);
  int result = memcmp(mem_raw_get_ptr(a) + get_smallint(a_offset),
                      mem_raw_get_ptr(b) + get_smallint(b_offset),
                      get_smallint(size));
  return lisp_bool(result == 0);
}

FUNC(primitive_run_gc) {
  CHECK(0 == argc, "No arguments allowed for run-gc.");
  run_gc_soon();
  return NIL;
}

FUNC(primitive_make_compiled_procedure) {
  PARSE_SEVEN_ARGS(name, lambda_list, in_frame, bytecode, ip, lookup_table,
                   max_stack_depth);
  oop result = make_compiled_procedure(lambda_list, in_frame,
                                       bytecode, ip, lookup_table,
                                       get_smallint(max_stack_depth));
  procedure_set_name(result, name);
  return result;
}

FUNC(primitive_fn_max_stack_depth) {
  PARSE_ONE_ARG(fn);
  CHECKV(is_compiled_lisp_procedure(fn), fn, "Needs bytecode procedure.");
  return make_smallint(fn_max_stack_depth(fn));
}

FUNC(primitive_get_process_time) {
  CHECK(argc == 0, "No arguments allowed for get-time.");
  struct timespec time;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &time);
  return make_smallint(time.tv_sec * 1000 + time.tv_nsec / 1000000);
}

FUNC(primitive_file_timestamp) {
  PARSE_ONE_ARG(filename);
  char* c_filename = c_string(filename);
  struct stat c_stat;
  int stat_result = stat(c_filename, &c_stat);
  if (stat_result == -1) {
    return NIL;
  }
  return make_smallint(c_stat.st_mtime);
}

FUNC(primitive_file_size) {
  PARSE_ONE_ARG(filename);
  char* c_filename = c_string(filename);
  struct stat c_stat;
  int stat_result = stat(c_filename, &c_stat);
  if (stat_result == -1) {
    return NIL;
  }
  return make_smallint(c_stat.st_size);
}

FUNC(primitive_fopen) {
  PARSE_TWO_ARGS(filename, mode);
  const char* c_filename = c_string(filename);
  const char* c_mode = c_string(mode);
  FILE* file = fopen(c_filename, c_mode);
  if (file == NULL) {
    return NIL;
  }
  // TODO: Bad assumptions about memory addresses.
  return make_smallint((fn_uint) file);
}

FUNC(primitive_fclose) {
  PARSE_ONE_ARG(file_handle);
  fclose((FILE*) get_smallint(file_handle));
  return NIL;
}

FUNC(primitive_fread_buf) {
  PARSE_THREE_ARGS(file_handle, buf, size);
  FILE* c_file_handle = (FILE*) get_smallint(file_handle);
  size_t c_size = (size_t) get_smallint(size);
  CHECKV(mem_raw_mem_size(buf) >= c_size, buf,
         "Raw memory block needs to have the required size.");
  void* c_buf = mem_raw_get_ptr(buf);
  size_t result = fread(c_buf, 1, c_size, c_file_handle);
  return make_smallint(result);
}

FUNC(primitive_fwrite_buf) {
  PARSE_THREE_ARGS(file_handle, buf, size);
  FILE* c_file_handle = (FILE*) get_smallint(file_handle);
  size_t c_size = (size_t) get_smallint(size);
  CHECKV(mem_raw_mem_size(buf) >= c_size, buf,
         "Raw memory block needs to have the required size.");
  void* c_buf = mem_raw_get_ptr(buf);
  size_t result = fwrite(c_buf, 1, c_size, c_file_handle);
  return make_smallint(result);
}

FUNC(primitive_get_frame) {
  CHECK(argc == 0, "Assumed no arguments.");
  return native_procedure_caller();
}

FUNC(primitive_lookup_var_object) {
  PARSE_ONE_ARG(symbol);
  return lookup_var_object_globally(symbol);
}

FUNC(primitive_id) {
  PARSE_ONE_ARG(obj);
  return make_smallint((fn_uint) obj.mem >> 2);
}

void init_primitives() {
  register_globally_fn("first", primitive_first);
  register_globally_fn("rest", primitive_rest);
  register_globally_fn("$make", primitive_mem_make);
  register_globally_fn("$mem-get", primitive_mem_get);
  register_globally_fn("$mem-set!", primitive_mem_set);
  register_globally_fn("$mem-size", primitive_mem_size);
  register_globally_fn("char->num", primitive_char_to_num);
  register_globally_fn("num->char", primitive_num_to_char);
  register_globally_fn("string->symbol", primitive_string_to_symbol);
  register_globally_fn("symbol->string", primitive_symbol_to_string);
  register_globally_fn("+", primitive_add);
  register_globally_fn("-", primitive_sub);
  register_globally_fn("*", primitive_mul);
  register_globally_fn("/", primitive_div);
  register_globally_fn("mod", primitive_mod);
  register_globally_fn("integer<=", primitive_le);
  register_globally_fn("eq?", primitive_eq);
  register_globally_fn("mem?", primitive_mem_p);
  register_globally_fn("char?", primitive_char_p);
  register_globally_fn("number?", primitive_number_p);
  register_globally_fn("symbol?", primitive_symbol_p);
  register_globally_fn("global-env?", primitive_global_env_p);
  register_globally_fn("list", primitive_list);
  register_globally_fn("apply", primitive_apply);
  register_globally_fn("writeout", primitive_write_out);
  register_globally_fn("kill-lisp", primitive_kill_lisp);
  register_globally_fn("$make-mem-block", primitive_raw_mem_alloc);
  register_globally_fn("mem-block?", primitive_raw_mem_p);
  register_globally_fn("$mem-block-byte-get", primitive_raw_mem_get);
  register_globally_fn("$mem-block-byte-set!", primitive_raw_mem_set);
  register_globally_fn("$mem-block=?", primitive_raw_mem_eq);
  register_globally_fn("$memcpy", primitive_raw_memcpy);
  register_globally_fn("mem-block-size", primitive_raw_mem_size);
  register_globally_fn("run-gc", primitive_run_gc);
  register_globally_fn("make-compiled-procedure",
                       primitive_make_compiled_procedure);
  register_globally_fn("fn-max-stack-depth", primitive_fn_max_stack_depth);
  register_globally_fn("get-time", primitive_get_process_time);
  register_globally_fn("file-timestamp", primitive_file_timestamp);
  register_globally_fn("file-size", primitive_file_size);
  register_globally_fn("fopen", primitive_fopen);
  register_globally_fn("fclose", primitive_fclose);
  register_globally_fn("fread", primitive_fread_buf);
  register_globally_fn("fwrite", primitive_fwrite_buf);
  register_globally_fn("$get-frame", primitive_get_frame);
  register_globally_fn("$lookup-var-object", primitive_lookup_var_object);
  register_globally_fn("$id", primitive_id);
}
