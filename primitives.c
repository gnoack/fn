
#include <stdio.h>
#include <time.h>

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
void check_argument_number(oop args, int expected) {
  CHECKV(length_int(args) == expected, args, "Argument number");
}

oop primitive_first(oop args) {
  PARSE_ONE_ARG(cons_cell);
  return first(cons_cell);
}

oop primitive_rest(oop args) {
  PARSE_ONE_ARG(cons_cell);
  return rest(cons_cell);
}

/* UNSAFE */
oop primitive_mem_make(oop args) {
  fn_uint size = length_int(args);
  oop result = mem_alloc(size);
  fn_uint i;
  for (i = 0; i < size; i++) {
    mem_set(result, i, first(args));
    args = rest(args);
  }
  return result;
}

/* UNSAFE, except for index == 0. */
oop primitive_mem_get(oop args) {
  PARSE_TWO_ARGS(obj, index);
  return mem_get(obj, get_smallint(index));
}

/* UNSAFE, except for index == 0.
 * This is an imperative function.  Avoid.
 */
oop primitive_mem_set(oop args) {
  PARSE_THREE_ARGS(obj, index, value);
  return mem_set(obj, get_smallint(index), value);
}

oop primitive_char_to_num(oop args) {
  PARSE_ONE_ARG(c);
  CHECKV(is_char(c), c, "Must be a char");
  return make_smallint(get_char(c));
}

oop primitive_num_to_char(oop args) {
  PARSE_ONE_ARG(i);
  CHECKNUMBER(i);
  return make_char(get_smallint(i));
}

oop primitive_string_to_symbol(oop args) {
  PARSE_ONE_ARG(str);
  CHECKV(is_string(str), str, "Must be a string");
  char* c_str = c_string(str);
  oop result = make_symbol(c_str);
  free(c_str);
  return result;
}

oop primitive_symbol_to_string(oop args) {
  PARSE_ONE_ARG(sym);
  CHECKV(is_symbol(sym), sym, "Must be a symbol");
  return make_string(get_symbol(sym));
}

// Integer addition.
oop primitive_add(oop args) {
  // TODO: Pretty inaccurate. This works on smallints only.
  fn_uint i = 0;
  for (; !is_nil(args); args = cdr(args)) {
    oop arg = car(args);
    CHECKV(is_smallint(arg), arg, "Only smallints can be added for now.");
    i += get_smallint(arg);
  }
  return make_smallint(i);
}

// TODO: This may crash, because we don't support negative numbers.
oop primitive_sub(oop args) {
  // TODO: Allow more arguments.
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) - get_smallint(b));
}

oop primitive_mul(oop args) {
  // TODO: Allow more arguments.
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) * get_smallint(b));
}

oop primitive_div(oop args) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) / get_smallint(b));
}

oop primitive_mod(oop args) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return make_smallint(get_smallint(a) % get_smallint(b));
}

// Helper function for predicate primitives.
oop lisp_bool(boolean b) {
  return b ? symbols._true : symbols._false;
}

oop primitive_le(oop args) {
  PARSE_TWO_ARGS(a, b);
  CHECKNUMBER(a);
  CHECKNUMBER(b);
  return lisp_bool(get_smallint(a) <= get_smallint(b));
}

oop primitive_eq(oop args) {
  PARSE_TWO_ARGS(a, b);
  return lisp_bool(value_eq(a, b));
}

UNARY_PREDICATE(primitive_mem_p, is_mem);
UNARY_PREDICATE(primitive_char_p, is_char);
UNARY_PREDICATE(primitive_number_p, is_smallint);
UNARY_PREDICATE(primitive_symbol_p, is_symbol);
UNARY_PREDICATE(primitive_mem_block_p, is_primitive_mem);
UNARY_PREDICATE(primitive_global_env_p, is_global_env);

oop primitive_list(oop args) {
  // Any argument number accepted, of course. :)
  return args;
}

oop primitive_apply(oop args) {
  PARSE_TWO_ARGS(procedure, arguments);
  return apply(make_cons(procedure, arguments));
}

/* Attention: has side effect of printing, returns argument. */
oop primitive_write_out(oop args) {
  PARSE_ONE_ARG(str);
  CHECKV(is_string(str), str, "Must be a string.");
  char* c_str = c_string(str);
  printf("%s", c_str);
  free(c_str);
  return car(args);
}

oop primitive_kill_lisp(oop args) {
  PARSE_ONE_ARG(exit_status);
  CHECKNUMBER(exit_status);
  CHECK(1==0, "Gaa, Lisp was killed!");
  exit(get_smallint(exit_status));
}

oop primitive_primitive_mem_alloc(oop args) {
  PARSE_ONE_ARG(size);
  CHECKNUMBER(size);
  return mem_primitive_mem_alloc(get_smallint(size));
}

oop primitive_primitive_mem_get(oop args) {
  PARSE_TWO_ARGS(target, index);
  CHECKNUMBER(index);
  return mem_primitive_mem_get(target, get_smallint(index));
}

oop primitive_primitive_mem_set(oop args) {
  PARSE_THREE_ARGS(target, index, value);
  CHECKNUMBER(index);
  CHECKNUMBER(value);
  mem_primitive_mem_set(target, get_smallint(index), get_smallint(value));
  return value;
}

oop primitive_run_gc(oop args) {
  CHECKV(is_nil(args), args, "No arguments allowed for run-gc.");
  run_gc_soon();
  return NIL;
}

oop primitive_make_compiled_procedure(oop args) {
  PARSE_FOUR_ARGS(name, lambda_list, code, in_frame);
  oop result = make_compiled_procedure(lambda_list, code, in_frame);
  procedure_set_name(result, name);
  return result;
}

oop primitive_get_process_time(oop args) {
  CHECKV(is_nil(args), args, "No arguments allowed for get-time.");
  struct timespec time;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &time);
  return make_smallint(time.tv_sec * 1000 + time.tv_nsec / 1000000);
}

void init_primitives() {
  register_globally_fn("first", primitive_first);
  register_globally_fn("rest", primitive_rest);
  register_globally_fn("$make", primitive_mem_make);
  register_globally_fn("$mem-get", primitive_mem_get);
  register_globally_fn("$mem-set!", primitive_mem_set);
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
  register_globally_fn("eq", primitive_eq);
  register_globally_fn("mem?", primitive_mem_p);
  register_globally_fn("char?", primitive_char_p);
  register_globally_fn("number?", primitive_number_p);
  register_globally_fn("symbol?", primitive_symbol_p);
  register_globally_fn("list", primitive_list);
  register_globally_fn("apply", primitive_apply);
  register_globally_fn("writeout", primitive_write_out);
  register_globally_fn("kill-lisp", primitive_kill_lisp);
  register_globally_fn("$make-mem-block", primitive_primitive_mem_alloc);
  register_globally_fn("mem-block?", primitive_mem_block_p);
  register_globally_fn("global-env?", primitive_global_env_p);
  register_globally_fn("$mem-block-byte-get", primitive_primitive_mem_get);
  register_globally_fn("$mem-block-byte-set!", primitive_primitive_mem_set);
  register_globally_fn("run-gc", primitive_run_gc);
  register_globally_fn("make-compiled-procedure",
                       primitive_make_compiled_procedure);
  register_globally_fn("get-time", primitive_get_process_time);
}
