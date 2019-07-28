#define _GNU_SOURCE  // For asprintf in stdio.h. oO

#include "native-compiler.h"

#include "data.h"
#include "debug.h"
#include "eval.h"
#include "cons.h"
#include "strings.h"
#include "symbols.h"
#include "value.h"
#include "memory.h"
#include "c_array.h"

#include <stdio.h>

struct label_index {
  symbol_t* label;
  unsigned int index;
};

DEFARRAY(byte_array, unsigned char)
DEFARRAY(oop_array, oop)
DEFARRAY(label_index_array, struct label_index)

struct compiler_result {
  // Emitted bytecode
  struct byte_array bytes;

  // Emitted oop table
  struct oop_array oop_table;

  // Where the jump targets are in the bytecode.
  struct label_index_array label_positions;

  // Where the addresses for the labels belong in the bytecode.
  struct label_index_array address_positions;

  // Calculating the maximum stack depth.
  unsigned int stack_depth;
  unsigned int max_stack_depth;
};

// global compiler state
struct compiler_result* result;

static void init() {
  result = malloc(sizeof(struct compiler_result));
  init_byte_array(&result->bytes, 1024);
  init_oop_array(&result->oop_table, 32);
  init_label_index_array(&result->label_positions, 16);
  init_label_index_array(&result->address_positions, 16);
  result->stack_depth = 0;
  result->max_stack_depth = 0;
}

// Return the position of where a jump label is in the code.
static unsigned int lookup_label_position(symbol_t* label) {
  struct label_index* item = result->label_positions.items;
  struct label_index* end =  label_index_array_end(&result->label_positions);
  for (; item < end; item++) {
    if (item->label == label) {
      return item->index;
    }
  }
  FATALV(symbol_to_oop(label), "Jump target not found.")
}

static void postprocess() {
  struct label_index* item = result->address_positions.items;
  struct label_index* end =  label_index_array_end(&result->address_positions);
  for (; item < end; item++) {
    unsigned int target_position = lookup_label_position(item->label);
    unsigned int addr_position = item->index;
    *byte_array_at(&result->bytes, addr_position) = 0xff & (target_position >> 8);
    *byte_array_at(&result->bytes, addr_position+1) = 0xff & target_position;
  }
}

static void finish() {
  free_byte_array(&result->bytes);
  free_oop_array(&result->oop_table);
  free_label_index_array(&result->label_positions);
  free_label_index_array(&result->address_positions);
}

// --------------------------------------------------------------

static symbol_t* invent_symbol(const char* name) {
  static unsigned counter = 0;
  char* full_name;
  CHECK(asprintf(&full_name, "%s-%d", name, counter) != -1,
        "Could not create new symbol.");
  symbol_t* result = make_symbol(full_name);
  free(full_name);
  counter++;
  return result;
}

static void emit_byte(unsigned char byte) {
  *byte_array_append(&result->bytes) = byte;
}

static void emit_oop(oop object) {
  // Check whether object is already in the table.
  int idx = oop_array_find(&result->oop_table, object, value_eq);
  if (idx == -1) {
    // Allocate something in the OOP table.
    idx = result->oop_table.size;
    *oop_array_append(&result->oop_table) = object;
  }

  CHECK(idx <= 0xff, "OOP table too large.");
  emit_byte(0xff & idx);
}

static void emit_address(symbol_t* label) {
  *label_index_array_append(&result->address_positions) = (struct label_index) {
    .label = label,
    .index = result->bytes.size
  };

  byte_array_append(&result->bytes);
  byte_array_append(&result->bytes);
}

static void emit_global_var_ref(symbol_t* label) {
  emit_oop(lookup_var_object_globally(label));
}

static void emit_label(symbol_t* label) {
  *label_index_array_append(&result->label_positions) = (struct label_index) {
    .label = label,
    .index = result->bytes.size
  };
}

static void adjust_stack(int delta) {
  result->stack_depth += delta;
  if (result->max_stack_depth < result->stack_depth) {
    result->max_stack_depth = result->stack_depth;
  }
}

struct stack_depth {
  unsigned int stack_depth;
  unsigned int max_stack_depth;
};

static void swap_stack_depth(struct stack_depth* sd) {
  unsigned int tmp;

  tmp = sd->stack_depth;
  sd->stack_depth = result->stack_depth;
  result->stack_depth = tmp;

  tmp = sd->max_stack_depth;
  sd->max_stack_depth = result->max_stack_depth;
  result->max_stack_depth = tmp;
}

// ------------------------------------------------------------------------

typedef unsigned char byte;

/*
 * These are emitting bytecode instructions on a slightly higher level.
 */
static void JUMP(symbol_t* label)                           { emit_byte(0); emit_address(label); }
static void JUMP_IF_TRUE(symbol_t* label)                   { emit_byte(1); emit_address(label);                                adjust_stack(-1); }
static void LOAD_VALUE(oop value)                           { emit_byte(2); emit_oop(value);                                    adjust_stack(1); }
static void READ_VAR(byte f_depth, byte v_index)            { emit_byte(3); emit_byte(f_depth); emit_byte(v_index);             adjust_stack(1); }
static void WRITE_VAR(byte f_depth, byte v_index)           { emit_byte(4); emit_byte(f_depth); emit_byte(v_index); }
static void READ_GLOBAL(symbol_t* name)                     { emit_byte(5); emit_global_var_ref(name);                          adjust_stack(1); }
static void WRITE_GLOBAL(symbol_t* name)                    { emit_byte(6); emit_global_var_ref(name); }
static void DISCARD()                                       { emit_byte(7);                                                     adjust_stack(-1); }
static void MAKE_LAMBDA(symbol_t* label, byte s_sz, oop ll) { emit_byte(8); emit_address(label); emit_byte(s_sz); emit_oop(ll); adjust_stack(1); }
static void CALL(byte argnum)                               { emit_byte(9); emit_byte(argnum);                                  adjust_stack(1 - argnum); }
static void TAIL_CALL(byte argnum)                          { emit_byte(10); emit_byte(argnum);                                 adjust_stack(1 - argnum); }
static void RETURN()                                        { emit_byte(11);                                                    adjust_stack(-1); }
static void TAIL_CALL_APPLY()                               { emit_byte(12);                                                    adjust_stack(1 - 2); }  // unused.
static void READ_FIELD(byte index)                          { emit_byte(13); emit_byte(index); }
static void WRITE_FIELD(byte index)                         { emit_byte(14); emit_byte(index);                                  adjust_stack(-1); }
static void LABEL(symbol_t* label)                          { emit_label(label); }

// --------------------------------------------------------------------

typedef struct env env_t;

typedef struct env {
  void (*compile_var_access)(env_t* self, int frame_depth,
                             symbol_t* name, char is_write);
  env_t* next;
  oop vars;
} env_t;

// Forward declaration.
static void compile(oop expr, env_t* env, bool is_tail);

static void compile_literal(oop expr) {
  LOAD_VALUE(expr);
}

static void compile_var_access_global(env_t* self, int frame_depth,
                                      symbol_t* name, char is_write) {
  if (is_write) {
    WRITE_GLOBAL(name);
  } else {
    READ_GLOBAL(name);
  }
}

// Return index of the name in vars, otherwise -1.
static int list_index_of(symbol_t* name, oop vars) {
  int index = 0;
  while (is_cons(vars)) {
    if (value_eq(first(vars), symbol_to_oop(name))) {
      return index;
    }
    index++;
    vars = rest(vars);
  }
  CHECKV(is_nil(vars), vars, "Must be terminated with nil.");
  return -1;
}

static void compile_var_access_frame(env_t* self, int frame_depth,
                                     symbol_t* name, char is_write) {
  int index = list_index_of(name, self->vars);
  if (index == -1) {
    self->next->compile_var_access(self->next, frame_depth+1, name, is_write);
  } else {
    if (is_write) {
      WRITE_VAR(frame_depth, index);
    } else {
      READ_VAR(frame_depth, index);
    }
  }
}

static void compile_var_access(oop expr, env_t* env, char is_write) {
  CHECKV(is_symbol(expr), expr, "Variable needs to be denoted as symbol.");
  env->compile_var_access(env, 0, to_symbol(expr), is_write);
}

static void compile_var_read(oop expr, env_t* env) {
  compile_var_access(expr, env, false);
}

#define PARSE2(original_expr, n1, n2) \
  oop original = rest(original_expr); \
  oop n1 = first(original);           \
  original = rest(original);          \
  oop n2 = first(original);

static void compile_set(oop set_expr, env_t* env) {
  PARSE2(set_expr, name, expr);

  compile(expr, env, false);
  compile_var_access(name, env, true);
}

#define PARSE3(original_expr, n1, n2, n3)       \
  oop original = rest(original_expr);           \
  oop n1 = first(original);                     \
  original = rest(original);                    \
  oop n2 = first(original);                     \
  original = rest(original);                    \
  oop n3 = first(original);

static void compile_if(oop if_expr, env_t* env, bool is_tail) {
  PARSE3(if_expr, cond_expr, conseq_expr, alt_expr);
  symbol_t* true_branch = invent_symbol("true-branch");
  symbol_t* after_if = invent_symbol("after-if");

  unsigned int stack_depth_before_if = result->stack_depth;
  compile(cond_expr, env, false);
  JUMP_IF_TRUE(true_branch);
  CHECK(stack_depth_before_if == result->stack_depth,
        "Stack size corrupted after condition.");
  compile(alt_expr, env, is_tail);
  JUMP(after_if);
  adjust_stack(-1);

  LABEL(true_branch);
  CHECK(stack_depth_before_if == result->stack_depth,
        "Stack size corrupted after condition.");
  compile(conseq_expr, env, is_tail);
  LABEL(after_if);

  CHECK(stack_depth_before_if + 1 == result->stack_depth,
        "Stack size corrupted after condition.");

}

static oop append_lists(oop a, oop b) {
  if (is_nil(a)) {
    return b;
  } else {
    CHECKV(is_cons(a), a, "Need a list as first argument.");
    return make_cons(first(a), append_lists(rest(a), b));
  }
}

// Compare with destructuring of lambda lists in procedures.c.
static oop vars_from_lambda_list(oop ll) {
  if (is_nil(ll)) {
    return NIL;
  } else if (is_cons(ll)) {
    if (value_eq(first(ll), symbol_to_oop(symbols._rest))) {
      return vars_from_lambda_list(rest(ll));
    } else {
      return append_lists(vars_from_lambda_list(first(ll)),
                          vars_from_lambda_list(rest(ll)));
    }
  } else {
    CHECKV(is_symbol(ll), ll, "Lambda list may consist of lists and symbols.");
    return make_cons(ll, NIL);
  }
}

static oop extract_vars(oop let_clauses) {
  if (is_cons(let_clauses)) {
    return make_cons(first(first(let_clauses)),
                     extract_vars(rest(let_clauses)));
  } else {
    return NIL;
  }
}

static oop extract_exprs(oop let_clauses) {
  if (is_cons(let_clauses)) {
    return make_cons(first(rest(first(let_clauses))),
                     extract_exprs(rest(let_clauses)));
  } else {
    return NIL;
  }
}

static void compile_let(oop let_expr, env_t* env, bool is_tail) {
  // Transform to a lambda expression call, then compile.
  let_expr = rest(let_expr);
  oop clauses = first(let_expr);
  oop vars = extract_vars(clauses);
  oop arguments = extract_exprs(clauses);

  oop body = rest(let_expr);
  oop lambda_expr = make_cons(symbol_to_oop(symbols._lambda),
                              make_cons(vars, body));
  oop call_expr = make_cons(lambda_expr, arguments);
  compile(call_expr, env, is_tail);
}

static void compile_def(oop def_expr, env_t* env) {
  PARSE2(def_expr, name, expr);

  compile(expr, env, false);
  compile_var_access(name, env, true);
}

static void compile_application(oop expr, env_t* env, bool is_tail) {
  CHECKV(is_cons(expr), expr, "Need at least a function to call!");

  int length = 0;
  while (is_cons(expr)) {
    compile(first(expr), env, false);
    length++;

    expr = rest(expr);
  }
  CHECK(is_nil(expr), "Function call needs to be a list.");

  CHECK(length <= 0xff, "Too many arguments in a function call!");
  if (is_tail) {
    TAIL_CALL(length);
  } else {
    CALL(length);
  }
}

static void compile_seq(oop exprs, env_t* env, bool is_tail) {
  CHECKV(!is_nil(exprs), exprs, "Empty sequences not supported.");
  while (!is_nil(rest(exprs))) {
    compile(first(exprs), env, false);
    DISCARD();

    exprs = rest(exprs);
  }
  // Last expression inherits tail call property.
  compile(first(exprs), env, is_tail);
}

static void compile_lambda_body(oop exprs, env_t* env) {
  compile_seq(exprs, env, true);
  RETURN();
}

static void compile_lambda(oop lambda_expr, env_t* env) {
  lambda_expr = rest(lambda_expr);
  oop lambda_list = first(lambda_expr);
  oop body = rest(lambda_expr);
  symbol_t* lambda_entry = invent_symbol("lambda-entry");
  symbol_t* after_lambda_body = invent_symbol("after-lambda-body");

  JUMP(after_lambda_body);
  LABEL(lambda_entry);

  // Save outer stack depth and reset stack depth to 0.
  struct stack_depth saved;
  saved.stack_depth = 0;
  saved.max_stack_depth = 0;
  swap_stack_depth(&saved);

  env_t* inner_env = calloc(1, sizeof(env_t));
  *inner_env = (env_t) {
    .compile_var_access = compile_var_access_frame,
    .next = env,
    .vars = vars_from_lambda_list(lambda_list)
  };
  compile_lambda_body(body, inner_env);
  free(inner_env);
  CHECK(result->stack_depth == 0,
        "Stack depth should be just zero after lambda body.");

  swap_stack_depth(&saved);
  LABEL(after_lambda_body);

  MAKE_LAMBDA(lambda_entry, saved.max_stack_depth, lambda_list);
}

static void compile_mem_get(oop mem_get_expr, env_t* env, bool is_tail) {
  PARSE2(mem_get_expr, expr, index);
  if (is_smallint(index)) {
    compile(expr, env, false);
    READ_FIELD(get_smallint(index));
  } else {
    compile_application(mem_get_expr, env, is_tail);
  }
}

static void compile_mem_set(oop mem_set_expr, env_t* env, bool is_tail) {
  PARSE3(mem_set_expr, expr, index, value);
  if (is_smallint(index)) {
    // TODO: Would be better to calculate expr first, and then value!
    compile(value, env, false);
    compile(expr, env, false);
    WRITE_FIELD(get_smallint(index));
  } else {
    compile_application(mem_set_expr, env, is_tail);
  }
}

static void compile_var_access_fields(env_t* self, int frame_depth,
                                      symbol_t* name, char is_write) {
  int index = list_index_of(name, rest(self->vars));
  if (index == -1) {
    self->next->compile_var_access(self->next, frame_depth, name, is_write);
  } else {
    symbol_t* objname = to_symbol(first(self->vars));
    self->next->compile_var_access(self->next, frame_depth, objname, false);
    if (is_write) {
      WRITE_FIELD(index);
    } else {
      READ_FIELD(index);
    }
  }
}

static void compile_fields(oop fields_expr, env_t* env, bool is_tail) {
  // ($fields (objname (field1 field2 ...)) body...)
  fields_expr = rest(fields_expr);
  oop objname = first(first(fields_expr));
  oop fieldnames = first(rest(first(fields_expr)));
  oop body = rest(fields_expr);
  env_t* inner_env = calloc(1, sizeof(env_t));
  *inner_env = (env_t) {
    .compile_var_access = compile_var_access_fields,
    .next = env,
    .vars = make_cons(objname, fieldnames)  // TODO: Not very nice.
  };
  compile_seq(body, inner_env, is_tail);
  free(inner_env);
}

static void compile(oop expr, env_t* env, bool is_tail) {
  if      (is_smallint(expr)) { compile_literal(expr); }
  else if (is_char(expr))     { compile_literal(expr); }
  else if (is_symbol(expr))   { compile_var_read(expr, env); }
  else if (is_string(expr))   { compile_literal(expr); }
  else if (is_cons(expr)) {
    if (is_symbol(first(expr))) {
      symbol_t* head = to_symbol(first(expr));
      if        (head == symbols._quote) { compile_literal(first(rest(expr)));
      } else if (head == symbols._if) { compile_if(expr, env, is_tail);
      } else if (head == symbols._lambda) { compile_lambda(expr, env);
      } else if (head == symbols._let) { compile_let(expr, env, is_tail);
      } else if (head == symbols._def) { compile_def(expr, env);
      } else if (head == symbols._set) { compile_set(expr, env);
      } else if (head == symbols._progn) { compile_seq(rest(expr), env, is_tail);
      } else if (head == symbols._mem_get) { compile_mem_get(expr, env, is_tail);
      } else if (head == symbols._mem_set) { compile_mem_set(expr, env, is_tail);
      } else if (head == symbols._fields) { compile_fields(expr, env, is_tail);
      } else {
        compile_application(expr, env, is_tail);
      }
    } else {
      compile_application(expr, env, is_tail);
    }
  } else {
    FATALV(expr, "Unknown expression type.");
  }
}

proc_t* compile_top_level_expression(oop expr) {
  init();

  env_t* global_env = calloc(1, sizeof(env_t));
  *global_env = (env_t) {
    .compile_var_access = compile_var_access_global,
    .next = NULL,
    .vars = NIL
  };
  compile_lambda_body(make_cons(expr, NIL), global_env);
  free(global_env);

  postprocess();

  oop lambda_list = NIL;
  oop bytecode = mem_raw_mem_make(result->bytes.items, result->bytes.size);
  oop ip = make_smallint(0);

  oop oop_lookup_table = make_array(result->oop_table.size);
  unsigned int i;
  for (i = 0; i < result->oop_table.size; i++) {
    array_set(oop_lookup_table, i, result->oop_table.items[i]);
  }

  fn_uint max_stack_size = result->max_stack_depth;

  finish();

  return make_compiled_procedure(lambda_list, NULL /* global env */,
                                 bytecode, ip, oop_lookup_table,
                                 max_stack_size);
}
