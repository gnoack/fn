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

#include <stdio.h>

static const int MAX_BYTES_SIZE = 0x1000;
static const int MAX_OOP_TABLE_SIZE = 0x100;
static const int MAX_LABEL_TABLE_SIZE = 0x800;
static const int MAX_ADDRESS_TABLE_SIZE = 0x800;

struct label_index {
  oop label;
  unsigned int index;
};

struct compiler_result {
  // Emitted bytecode
  unsigned char* bytes;
  unsigned int bytes_size;

  // Emitted oop table
  oop* oop_table;
  unsigned int oop_table_size;

  // Where the jump targets are in the bytecode.
  struct label_index* label_positions;
  unsigned int label_positions_size;

  // Where the addresses for the labels belong in the bytecode.
  struct label_index* address_positions;
  unsigned int address_positions_size;

  // Calculating the maximum stack depth.
  unsigned int stack_depth;
  unsigned int max_stack_depth;
};

struct compiler_result* result;

void init() {
  result = malloc(sizeof(struct compiler_result));
  result->bytes = calloc(sizeof(unsigned char), MAX_BYTES_SIZE);
  result->bytes_size = 0;
  result->oop_table = calloc(sizeof(oop), MAX_OOP_TABLE_SIZE);
  result->oop_table_size = 0;
  result->label_positions = calloc(sizeof(oop), MAX_LABEL_TABLE_SIZE);
  result->label_positions_size = 0;
  result->address_positions = calloc(sizeof(oop), MAX_ADDRESS_TABLE_SIZE);
  result->address_positions_size = 0;
  result->stack_depth = 0;
  result->max_stack_depth = 0;
}

// Return the position of where a jump label is in the code.
unsigned int lookup_label_position(oop label) {
  unsigned int i;
  for (i = 0; i < result->label_positions_size; i++) {
    if (value_eq(label, result->label_positions[i].label)) {
      return result->label_positions[i].index;
    }
  }
  CHECKV(NO, label, "Jump target not found.")
}

void postprocess() {
  unsigned int i;
  for (i = 0; i < result->address_positions_size; i++) {
    unsigned target_position =
        lookup_label_position(result->address_positions[i].label);
    unsigned address_position = result->address_positions[i].index;
    result->bytes[address_position  ] = 0xff & (target_position >> 8);
    result->bytes[address_position+1] = 0xff & target_position;
  }
}

void finish() {
  free(result->bytes);
  free(result->oop_table);
  free(result->label_positions);
  free(result->address_positions);
}

// --------------------------------------------------------------

oop invent_symbol(const char* name) {
  static unsigned counter = 0;
  char* full_name;
  CHECK(asprintf(&full_name, "%s-%d", name, counter) != -1,
        "Could not create new symbol.");
  oop result = make_or_lookup_symbol(full_name);
  free(full_name);
  counter++;
  return result;
}

void emit_byte(unsigned char byte) {
  CHECK(result->bytes_size < MAX_BYTES_SIZE + 1,
        "Byte buffer is too small.");
  result->bytes[result->bytes_size] = byte;
  result->bytes_size++;
}

void emit_oop(oop object) {
  // Check whether object is already in the table.
  unsigned int i;
  for (i=0; i<result->oop_table_size; i++) {
    if (value_eq(object, result->oop_table[i])) {
      CHECK(i <= 0xff, "OOP table too large.");
      emit_byte(0xff & i);
      return;
    }
  }

  // Allocate something in the OOP table.
  unsigned int to_emit = result->oop_table_size;

  CHECK(result->oop_table_size < MAX_OOP_TABLE_SIZE + 1,
        "OOP table buffer is too small.");
  result->oop_table[result->oop_table_size] = object;
  result->oop_table_size++;

  CHECK(to_emit <= 0xff, "OOP table too large.");
  emit_byte(0xff & to_emit);
}

void emit_address(oop label) {
  CHECK(result->address_positions_size < MAX_ADDRESS_TABLE_SIZE + 1,
        "Too many addresses emitted already.");
  struct label_index* tup =
    result->address_positions + result->address_positions_size;
  tup->label = label;
  tup->index = result->bytes_size;
  result->address_positions_size++;

  result->bytes_size += 2;  // Or something.
}

void emit_global_var_ref(oop label) {
  emit_oop(lookup_var_object_globally(label));
}

void emit_label(oop label) {
  CHECK(result->label_positions_size < MAX_LABEL_TABLE_SIZE + 1,
        "Too many labels emitted already.");
  struct label_index* tup =
    result->label_positions + result->label_positions_size;
  tup->label = label;
  tup->index = result->bytes_size;
  result->label_positions_size++;
}

void adjust_stack(int delta) {
  result->stack_depth += delta;
  if (result->max_stack_depth < result->stack_depth) {
    result->max_stack_depth = result->stack_depth;
  }
}

struct stack_depth {
  unsigned int stack_depth;
  unsigned int max_stack_depth;
};

void swap_stack_depth(struct stack_depth* sd) {
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
void JUMP(oop label)                           { emit_byte(0); emit_address(label); }
void JUMP_IF_TRUE(oop label)                   { emit_byte(1); emit_address(label); adjust_stack(-1); }
void LOAD_VALUE(oop value)                     { emit_byte(2); emit_oop(value); adjust_stack(1); }
void READ_VAR(byte f_depth, byte v_index)      { emit_byte(3); emit_byte(f_depth); emit_byte(v_index); adjust_stack(1); }
void WRITE_VAR(byte f_depth, byte v_index)     { emit_byte(4); emit_byte(f_depth); emit_byte(v_index); }
void READ_GLOBAL(oop name)                     { emit_byte(5); emit_global_var_ref(name); adjust_stack(1); }
void WRITE_GLOBAL(oop name)                    { emit_byte(6); emit_global_var_ref(name); }
void DISCARD()                                 { emit_byte(7); adjust_stack(-1); }
void MAKE_LAMBDA(oop label, byte s_sz, oop ll) { emit_byte(8); emit_address(label); emit_byte(s_sz); emit_oop(ll); adjust_stack(1); }
void CALL(byte argnum)                         { emit_byte(9); emit_byte(argnum); adjust_stack(1 - argnum); }
void TAIL_CALL(byte argnum)                    { emit_byte(10); emit_byte(argnum); adjust_stack(1 - argnum); }
void RETURN()                                  { emit_byte(11); adjust_stack(-1); }
void TAIL_CALL_APPLY()                         { emit_byte(12); adjust_stack(1 - 2); }  // unused.
void LABEL(oop label)                          { emit_label(label); }


// --------------------------------------------------------------------

// Forward declaration.
void compile(oop expr, oop env, boolean is_tail);

void compile_literal(oop expr) {
  LOAD_VALUE(expr);
}

void compile_var_access(oop expr, oop env, char is_write) {
  unsigned int frame_index = 0;
  while (!is_nil(env)) {
    oop frame = first(env);

    unsigned int var_index = 0;
    while (!is_nil(frame)) {
      if (value_eq(expr, first(frame))) {
        if (is_write) {
          WRITE_VAR(frame_index, var_index);
        } else {
          READ_VAR(frame_index, var_index);
        }
        return;
      }

      // Next variable.
      frame = rest(frame);
      var_index++;
    }

    // Next frame.
    env = rest(env);
    frame_index++;
  }

  if (is_write) {
    WRITE_GLOBAL(expr);
  } else {
    READ_GLOBAL(expr);
  }
}

void compile_var_read(oop expr, oop env) {
  compile_var_access(expr, env, NO);
}

#define PARSE2(original, n1, n2) \
  original = rest(original);     \
  oop n1 = first(original);      \
  original = rest(original);     \
  oop n2 = first(original);

void compile_set(oop set_expr, oop env) {
  PARSE2(set_expr, name, expr);

  compile(expr, env, NO);
  compile_var_access(name, env, YES);
}

#define PARSE3(original, n1, n2, n3)    \
  original = rest(original);            \
  oop n1 = first(original);             \
  original = rest(original);            \
  oop n2 = first(original);             \
  original = rest(original);            \
  oop n3 = first(original);

void compile_if(oop if_expr, oop env, boolean is_tail) {
  PARSE3(if_expr, cond_expr, conseq_expr, alt_expr);
  oop true_branch = invent_symbol("true-branch");
  oop after_if = invent_symbol("after-if");

  unsigned int stack_depth_before_if = result->stack_depth;
  compile(cond_expr, env, NO);
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

oop append_lists(oop a, oop b) {
  if (is_nil(a)) {
    return b;
  } else {
    CHECKV(is_cons(a), a, "Need a list as first argument.");
    return make_cons(first(a), append_lists(rest(a), b));
  }
}

// Compare with destructuring of lambda lists in procedures.c.
oop vars_from_lambda_list(oop ll) {
  if (is_nil(ll)) {
    return NIL;
  } else if (is_cons(ll)) {
    if (value_eq(symbols._rest, first(ll))) {
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

oop extract_vars(oop let_clauses) {
  if (is_cons(let_clauses)) {
    return make_cons(first(first(let_clauses)),
                     extract_vars(rest(let_clauses)));
  } else {
    return NIL;
  }
}

oop extract_exprs(oop let_clauses) {
  if (is_cons(let_clauses)) {
    return make_cons(first(rest(first(let_clauses))),
                     extract_exprs(rest(let_clauses)));
  } else {
    return NIL;
  }
}

void compile_let(oop let_expr, oop env, boolean is_tail) {
  // Transform to a lambda expression call, then compile.
  let_expr = rest(let_expr);
  oop clauses = first(let_expr);
  oop vars = extract_vars(clauses);
  oop arguments = extract_exprs(clauses);

  oop body = rest(let_expr);
  oop lambda_expr = make_cons(symbols._lambda, make_cons(vars, body));
  oop call_expr = make_cons(lambda_expr, arguments);
  compile(call_expr, env, is_tail);
}

void compile_def(oop def_expr, oop env) {
  PARSE2(def_expr, name, expr);

  compile(expr, env, NO);
  compile_var_access(name, env, YES);
}

void compile_application(oop expr, oop env, boolean is_tail) {
  CHECKV(is_cons(expr), expr, "Need at least a function to call!");

  int length = 0;
  while (is_cons(expr)) {
    compile(first(expr), env, NO);
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

void compile_seq(oop exprs, oop env, boolean is_tail) {
  CHECKV(!is_nil(exprs), exprs, "Empty sequences not supported.");
  while (!is_nil(rest(exprs))) {
    compile(first(exprs), env, NO);
    DISCARD();

    exprs = rest(exprs);
  }
  // Last expression inherits tail call property.
  compile(first(exprs), env, is_tail);
}

void compile_lambda_body(oop exprs, oop env) {
  compile_seq(exprs, env, YES);
  RETURN();
}

void compile_lambda(oop lambda_expr, oop env) {
  lambda_expr = rest(lambda_expr);
  oop lambda_list = first(lambda_expr);
  oop body = rest(lambda_expr);
  oop lambda_entry = invent_symbol("lambda-entry");
  oop after_lambda_body = invent_symbol("after-lambda-body");

  JUMP(after_lambda_body);
  LABEL(lambda_entry);

  // Save outer stack depth and reset stack depth to 0.
  struct stack_depth saved;
  saved.stack_depth = 0;
  saved.max_stack_depth = 0;
  swap_stack_depth(&saved);

  compile_lambda_body(body, make_cons(vars_from_lambda_list(lambda_list), env));
  CHECK(result->stack_depth == 0,
        "Stack depth should be just zero after lambda body.");

  swap_stack_depth(&saved);
  LABEL(after_lambda_body);

  MAKE_LAMBDA(lambda_entry, saved.max_stack_depth, lambda_list);
}

void compile(oop expr, oop env, boolean is_tail) {
  if      (is_smallint(expr)) { compile_literal(expr); }
  else if (is_char(expr))     { compile_literal(expr); }
  else if (is_symbol(expr))   { compile_var_read(expr, env); }
  else if (is_string(expr))   { compile_literal(expr); }
  else if (is_cons(expr)) {
    oop head = first(expr);
    if        (value_eq(head, symbols._quote)) {      compile_literal(first(rest(expr)));
    } else if (value_eq(head, symbols._if)) {  compile_if(expr, env, is_tail);
    } else if (value_eq(head, symbols._lambda)) { compile_lambda(expr, env);
    } else if (value_eq(head, symbols._let)) { compile_let(expr, env, is_tail);
    } else if (value_eq(head, symbols._def)) { compile_def(expr, env);
    } else if (value_eq(head, symbols._set)) { compile_set(expr, env);
    } else if (value_eq(head, symbols._progn)) { compile_seq(rest(expr), env, is_tail);
    } else {
      compile_application(expr, env, is_tail);
    }
  } else {
    // TODO: Error
  }
}

proc_t* compile_top_level_expression(oop expr) {
  init();
  compile_lambda_body(make_cons(expr, NIL), NIL);
  postprocess();

  oop lambda_list = NIL;
  oop bytecode = mem_raw_mem_make(result->bytes, result->bytes_size);
  oop ip = make_smallint(0);

  oop oop_lookup_table = make_array(result->oop_table_size);
  unsigned int i;
  for (i = 0; i < result->oop_table_size; i++) {
    array_set(oop_lookup_table, i, result->oop_table[i]);
  }

  fn_uint max_stack_size = result->max_stack_depth;

  finish();

  return make_compiled_procedure(lambda_list, NULL /* env */,
				 bytecode, ip, oop_lookup_table,
				 max_stack_size);
}
