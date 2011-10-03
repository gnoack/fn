
#ifndef __SYMBOLS_H__

typedef struct {
  oop _if;
  oop _def;
  oop _lambda;
  oop _let;
  oop _true;
  oop _false;
  oop _quote;
  oop _rest;
  oop _macroexpand;
  oop _native_procedure_marker;
  oop _lisp_procedure_marker;
  oop _set;
  oop _cons;

  // Bytecode.
  oop _bc_halt;
  oop _bc_jump;
  oop _bc_jump_if_true;
  oop _bc_load_value;
  oop _bc_load_var;
  oop _bc_write_var;
  oop _bc_load_global_var;
  oop _bc_write_global_var;
  oop _bc_push;
  oop _bc_pop;
  oop _bc_make_lambda;
  oop _bc_call;
  oop _bc_return;

} symbols_t;

extern symbols_t symbols;

extern void init_symbols();

#define __SYMBOLS_H__ 0
#endif // __SYMBOLS_H__
