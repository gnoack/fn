#ifndef _SYMBOLS_H_
#define _SYMBOLS_H_

typedef struct {
  oop _if;
  oop _def;
  oop _lambda;
  oop _let;
  oop _true;
  oop _false;
  oop _progn;
  oop _quote;
  oop _rest;
  oop _macroexpand;
  oop _set;

  // Built-in types.
  oop _array;
  oop _cons;
  oop _continuation;
  oop _dframe;
  oop _dict;
  oop _frame;
  oop _retptr;
  oop _string;
  oop _symbol;

  // Procedure types.
  oop _procedure;
  oop _native_procedure;
  oop _compiled_procedure;

  // C types.
  oop _c_int;
  oop _c_str;
  oop _c_void;
} symbols_t;

extern symbols_t symbols;

extern void symbol_hashmap_clear();
extern void symbol_hashmap_register(oop symbol);

extern void init_symbols();

extern oop make_or_lookup_symbol(const char* str);

#endif // _SYMBOLS_H_
