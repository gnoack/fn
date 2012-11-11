
#ifndef __SYMBOLS_H__

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

  oop _procedure;
  oop _native_procedure;
  oop _compiled_procedure;
} symbols_t;

extern symbols_t symbols;

extern void symbol_hashmap_clear();
extern void symbol_hashmap_register(oop symbol);

extern void init_symbols();

extern oop make_or_lookup_symbol(const char* str);

#define __SYMBOLS_H__ 0
#endif // __SYMBOLS_H__
