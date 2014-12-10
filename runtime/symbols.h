#ifndef _SYMBOLS_H_
#define _SYMBOLS_H_

typedef struct {
  symbol_t* _if;
  symbol_t* _def;
  symbol_t* _lambda;
  symbol_t* _let;
  oop _true;  // not a symbol
  oop _false;  // not a symbol
  symbol_t* _progn;
  symbol_t* _quote;
  symbol_t* _rest;
  symbol_t* _macroexpand;
  symbol_t* _set;

  // Built-in types.
  oop _array;
  oop _cons;
  oop _dict;
  oop _frame;
  oop _stack;
  oop _string;
  oop _symbol;
  oop _defined_var;
  oop _undefined_var;

  // Boolean types.
  oop _True;
  oop _False;

  // Procedure types.
  oop _native_procedure;
  oop _compiled_procedure;

  // C types.
  symbol_t* _c_int;
  symbol_t* _c_str;
  symbol_t* _c_void;
} symbols_t;

extern symbols_t symbols;

typedef struct symbol {
  oop type;
  oop raw_mem;
  oop hash;
} symbol_t;

extern void symbol_hashmap_clear();
extern void symbol_hashmap_register(oop symbol);

extern void init_symbols();

extern symbol_t* make_or_lookup_symbol(const char* str);

#endif // _SYMBOLS_H_
