
#ifndef __SYMBOLS_H__

typedef struct {
  oop _if;
  oop _lambda;
  oop _let;
  oop _true;
  oop _false;
  oop _native_procedure_marker;
  oop _lisp_procedure_marker;
} symbols_t;

extern symbols_t symbols;

extern void init_symbols();

#define __SYMBOLS_H__ 0
#endif // __SYMBOLS_H__
