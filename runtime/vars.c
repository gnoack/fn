#include "vars.h"

#include "debug.h"
#include "memory.h"
#include "symbols.h"
#include "procedures.h"

typedef struct {
  oop type;
  symbol_t* symbol;
  oop value;
} var_t;

static inline var_t* to_var(oop o) { return (var_t*) o.mem; }

oop make_undefined_var(symbol_t* symbol) {
  var_t* var = MemAlloc(var_t);
  *var = (var_t) {
    .type   = symbols._undefined_var,
    .symbol = symbol,
    .value  = NIL
  };
  return to_oop(var);
}

boolean is_var(oop var) {
  return value_eq(to_var(var)->type, symbols._defined_var)
    || value_eq(to_var(var)->type, symbols._undefined_var);
}

boolean is_set_var(oop var) {
  return value_eq(to_var(var)->type, symbols._defined_var);
}

void var_set(oop var, oop value) {
  var_t* v = to_var(var);
  v->type = symbols._defined_var;
  v->value = value;

  // If it's a procedure, set its name.
  if (is_procedure(value)) {
    procedure_set_name(value, symbol_to_oop(v->symbol));
  }
}

void var_unset(oop var) {
  var_t* v = to_var(var);
  v->type = symbols._undefined_var;
  v->value = NIL;
}

oop var_get(oop var) {
  CHECKV(is_set_var(var), var, "Undefined variable.");
  return var_get_unchecked(var);
}

oop var_get_unchecked(oop var) {
  // The caller guarantees that the variable is defined.
  return to_var(var)->value;
}

symbol_t* var_name(oop var) {
  return to_var(var)->symbol;
}

void print_var(oop var) {
  char* type = is_set_var(var) ? "defined-var" : "undefined-var";
  printf("<%s %s>", type, get_symbol(to_var(var)->symbol));
}
