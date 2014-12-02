#include "vars.h"

#include "debug.h"
#include "memory.h"
#include "symbols.h"
#include "procedures.h"

typedef struct {
  oop type;
  oop symbol;
  oop value;
} var_t;

var_t* to_var(oop o) { return (var_t*) o.mem; }
oop to_oop(var_t* v) { return *((oop*) &v); }

oop make_undefined_var(oop symbol) {
  var_t* var = to_var(mem_alloc(sizeof(var_t) / sizeof(oop)));
  var->type = symbols._undefined_var;
  var->symbol = symbol;
  var->value = NIL;
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
    procedure_set_name(value, v->symbol);
  }
}

void var_unset(oop var) {
  var_t* v = to_var(var);
  v->type = symbols._undefined_var;
  v->value = NIL;
}

oop var_get(oop var) {
  CHECKV(is_set_var(var), var, "Undefined variable.");
  return to_var(var)->value;
}

oop var_name(oop var) {
  return to_var(var)->symbol;
}

void print_var(oop var) {
  char* type = is_set_var(var) ? "defined-var" : "undefined-var";
  printf("<%s %s>", type, get_symbol(to_var(var)->symbol));
}
