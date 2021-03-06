#include <stdio.h>

#include "carcdr.h"
#include "cons.h"
#include "data.h"
#include "debug.h"
#include "eval.h"
#include "gc.h"
#include "native-compiler.h"
#include "procedures.h"
#include "strings.h"
#include "symbols.h"
#include "value.h"
#include "vars.h"

// Definition.
oop global_env;

static oop remaining_declarations;

static void set_globally_oop(symbol_t* key, oop value) {
  var_set(lookup_var_object_globally(key), value);
}

static bool is_defined_globally(symbol_t* key) {
  return dict_has_key(global_env, symbol_to_oop(key)) &&
         is_set_var(dict_get(global_env, symbol_to_oop(key)));
}

// Like the above, but also check for redefinitions.
static void register_globally_symbol(symbol_t* key, oop value) {
  CHECKV(!is_defined_globally(key), symbol_to_oop(key),
	 "Symbol already defined.");
  set_globally_oop(key, value);
}

bool is_global_env(oop v) {
  return value_eq(global_env, v);
}

// Registers a lisp value under a global variable name.
void register_globally(const char* name, oop value) {
  symbol_t* symbol = make_symbol(name);
  register_globally_symbol(symbol, value);
}

// TODO: Move this to a different module.
// Registers a lisp function under a global variable name.
void register_globally_fn(const char* name, function fn) {
  register_globally(name, make_native_procedure(fn));
}

oop lookup_globally(symbol_t* key) {
  // Doesn't use lookup_var_object_globally for performance reasons.
  // The caller guarantees that the symbol is defined.
  return var_get(dict_get(global_env, symbol_to_oop(key)));
}

// This returns a set/unset var object as in vars.h, not the defined value!
oop lookup_var_object_globally(symbol_t* key) {
  if (!dict_has_key(global_env, symbol_to_oop(key))) {
    dict_put(global_env, symbol_to_oop(key), make_undefined_var(key));
  }
  return dict_get(global_env, symbol_to_oop(key));
}

static void enumerate_gc_roots(void (*accept)(oop* place)) {
  accept(&global_env);
  accept(&remaining_declarations);
}

void init_eval() {
  static bool initialized = false;
  if (initialized) return;
  global_env = make_dict(47);
  remaining_declarations = NIL;
  register_globally("nil", NIL);
  register_globally("true", symbols._true);
  register_globally("false", symbols._false);
  register_globally("Array", symbols._array);
  register_globally("Cons", symbols._cons);
  register_globally("Dict", symbols._dict);
  register_globally("Frame", symbols._frame);
  register_globally("Stack", symbols._stack);
  register_globally("String", symbols._string);
  register_globally("Symbol", symbols._symbol);
  register_globally("DefinedVar", symbols._defined_var);
  register_globally("UndefinedVar", symbols._undefined_var);
  register_globally("NativeProcedure", symbols._native_procedure);
  register_globally("CompiledProcedure", symbols._compiled_procedure);
  register_globally("True", symbols._True);
  register_globally("False", symbols._False);
  gc_register_persistent_refs(enumerate_gc_roots);
}

// TODO: Write REPL and file loading more in Lisp and remove the weird
// compilation code from here.
static oop eval_global(oop program) {
  if (is_defined_globally(symbols._macroexpand)) {
    oop macroexpand_fn = lookup_globally(symbols._macroexpand);
    program = apply(make_cons(macroexpand_fn, make_cons(program, NIL)));
  }
  return apply_compiled_lisp_procedure(
      compile_top_level_expression(program), NIL, NULL);
}

void load_decls(oop decls) {
  while (!is_nil(decls)) {
    // Protect the remaining declarations during GC.
    remaining_declarations = cdr(decls);

    eval_global(car(decls));
    gc_run();

    decls = remaining_declarations;
  }
}
