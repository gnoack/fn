
#include <stdio.h>

#include "carcdr.h"
#include "cons.h"
#include "data.h"
#include "debug.h"
#include "eval.h"
#include "gc.h"
#include "procedures.h"
#include "strings.h"
#include "symbols.h"
#include "value.h"

oop global_env;
oop remaining_declarations;

void set_globally_oop(oop key, oop value) {
  // If it's a procedure, set its name.
  if (is_procedure(value)) {
    procedure_set_name(value, key);
  }
  dict_put(global_env, key, value);
}

// Like the above, but also check for redefinitions.
void register_globally_oop(oop key, oop value) {
  CHECKV(!dict_has_key(global_env, key), key, "Symbol already defined.");
  set_globally_oop(key, value);
}

boolean is_global_env(oop v) {
  return value_eq(global_env, v);
}

// Registers a lisp value under a global variable name.
void register_globally(const char* name, oop value) {
  register_globally_oop(make_symbol(name), value);
}

// TODO: Move this to a different module.
// Registers a lisp function under a global variable name.
void register_globally_fn(const char* name, function fn) {
  register_globally(name, make_native_procedure(fn));
}

oop lookup_globally(oop key) {
  return dict_get(global_env, key);
}

void enumerate_gc_roots(void (*accept)(oop* place)) {
  accept(&global_env);
  accept(&remaining_declarations);
}

void init_eval() {
  static boolean initialized = NO;
  if (initialized) return;
  global_env = make_dict(47);
  remaining_declarations = NIL;
  register_globally("nil", NIL);
  register_globally("true", symbols._true);
  register_globally("false", symbols._false);
  register_globally("@array", symbols._array);
  register_globally("@cons", symbols._cons);
  register_globally("@continuation", symbols._continuation);
  register_globally("@dframe", symbols._dframe);
  register_globally("@dict", symbols._dict);
  register_globally("@frame", symbols._frame);
  register_globally("@retptr", symbols._retptr);
  register_globally("@string", symbols._string);
  register_globally("@symbol", symbols._symbol);
  register_globally("@procedure", symbols._procedure);
  register_globally("@native-procedure", symbols._native_procedure);
  register_globally("@compiled-procedure", symbols._compiled_procedure);
  gc_register_persistent_refs(enumerate_gc_roots);
}

// Evaluation.

oop eval_if(oop sexp, oop env) {
  CHECKV(length_int(sexp) == 4, sexp, "If-expression must have size of 4");
  oop condition = cadr(sexp);
  oop then_branch = caddr(sexp);
  oop else_branch = cadddr(sexp);
  oop condition_result = eval(condition, env);
  if (value_eq(condition_result, symbols._true)) {
    return eval(then_branch, env);
  } else {
    CHECKV(value_eq(condition_result, symbols._false),
	   condition_result,
	  "Need true or false as condition value.");
    return eval(else_branch, env);
  }
}

oop eval_let(oop sexp, oop env) {
  CHECKV(value_eq(symbols._let, car(sexp)), sexp, "Must be a let form.");
  CHECKV(length_int(sexp) >= 3, sexp, "Bad let form length.");
  oop bindings = cadr(sexp);
  oop new_env = make_dframe(env, length_int(bindings));
  int pos = 0;
  while (!is_nil(bindings)) {
    oop binding = first(bindings);
    CHECKV(length_int(binding) == 2, binding, "Bindings need length 2.");
    oop key = car(binding);
    oop value = eval(cadr(binding), env);
    dframe_register_key(new_env, pos, key, value);
    pos++;
    bindings = rest(bindings);
  }
  oop body = cddr(sexp);
  return eval_all(body, new_env);
}

// TODO: Should register in current env only.
// TODO: Registering multiple variables at once.
oop eval_def(oop program, oop env) {
  oop symbol = cadr(program);
  oop value = eval(caddr(program), env);
  register_globally_oop(symbol, value);
  return value;
}

oop eval_lambda(oop program, oop env) {
  return make_procedure(cadr(program),
			cddr(program),
			env);
}

oop eval_quote(oop program, oop env) {
  CHECKV(length_int(program) == 2, program, "quote has only one argument.");
  return cadr(program);  // Quote.
}

/* Primitive set!, modifies an existing variable in the environment. */
oop eval_set(oop program, oop env) {
  CHECKV(length_int(program) == 3, program, "set! needs two arguments.");
  oop symbol = cadr(program);
  oop new_value = eval(caddr(program), env);
  dframe_set(env, symbol, new_value);
  return new_value;
}

// TODO: Write REPL and file loading more in Lisp and remove the weird
// compilation code from here.
extern
oop eval_global(oop program) {
  if (dict_has_key(global_env, symbols._macroexpand)) {
    oop macroexpand_fn = dict_get(global_env, symbols._macroexpand);
    program = apply(make_cons(macroexpand_fn, make_cons(program, NIL)));
  }
  oop _compile_for_global_eval =
    make_symbol("compile-and-assemble-expr-for-global-eval");
  if (dict_has_key(global_env, _compile_for_global_eval)) {
    oop code = eval(LIST(_compile_for_global_eval,
                         LIST(make_symbol("quote"),
                              program)),
                    global_env);
    return interpret(global_env, code);
  }
  return eval(program, global_env);
}

// C equivalent to calling (map eval list).
oop map_eval(oop list, oop env) {
  if (is_nil(list)) {
    return list;
  } else {
    // Done in two steps for in-order execution.
    oop mycar = eval(car(list), env);
    return make_cons(mycar, map_eval(cdr(list), env));
  }
}

// Evaluate all expressions in the environment, return last result.
// (Imperative).
oop eval_all(oop expressions, oop env) {
  CHECKV(is_cons(expressions), expressions, "Must be a list of expressions.");
  while (!is_nil(rest(expressions))) {
    eval(first(expressions), env);  // Discarding result.
    expressions = rest(expressions);
  }
  return eval(first(expressions), env);
}

extern
oop eval(oop program, oop env) {
  //println_value(program);
  if (is_smallint(program) || is_nil(program) ||
      is_char(program) || is_string(program)) {
    return program;
  } else if (is_symbol(program)) {
    // TODO: Proper error handling, this just crashes.
    return dframe_get(env, program);
  }
  CHECKV(is_cons(program), program, "What is this? I can't evaluate it!");
  oop command = car(program);
  if (value_eq(command, symbols._if)) {
    return eval_if(program, env);
  }
  if (value_eq(command, symbols._lambda)) {
    return eval_lambda(program, env);
  }
  if (value_eq(command, symbols._def)) {
    return eval_def(program, env);
  }
  if (value_eq(command, symbols._let)) {
    return eval_let(program, env);
  }
  if (value_eq(command, symbols._quote)) {
    return eval_quote(program, env);
  }
  if (value_eq(command, symbols._set)) {
    return eval_set(program, env);
  }
  // Otherwise, it must be a function application.
  return apply(map_eval(program, env));
}

extern
void load_decls(oop decls) {
  while (!is_nil(decls)) {
    eval_global(car(decls));

    // Protect the remaining declarations during GC.
    remaining_declarations = cdr(decls);
    gc_run();
    decls = remaining_declarations;
  }
}
