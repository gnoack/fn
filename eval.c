
#include <stdio.h>

#include "gc.h"
#include "cons.h"
#include "value.h"
#include "carcdr.h"
#include "procedures.h"
#include "symbols.h"
#include "env.h"

#include "eval.h"

oop global_env;

// TODO: Check for already registered keys.
void register_globally_oop(oop key, oop value) {
  // If it's a procedure, set its name.
  if (is_procedure(value)) {
    fn_set_name(value, key);
  }
  // This is needed for recursion!
  if (is_nil(global_env)) {
    global_env = make_env(key, value, global_env);
  } else {
    env_put(global_env, key, value);
  }
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
  register_globally(name, make_native_fn(fn));
}

oop lookup_globally(oop key) {
  return env_lookup(global_env, key);
}

void init_eval() {
  static boolean initialized = NO;
  if (initialized) return;
  global_env = NIL;
  register_globally("true", symbols._true);
  register_globally("false", symbols._false);
  register_globally("@cons", symbols._cons);
  register_globally("@procedure", symbols._procedure);
  register_globally("@native-procedure", symbols._native_procedure);
  register_globally("@compiled-procedure", symbols._compiled_procedure);
  register_globally("@string", symbols._string);
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
  while (!is_nil(bindings)) {
    oop binding = car(bindings);
    CHECKV(length_int(binding) == 2, binding, "Bindings need length 2.");
    env = make_env(car(binding), eval(cadr(binding), env), env);
    bindings = cdr(bindings);
  }
  oop body = cddr(sexp);
  return eval_all(body, env);
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
  CHECKV(env_haskey(env, symbol), symbol, "Symbol not present in environment.");
  oop new_value = eval(caddr(program), env);
  env_put(env, symbol, new_value);
  return new_value;
}

extern
oop eval_global(oop program) {
  if (env_haskey(global_env, symbols._macroexpand)) {
    oop macroexpand_fn = env_lookup(global_env,
				    symbols._macroexpand);
    program = apply(make_cons(macroexpand_fn,
			      make_cons(program, NIL)));
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
  //print_value(program);
  if (is_smallint(program) || is_nil(program) ||
      is_char(program) || is_string(program)) {
    return program;
  } else if (is_symbol(program)) {
    // TODO: Proper error handling.
    CHECKV(env_haskey(env, program), program, "Unknown symbol.");
    return env_lookup(env, program);
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

    decls = cdr(decls);
  }
  global_env = gc_run(global_env);
}
