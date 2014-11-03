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
#include "vars.h"

oop global_env;
oop remaining_declarations;

struct {
  boolean active;
  oop expr;
  oop env;
} global_trampoline;


oop trampoline(oop expr, oop env) {
  global_trampoline.expr = expr;
  global_trampoline.env = env;
  global_trampoline.active = YES;
  return NIL;
}

static inline
oop eval_trampoline(oop program, oop env);

void set_globally_oop(oop key, oop value) {
  // If it's a procedure, set its name.
  if (is_procedure(value)) {
    procedure_set_name(value, key);
  }
  dict_put(global_env, key, make_var(key, value));
}

static inline
boolean is_defined_globally(oop key) {
  return dict_has_key(global_env, key)
    && is_set_var(dict_get(global_env, key));
}

// Like the above, but also check for redefinitions.
void register_globally_oop(oop key, oop value) {
  CHECKV(!is_defined_globally(key), key, "Symbol already defined.");
  set_globally_oop(key, value);
}

boolean is_global_env(oop v) {
  return value_eq(global_env, v);
}

// Registers a lisp value under a global variable name.
void register_globally(const char* name, oop value) {
  oop symbol = make_symbol(name);
  register_globally_oop(symbol, value);
}

// TODO: Move this to a different module.
// Registers a lisp function under a global variable name.
void register_globally_fn(const char* name, function fn) {
  register_globally(name, make_native_procedure(fn));
}

oop lookup_globally(oop key) {
  return var_get(dict_get(global_env, key));
}

void enumerate_gc_roots(void (*accept)(oop* place)) {
  accept(&global_env);
  accept(&remaining_declarations);
}

void init_eval() {
  static boolean initialized = NO;
  if (initialized) return;
  global_env = make_dict(47);
  global_trampoline.active = NO;
  remaining_declarations = NIL;
  register_globally("nil", NIL);
  register_globally("true", symbols._true);
  register_globally("false", symbols._false);
  register_globally("Array", symbols._array);
  register_globally("Cons", symbols._cons);
  register_globally("Continuation", symbols._continuation);
  register_globally("Dframe", symbols._dframe);
  register_globally("Dict", symbols._dict);
  register_globally("Frame", symbols._frame);
  register_globally("Stack", symbols._stack);
  register_globally("String", symbols._string);
  register_globally("Symbol", symbols._symbol);
  register_globally("DefinedVar", symbols._defined_var);
  register_globally("UndefinedVar", symbols._undefined_var);
  register_globally("Procedure", symbols._procedure);
  register_globally("NativeProcedure", symbols._native_procedure);
  register_globally("CompiledProcedure", symbols._compiled_procedure);
  register_globally("True", symbols._True);
  register_globally("False", symbols._False);
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
    return trampoline(then_branch, env);
  } else {
    CHECKV(value_eq(condition_result, symbols._false),
	   condition_result,
	  "Need true or false as condition value.");
    return trampoline(else_branch, env);
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
  return trampoline(first(expressions), env);
}

oop eval_let(oop sexp, oop env) {
  CHECKV(value_eq(symbols._let, car(sexp)), sexp, "Must be a let form.");
  CHECKV(length_int(sexp) >= 3, sexp, "Bad let form length.");
  oop bindings = cadr(sexp);
  oop new_env = make_dframe(env, length_int(bindings), env, NIL);
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

oop eval_progn(oop program, oop env) {
  return eval_all(rest(program), env);
}

// TODO: Write REPL and file loading more in Lisp and remove the weird
// compilation code from here.
extern
oop eval_global(oop program) {
  if (is_defined_globally(symbols._macroexpand)) {
    oop macroexpand_fn = lookup_globally(symbols._macroexpand);
    program = apply(make_cons(macroexpand_fn, make_cons(program, NIL)));
  }
  oop _compile_top_level_expr =
    make_symbol("compile-top-level-expr");
  if (is_defined_globally(_compile_top_level_expr)) {
    oop proc = apply(LIST(lookup_globally(_compile_top_level_expr), program));
    oop result = apply(make_cons(proc, NIL));
    return result;
  }
  gc_protect_counter++;
  oop result = eval(program, global_env);
  gc_protect_counter--;
  return result;
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

static inline
oop apply_with_trampoline(oop values, oop env) {
  oop fn = first(values);
  if (is_lisp_procedure(fn)) {
    oop env = make_dframe_for_application(fn, rest(values), env);
    oop program = make_cons(symbols._progn, fn_code(fn));
    return trampoline(program, env);
  } else {
    return apply_with_caller(values, env);
  }
}

static inline
oop eval_trampoline(oop program, oop env) {
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
  if (value_eq(command, symbols._progn)) {
    return eval_progn(program, env);
  }
  // Otherwise, it must be a function application.
  return apply_with_trampoline(map_eval(program, env), env);
}

// Evaluate and bounce on returned trampolines.
extern
oop eval(oop program, oop env) {
  oop result = NIL;
  for (;;) {
    global_trampoline.active = NO;
    result = eval_trampoline(program, env);
    if (global_trampoline.active) {
      //printf("jumping into trampoline ");
      //println_value(program);
      program = global_trampoline.expr;
      env = global_trampoline.env;
      continue;
    }
    break;
  };
  return result;
}

extern
void load_decls(oop decls) {
  while (!is_nil(decls)) {
    // Protect the remaining declarations during GC.
    remaining_declarations = cdr(decls);

    eval_global(car(decls));
    gc_run();

    decls = remaining_declarations;
  }
}
