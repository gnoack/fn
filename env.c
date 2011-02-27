
#include "value.h"
#include "cons.h"
#include "carcdr.h"

#include "env.h"

void env_put(oop env, oop key, oop value) {
  CHECK(is_cons(env), "Can't mutate empty environment for now.");
  if (value_eq(caar(env), key)) {
    set_rest(car(env), value);
  } else if (is_nil(cdr(env))) {
    set_rest(env, make_env(key, value, NIL));
  } else {
    env_put(cdr(env), key, value);
  }
}

oop make_env(oop key, oop value, oop env) {
  return make_cons(make_cons(key, value), env);
}

bool env_haskey(oop env, oop key) {
  while (!is_nil(env)) {
    if (value_eq(key, caar(env))) {
      return YES;
    }
    env = cdr(env);
  }
  return NO;
}

oop env_lookup(oop env, oop key) {
  if (is_nil(env)) {
    return NIL;
  } else {
    oop pair = car(env);
    oop mykey = car(pair);
    if (value_eq(mykey, key)) {
      return cdr(pair);
    } else {
      return env_lookup(cdr(env), key);
    }
  }
}

