
#include "value.h"
#include "cons.h"
#include "carcdr.h"

#include "env.h"

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

