
#ifndef __ENV_H__

/*
 * An environment maps values to other values.
 * Keys are compared using value_eq (reference identity).
 */
// Empty environment is nil.
extern oop make_env(oop key, oop value, oop env);
extern boolean env_haskey(oop env, oop key);
extern oop env_lookup(oop env, oop key);

/* Manipulates an environment. */
extern void env_put(oop env, oop key, oop value);

#define __ENV_H__ 0
#endif // __ENV_H__
