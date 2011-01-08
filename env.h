
#ifndef __ENV_H__

// Empty environment is nil.
extern oop make_env(oop key, oop value, oop env);
extern bool env_haskey(oop env, oop key);
extern oop env_lookup(oop env, oop key);

#define __ENV_H__ 0
#endif // __ENV_H__
