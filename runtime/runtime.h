#ifndef _RUNTIME_H_
#define _RUNTIME_H_

// Initializes all modules in the runtime, in the right order.
void fn_runtime_init();
void fn_runtime_init_lisp_decls();
void fn_load_file(const char* filename);

#endif  // _RUNTIME_H_
