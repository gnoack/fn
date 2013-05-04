#ifndef _RUNTIME_H_
#define _RUNTIME_H_

extern void fn_runtime_init();
extern void fn_runtime_init_lisp_decls();
extern void fn_load_file(const char* filename);

#endif  // _RUNTIME_H_
