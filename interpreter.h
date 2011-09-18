#ifndef _INTERPRETER_H_

#include "value.h"

extern void init_interpreter();

// Stack
extern void stack_push(oop value);
extern oop stack_pop();

// Frame
extern
oop make_frame(unsigned int argnum,
	       unsigned char* retptr,
	       oop retfrm,
	       oop next_frame);

extern
void set_var(oop frame, unsigned int index, oop value);

extern
oop get_var(oop frame, unsigned int index);

typedef struct {
  oop reg_acc;
  oop reg_frm;
  unsigned char* ip;
} interpreter_state_t;

extern interpreter_state_t state;

// Bytecodes
#define BC_HALT 0
#define BC_JUMP 1
#define BC_JUMP_IF_TRUE 2
#define BC_LOAD_VALUE 3
#define BC_WRITE_VALUE 4
#define BC_LOAD_VAR 5
#define BC_WRITE_VAR 6
#define BC_LOAD_GLOBAL_VAR 7
#define BC_WRITE_GLOBAL_VAR 8
#define BC_PUSH 9
#define BC_POP 10
#define BC_MAKE_LAMBDA 11
#define BC_CALL 12
#define BC_RETURN 13

#define _INTERPRETER_H_ 0
#endif  // _INTERPRETER_H_
