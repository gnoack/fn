#ifndef _INTERPRETER_H_
#define _INTERPRETER_H_

#include "value.h"

extern void init_interpreter();
extern oop interpret(oop frame, oop procedure);

// Stack
typedef struct {
  oop* stack;
  unsigned int size;
  unsigned int max_size;
} stack_t;

extern void stack_init(stack_t* stack, unsigned int max_size);
extern void stack_push(stack_t* stack, oop value);
extern oop stack_pop(stack_t* stack);
extern unsigned int stack_size(stack_t* stack);
extern oop stack_peek(stack_t* stack);
extern oop stack_peek_at(stack_t* stack, fn_uint n);
extern void stack_shrink(stack_t* stack, int n);

// Frame
extern oop make_frame(oop procedure, oop caller);
extern boolean is_frame(oop obj);
extern void print_frame(oop obj);
extern void set_var(oop frame, unsigned int index, oop value);
extern oop get_var(oop frame, unsigned int index);

extern boolean is_continuation(oop continuation);


typedef struct {
  // Current frame.
  oop reg_frm;
  stack_t stack;

  // Execution point.
  fn_uint ip;
  oop bytecode;
  oop oop_lookups;
} interpreter_state_t;


// Bytecodes
#define BC_JUMP 0
#define BC_JUMP_IF_TRUE 1
#define BC_LOAD_VALUE 2
#define BC_READ_VAR 3
#define BC_WRITE_VAR 4
#define BC_READ_GLOBAL_VAR 5
#define BC_WRITE_GLOBAL_VAR 6
#define BC_DISCARD 7
#define BC_MAKE_LAMBDA 8
#define BC_CALL 9
#define BC_TAIL_CALL 10
#define BC_RETURN 11
#define BC_CALL_CC 12
#define BC_INVALIDATE_CONTINUATION 13
#define BC_RESTORE_CONTINUATION 14
#define BC_TAIL_CALL_APPLY 15

#endif  // _INTERPRETER_H_
