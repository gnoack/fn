#ifndef _INTERPRETER_H_
#define _INTERPRETER_H_

#include "value.h"

typedef struct procedure proc_t;  // TODO: Forward decl from procedures.h


extern void init_interpreter();
extern oop interpret(oop frame, proc_t* proc);

// Stack
typedef struct {
  oop* stack;
  unsigned int size;
  unsigned int max_size;
} stack_t;

extern void stack_push(stack_t* stack, oop value);
extern oop stack_pop(stack_t* stack);
extern unsigned int stack_size(stack_t* stack);
extern oop stack_peek(stack_t* stack);
extern oop stack_peek_at(stack_t* stack, fn_uint n);
extern void stack_shrink(stack_t* stack, int n);

// Frame
extern oop make_frame(proc_t* proc, oop caller);
extern boolean is_frame(oop obj);
extern void print_frame(oop obj);
extern void frame_set_var(oop frame, unsigned int index, oop value);

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
enum {
  BC_JUMP             = 0,
  BC_JUMP_IF_TRUE     = 1,
  BC_LOAD_VALUE       = 2,
  BC_READ_VAR         = 3,
  BC_WRITE_VAR        = 4,
  BC_READ_GLOBAL_VAR  = 5,
  BC_WRITE_GLOBAL_VAR = 6,
  BC_DISCARD          = 7,
  BC_MAKE_LAMBDA      = 8,
  BC_CALL             = 9,
  BC_TAIL_CALL        = 10,
  BC_RETURN           = 11,
  BC_TAIL_CALL_APPLY  = 12,
};

#endif  // _INTERPRETER_H_
