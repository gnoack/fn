#ifndef _INTERPRETER_H_
#define _INTERPRETER_H_

#include "value.h"

void init_interpreter();

// Start interpreting the given frame.
//
// 'frame' needs to be populated with the necessary information:
// - lexical environment (frame)
// - caller frame to return to
// - procedure and instruction pointer
// - a stack sufficient for the function to execute
// - appropriate args, set using frame_set_var().
//
// The frame is populated in procedures.c.
oop interpret(frame_t* frame);

// Stack
typedef struct {
  oop* stack;
  unsigned int size;
  unsigned int max_size;
} stack_t;

void stack_push(stack_t* stack, oop value);
oop stack_pop(stack_t* stack);
unsigned int stack_size(stack_t* stack);
oop stack_peek(stack_t* stack);
oop stack_peek_at(stack_t* stack, fn_uint n);
void stack_shrink(stack_t* stack, int n);

// Frame
frame_t* make_frame(proc_t* proc, frame_t* caller);
bool is_frame(oop);
void print_frame(frame_t*);
void frame_set_var(frame_t* frame, unsigned int index, oop value);

typedef struct {
  // Current frame.
  frame_t* reg_frm;
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
  BC_READ_FIELD       = 13,
  BC_WRITE_FIELD      = 14,
};

#endif  // _INTERPRETER_H_
