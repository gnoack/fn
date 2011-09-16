
#include "value.h"
#include "memory.h"

#include "interpreter.h"

#define MAX_STACK_SIZE 0x800

oop stack[MAX_STACK_SIZE];
unsigned int stack_size;
interpreter_state_t interpreter_state;

void init_interpreter() {
  int i;
  for (i=0; i<MAX_STACK_SIZE; i++) {
    stack[i] = NIL;
  }
  stack_size = 0;
  interpreter_state.reg_acc = NIL;
  interpreter_state.reg_frm = NIL;  // TODO
}

// Stack
void stack_push(oop value) {
  CHECK(stack_size < MAX_STACK_SIZE, "Stack too large, can't push.");
  stack[stack_size] = value;
  stack_size++;
}

oop stack_pop() {
  CHECK(stack_size > 0, "Stack empty, can't pop.");
  stack_size--;
  return stack[stack_size];
}

// Frame
oop make_frame(unsigned int argnum,
	       unsigned char* retptr,
	       oop retfrm,
	       oop next_frame) {
  oop result = mem_alloc(5 + argnum);
  mem_set(result, 0, NIL);  // TODO: Type marker.
  mem_set(result, 1, (oop) (oop*) retptr);
  mem_set(result, 2, retfrm);
  mem_set(result, 3, next_frame);
  mem_set(result, 4, make_smallint(argnum));
  return result;
}

oop nth_frame(oop frame, unsigned int depth) {
  while (depth > 0) {
    frame = mem_get(frame, 3);  // next_frame
    depth--;
  }
  return frame;
}

void set_var(oop frame, unsigned int index, oop value) {
  CHECK(0 <= index && index < get_smallint(mem_get(frame, 4)),
	"Index out of bounds.");
  mem_set(frame, 5 + index, value);
}

oop get_var(oop frame, unsigned int index) {
  CHECK(0 <= index && index < get_smallint(mem_get(frame, 4)),
	"Index out of bounds.");
  return mem_get(frame, 5 + index);
}

// Reading from the instruction stream
unsigned char read_byte() {
  unsigned char result = *(interpreter_state.ip);
  interpreter_state.ip++;
  return result;
}

oop read_oop() {
  oop* ptr = (oop*) interpreter_state.ip;
  oop result = *ptr;
  ptr++;
  interpreter_state.ip = (unsigned char*) ptr;
  return result;
}

// Interpreter
extern void interpret() {
  for (;;) {
    unsigned char operation = read_byte();
    switch (operation) {
    case BC_HALT:
      return;
    case BC_LOAD_VALUE:
      interpreter_state.reg_acc = read_oop();
      break;
    case BC_PUSH:
      stack_push(interpreter_state.reg_acc);
      break;
    case BC_POP:
      interpreter_state.reg_acc = stack_pop();
      break;
    case BC_LOAD_VAR:
      {
	unsigned int depth = read_byte();
	unsigned int index = read_byte();
	oop frame = nth_frame(interpreter_state.reg_frm, depth);
	interpreter_state.reg_acc = get_var(frame, index);
      }
      break;
    case BC_WRITE_VAR:
      {
	unsigned int depth = read_byte();
	unsigned int index = read_byte();
	oop frame = nth_frame(interpreter_state.reg_frm, depth);
	set_var(frame, index, interpreter_state.reg_acc);
      }
      break;
    case BC_MAKE_LAMBDA:
      {
	interpreter_state.reg_acc = mem_alloc(3);
	// TODO: Set type marker.
	mem_set(interpreter_state.reg_acc, 1, interpreter_state.reg_frm);
	mem_set(interpreter_state.reg_acc, 2, read_oop());
      }
      break;
    default:
      printf("Unknown byte code: %02x\n", operation);
      exit(1);
    }
  }
}
