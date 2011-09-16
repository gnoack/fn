
#include "value.h"

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

extern oop stack_pop() {
  CHECK(stack_size > 0, "Stack empty, can't pop.");
  stack_size--;
  return stack[stack_size];
}

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
    default:
      printf("Unknown byte code: %02x\n", operation);
      exit(1);
    }
  }
}
