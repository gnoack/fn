
#include "value.h"
#include "memory.h"
#include "symbols.h"

#include "interpreter.h"

#define MAX_STACK_SIZE 0x800

oop stack[MAX_STACK_SIZE];
unsigned int stack_size;
interpreter_state_t state;

void init_interpreter() {
  int i;
  for (i=0; i<MAX_STACK_SIZE; i++) {
    stack[i] = NIL;
  }
  stack_size = 0;
  state.reg_acc = NIL;
  state.reg_frm = NIL;  // TODO
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

oop read_oop() {
  oop result = *state.ip;
  state.ip++;
  return result;
}

#define OPEQ(name) value_eq(operation, symbols._bc_ ## name)

// Interpreter
extern void interpret() {
  for (;;) {
    oop operation = read_oop();
    if (OPEQ(halt)) {
      return;
    } else if (OPEQ(load_value)) {
      state.reg_acc = read_oop();
    } else if (OPEQ(push)) {
      stack_push(state.reg_acc);
    } else if (OPEQ(pop)) {
      state.reg_acc = stack_pop();
    } else if (OPEQ(load_var)) {
      unsigned int depth = get_smallint(read_oop());
      unsigned int index = get_smallint(read_oop());
      oop frame = nth_frame(state.reg_frm, depth);
      state.reg_acc = get_var(frame, index);
    } else if (OPEQ(write_var)) {
      unsigned int depth = get_smallint(read_oop());
      unsigned int index = get_smallint(read_oop());
      oop frame = nth_frame(state.reg_frm, depth);
      set_var(frame, index, state.reg_acc);
    } else if (OPEQ(make_lambda)) {
      state.reg_acc = mem_alloc(3);
      mem_set(state.reg_acc, 0, NIL);  // TODO: Marker
      mem_set(state.reg_acc, 1, state.reg_frm);
      mem_set(state.reg_acc, 2, read_oop());
    } else {
      // TODO: Implement CALL, RETURN, global variables (R/W).
      printf("Unknown byte code:\n");
      print_value(operation);
      exit(1);
    }
  }
}

#undef OPEQ

