
#include "cons.h"
#include "eval.h"  // TODO: Break dependency to lookup_globally().
#include "memory.h"
#include "symbols.h"
#include "value.h"
#include "procedures.h"

#include "carcdr.h"
#include "interpreter.h"

#define MAX_STACK_SIZE 0x4000

// #define INTERPRETER_DEBUG 1
// #define INTERPRETER_LOGGING 1

#ifdef INTERPRETER_LOGGING
#define IPRINT(...) printf(__VA_ARGS__)
#define IVALUE(x) print_value(x)
#else
#define IPRINT(...)
#define IVALUE(x)
#endif  // INTERPRETER_DEBUG

typedef struct {
  oop stack[MAX_STACK_SIZE];
  unsigned int size;
} stack_t;

stack_t stack;

void init_interpreter() {
  int i;
  for (i=0; i<MAX_STACK_SIZE; i++) {
    stack.stack[i] = NIL;
  }
  stack.size = 0;
}

// Stack
void stack_push(oop value) {
  if (stack.size >= MAX_STACK_SIZE) {
    int i;
    for (i=0; i<MAX_STACK_SIZE; i++) {
      print_value(stack.stack[i]);
    }
  }
  CHECK(stack.size < MAX_STACK_SIZE, "Stack too large, can't push.");
  stack.stack[stack.size] = value;
  stack.size++;
}

oop stack_peek() {
  CHECK(stack.size > 0, "Stack empty, can't peek.");
  return stack.stack[stack.size - 1];
}

oop stack_pop() {
  CHECK(stack.size > 0, "Stack empty, can't pop.");
  stack.size--;
  return stack.stack[stack.size];
}

// Convenience function to pop n elements from the stack and make them
// into a list.
oop stack_pop_list(fn_uint size) {
  oop list = NIL;
  while (size > 0) {
    list = make_cons(stack_pop(), list);
    size--;
  }
  return list;
}

#ifdef INTERPRETER_DEBUG
void print_stack() {
  int i;
  for (i=stack.size-1; i>=0; i--) {
    printf(" [s] %03d ", i);
    print_value(stack.stack[i]);
  }
}
#endif  // INTERPRETER_DEBUG


// Frame
oop make_frame(unsigned int argnum, oop next_frame) {
  oop result = mem_alloc(5 + argnum);
  mem_set(result, 0, symbols._frame);
  mem_set(result, 1, next_frame);
  mem_set(result, 2, make_smallint(argnum));
  return result;
}

oop nth_frame(oop frame, unsigned int depth) {
  while (depth > 0) {
    frame = mem_get(frame, 1);  // next_frame
    depth--;
  }
  return frame;
}

void set_var(oop frame, unsigned int index, oop value) {
  CHECK(0 <= index && index < get_smallint(mem_get(frame, 2)),
	"Index out of bounds.");
  mem_set(frame, 3 + index, value);
}

oop get_var(oop frame, unsigned int index) {
  CHECK(0 <= index && index < get_smallint(mem_get(frame, 2)),
	"Index out of bounds.");
  return mem_get(frame, 3 + index);
}


// Apply compiled Lisp procedures by modifying
// the bytecode interpreter state and letting the interpreter do it.
void apply_into_interpreter(fn_uint arg_count, interpreter_state_t* state,
                            boolean tailcall) {
  // TODO: Stack traces for procedures executed like this.
  oop values = stack_pop_list(arg_count);
  IPRINT("call %lu         .oO ", arg_count);
  IVALUE(values);
  oop cfn = car(values);
  if (is_compiled_lisp_procedure(cfn)) {
    oop args = cdr(values);
    oop env = make_frame_for_application(cfn, args);

    if (tailcall == NO) {
      stack_push(serialize_retptr(state));
    } else {
      apply_stack_pop();
    }
    apply_stack_push(values);

    // Modify the interpreter state.

    // Data.
    state->reg_acc = NIL;  // Just for encapsulation.
    state->reg_frm = env;

    // Position.
    oop code = fn_code(cfn);
    state->ip = get_smallint(car(code));
    state->bytecode = cadr(code);
    state->oop_lookups = caddr(code);
  } else {
    // Call recursively on the C stack.
    state->reg_acc = apply(values);
  }
}



unsigned char read_byte(interpreter_state_t* state) {
  unsigned char result = ((unsigned char*) state->bytecode.mem)[state->ip];
  state->ip++;
  return result;
}

fn_uint read_label_address(interpreter_state_t* state) {
  fn_uint upper = read_byte(state);
  fn_uint lower = read_byte(state);
  return (upper << 8) | lower;
}

fn_uint read_index(interpreter_state_t* state) {
  return read_byte(state);
}

// TODO: Only one byte: Will it be enough?
oop read_oop(interpreter_state_t* state) {
#ifdef INTERPRETER_DEBUG
  return mem_get(state->oop_lookups, 2 + read_byte(state));
#else
  return state->oop_lookups.mem[2 + read_byte(state)];
#endif  // INTERPRETER_DEBUG
}

oop serialize_retptr(interpreter_state_t* state) {
  oop result = mem_alloc(4);
  // TODO: Type marker?
  mem_set(result, 0, state->reg_frm);
  mem_set(result, 1, make_smallint(state->ip));
  mem_set(result, 2, state->bytecode);
  mem_set(result, 3, state->oop_lookups);
  return result;
}

void deserialize_retptr(oop retptr, interpreter_state_t* state) {
  state->reg_frm     = mem_get(retptr, 0);
  state->ip          = get_smallint(mem_get(retptr, 1));
  state->bytecode    = mem_get(retptr, 2);
  state->oop_lookups = mem_get(retptr, 3);
}

// Interpreter
oop interpret(oop frame, oop code) {
  interpreter_state_t state;
  state.reg_acc = NIL;
  state.reg_frm = frame;
  state.ip = get_smallint(first(code));
  state.bytecode = first(rest(code));
  state.oop_lookups = first(rest(rest(code)));

  #ifdef INTERPRETER_DEBUG
  unsigned int stack_size_before = stack.size;
  #endif  // INTERPRETER_DEBUG

  stack_push(NIL);  // Return pointer.

  for (;;) {
    IPRINT("[i] %5lu: ", state.ip);
    unsigned char operation = read_byte(&state);
    switch (operation) {
    case BC_HALT:
      IPRINT("halt\n");
      return NIL;
    case BC_JUMP: {
      fn_uint address = read_label_address(&state);
      IPRINT("jump %lu\n", address);
      state.ip = address;
      break;
    }
    case BC_JUMP_IF_TRUE: {
      fn_uint address = read_label_address(&state);
      if (value_eq(state.reg_acc, symbols._true)) {
        IPRINT("jump-if-true %lu (taken)\n", address);
        state.ip = address;
      } else {
        CHECK(value_eq(state.reg_acc, symbols._false),
              "Condition evaluated to non-boolean value.");
        IPRINT("jump-if-true %lu (not taken)\n", address);
      }
      break;
    }
    case BC_LOAD_VALUE:
      state.reg_acc = read_oop(&state);
      IPRINT("load-value ");
      IVALUE(state.reg_acc);
      stack_push(state.reg_acc);
      break;
    case BC_READ_VAR: {
      fn_uint depth = read_index(&state);
      fn_uint index = read_index(&state);
      oop frame = nth_frame(state.reg_frm, depth);
      state.reg_acc = get_var(frame, index);
      IPRINT("read-var %lu %lu   .oO ", depth, index);
      IVALUE(state.reg_acc);
      stack_push(state.reg_acc);
      break;
    }
    case BC_WRITE_VAR: {
      fn_uint depth = read_index(&state);
      fn_uint index = read_index(&state);
      oop frame = nth_frame(state.reg_frm, depth);
      state.reg_acc = stack_peek();
      set_var(frame, index, state.reg_acc);
      IPRINT("write-var %lu %lu  // ", depth, index);
      IVALUE(state.reg_acc);
      break;
    }
    case BC_READ_GLOBAL_VAR: {
      oop key = read_oop(&state);
      state.reg_acc = lookup_globally(key);
      IPRINT("load-global-var ");
      IVALUE(key);
      stack_push(state.reg_acc);
      break;
    }
    case BC_WRITE_GLOBAL_VAR: {
      oop key = read_oop(&state);
      // TODO: Make bytecode-level distinction between defining and setting?
      state.reg_acc = stack_peek();
      set_globally_oop(key, state.reg_acc);
      IPRINT("write-global-var ");
      IVALUE(key);
      IPRINT("                 ");
      IVALUE(state.reg_acc);
      break;
    }
    case BC_PUSH:
      stack_push(state.reg_acc);
      IPRINT("push           .oO stack-size=%d\n", stack.size);
      break;
    case BC_POP:
      state.reg_acc = stack_pop();
      IPRINT("pop            .oO stack-size=%d, ", stack.size);
      IVALUE(state.reg_acc);
      break;
    case BC_MAKE_LAMBDA: {
      fn_uint start_ip = read_label_address(&state);
      oop lambda_list = read_oop(&state);
      IPRINT("make-lambda %lu ", start_ip);
      IVALUE(lambda_list);
      oop code = LIST(make_smallint(start_ip), state.bytecode, state.oop_lookups);
      state.reg_acc = make_compiled_procedure(lambda_list, code, state.reg_frm);
      break;
    }
    case BC_CALL: {
      fn_uint arg_count = read_index(&state);
      apply_into_interpreter(arg_count, &state, NO);
      break;
    }
    case BC_TAIL_CALL: {
      fn_uint arg_count = read_index(&state);
      apply_into_interpreter(arg_count, &state, YES);
      break;
    }
    case BC_RETURN:
      IPRINT("return\n");
      oop retptr = stack_pop();
      if(is_nil(retptr)) {
        #ifdef INTERPRETER_DEBUG
        if (stack_size_before != stack.size) {
          printf("The stack is a mutant!");
          print_stack();
          exit(1);
        }
        #endif  // INTERPRETER_DEBUG
        return state.reg_acc;
      } else {
        apply_stack_pop();
        deserialize_retptr(retptr, &state);
      }
      // TODO: Restore previous state if reg_frm[2] != nil.
      break;
    default:
      printf("Fatal: Unknown byte code!\n");
      exit(1);
    }
  }
}

