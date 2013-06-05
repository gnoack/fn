#include <string.h>

#include "carcdr.h"
#include "cons.h"
#include "data.h"
#include "debug.h"
#include "eval.h"  // TODO: Break dependency to lookup_globally().
#include "gc.h"
#include "interpreter.h"
#include "memory.h"
#include "procedures.h"
#include "symbols.h"
#include "value.h"

#define MAX_STACK_SIZE 0x4000

// #define INTERPRETER_DEBUG 1
// #define INTERPRETER_LOGGING 1

#ifdef INTERPRETER_DEBUG
#define DEBUG_CHECK(a,b) CHECK(a,b)
#define DEBUG_CHECKV(a,v,b) CHECKV(a,v,b)
#else  // INTERPRETER_DEBUG
#define DEBUG_CHECK(a,b)
#define DEBUG_CHECKV(a,v,b)
#endif  // INTERPRETER_DEBUG

#ifdef INTERPRETER_LOGGING
#define IPRINT(...) printf(__VA_ARGS__)
#define IVALUE(x) println_value(x)
#else
#define IPRINT(...)
#define IVALUE(x)
#endif  // INTERPRETER_LOGGING

typedef struct {
  oop stack[MAX_STACK_SIZE];
  unsigned int size;
} stack_t;

stack_t* stack;

// Pointer to an interpreter state that's protected.
// This will be the first interpreter state on the stack.
interpreter_state_t* protected_interpreter_state = NULL;

void enumerate_interpreter_roots(void (*accept)(oop* place)) {
  int i;
  for (i=0; i<stack->size; i++) {
    accept(&stack->stack[i]);
  }
  if (protected_interpreter_state != NULL) {
    accept(&(protected_interpreter_state->reg_frm));
    accept(&(protected_interpreter_state->bytecode));
    accept(&(protected_interpreter_state->oop_lookups));
  }
}

// Stack
void stack_push(oop value) {
  if (stack->size >= MAX_STACK_SIZE) {
    int i;
    for (i=0; i<MAX_STACK_SIZE; i++) {
      println_value(stack->stack[i]);
    }
  }
  CHECK(stack->size < MAX_STACK_SIZE, "Stack too large, can't push.");
  stack->stack[stack->size] = value;
  stack->size++;
}

unsigned int stack_size() {
  return stack->size;
}

oop stack_peek() {
  DEBUG_CHECK(stack->size > 0, "Stack empty, can't peek.");
  return stack->stack[stack->size - 1];
}

// Peek at position n from top, 1 being the topmost element.
oop stack_peek_at(fn_uint n) {
  int idx = stack->size - n;
  DEBUG_CHECK(n >= 1, "1 is the lowermost peekable index.");
  DEBUG_CHECK(idx >= 0, "Can't peek that deep.");
  return stack->stack[idx];
}

oop stack_pop() {
  DEBUG_CHECK(stack->size > 0, "Stack empty, can't pop.");
  stack->size--;
  return stack->stack[stack->size];
}

// Shrink stack by n elements.
void stack_shrink(int n) {
  DEBUG_CHECK(stack->size >= n, "Can't shrink below stack size 0.");
  stack->size -= n;
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
  for (i=stack->size-1; i>=0; i--) {
    printf(" [s] %03d ", i);
    println_value(stack->stack[i]);
  }
}
#endif  // INTERPRETER_DEBUG


#define FRAME_NEXT 1
#define FRAME_CALLER 2
#define FRAME_PROCEDURE 3
#define FRAME_IP 4
#define FRAME_HEADER_SIZE 5

// Frame
// TODO: Does IP always need to be set?
oop make_frame(oop procedure, oop caller) {
  oop result = mem_alloc(FRAME_HEADER_SIZE + fn_argnum(procedure));
  MEM_SET(result, 0, symbols._frame);
  MEM_SET(result, 1, fn_env(procedure));  // next lexical environment. (Needed?)
  MEM_SET(result, 2, caller);  // caller frame
  MEM_SET(result, 3, procedure);
  MEM_SET(result, 4, MEM_GET(procedure, CFN_IP));  // Initial IP.
  return result;
}

void restore_from_frame(oop frame, interpreter_state_t* state) {
  DEBUG_CHECKV(is_frame(frame), frame,
               "Needs to be a frame for deserializing it.");
  oop proc = MEM_GET(frame, FRAME_PROCEDURE);
  state->reg_frm     = frame;
  state->ip          = get_smallint(MEM_GET(frame, FRAME_IP));
  state->bytecode    = MEM_GET(proc, CFN_CODE);
  state->oop_lookups = MEM_GET(proc, CFN_LOOKUP_TABLE);
}

// Because interpreter_state_t is only a cache for the frame.
void writeback_to_frame(interpreter_state_t* state) {
  MEM_SET(state->reg_frm, FRAME_IP, make_smallint(state->ip));
}

oop make_frame_from_stack(oop cfn, oop caller) {
  fn_uint function_argnum = fn_argnum(cfn);
  oop result = make_frame(cfn, caller);
  memcpy(&(result.mem[FRAME_HEADER_SIZE]),
         &(stack->stack[stack->size - function_argnum]),
         sizeof(oop) * function_argnum);
  return result;
}

fn_uint frame_size(oop frame) {
  return fn_argnum(MEM_GET(frame, FRAME_PROCEDURE));
}

oop frame_caller(oop frame) {
  return MEM_GET(frame, FRAME_CALLER);
}

oop nth_frame(oop frame, unsigned int depth) {
  while (depth > 0) {
    frame = MEM_GET(frame, FRAME_NEXT);
    depth--;
  }
  return frame;
}

void set_var(oop frame, unsigned int index, oop value) {
  DEBUG_CHECK(0 <= index && index < frame_size(frame),
              "Index out of bounds.");
  MEM_SET(frame, FRAME_HEADER_SIZE + index, value);
}

oop get_var(oop frame, unsigned int index) {
  DEBUG_CHECK(0 <= index && index < frame_size(frame),
              "Index out of bounds.");
  return MEM_GET(frame, FRAME_HEADER_SIZE + index);
}

boolean is_frame(oop obj) {
  return TO_BOOL(is_mem(obj) && value_eq(obj.mem[0], symbols._frame));
}

void print_frame(oop obj) {
  CHECKV(is_frame(obj), obj, "Must be frame.");
  printf("(");
  print_value(fn_name(MEM_GET(obj, FRAME_PROCEDURE)));
  int i;
  for (i=0; i<frame_size(obj); i++) {
    printf(" ");
    print_value(get_var(obj, i));
  }
  printf(")");
}


// Apply compiled Lisp procedures by modifying
// the bytecode interpreter state and letting the interpreter do it.
void apply_into_interpreter(fn_uint arg_count, interpreter_state_t* state,
                            boolean tailcall) {
  oop cfn = stack_peek_at(arg_count);
  if (is_compiled_lisp_procedure(cfn)) {
    oop caller = state->reg_frm;
    if (tailcall == YES) {
      caller = frame_caller(state->reg_frm);
    }
    oop env;
    if (fn_nested_args(cfn)) {
      oop args = stack_pop_list(arg_count - 1);
      stack_shrink(1);
      env = make_frame_for_application(cfn, args, caller);
    } else {
      // TODO: Do quick calls with vararg procedures, too.
      env = make_frame_from_stack(cfn, caller);
      CHECK(arg_count == fn_argnum(cfn) + 1, "Bad argument number.");
      stack_shrink(arg_count);
    }

    if (tailcall == NO) {
      // Only need to writeback when frame is not discarded.
      writeback_to_frame(state);
    }

    IPRINT("call %lu         .oO ", arg_count);

    // Modify the interpreter state.

    // Data.
    state->reg_frm = env;

    // Position.
    state->ip = get_smallint(MEM_GET(cfn, CFN_IP));
    state->bytecode = MEM_GET(cfn, CFN_CODE);
    state->oop_lookups = MEM_GET(cfn, CFN_LOOKUP_TABLE);
  } else {
    // Call recursively on the C stack.
    oop values = stack_pop_list(arg_count);
    IPRINT("call %lu         .oO ", arg_count);
    IVALUE(values);
    stack_push(apply_with_caller(values, state->reg_frm));
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
  // TODO: Depends on memory layout in arrays.
  return MEM_GET(state->oop_lookups, 1 + read_byte(state));
}


// Continuations
oop make_continuation(interpreter_state_t* state) {
  writeback_to_frame(state);
  oop result = mem_alloc(3);
  MEM_SET(result, 0, symbols._continuation);
  MEM_SET(result, 1, state->reg_frm);
  MEM_SET(result, 2, make_smallint(stack->size));
  return result;
}

boolean is_continuation_valid(oop continuation) {
  return TO_BOOL(!is_nil(MEM_GET(continuation, 1)));
}

void invalidate_continuation(oop continuation) {
  CHECKV(is_continuation_valid(continuation), continuation,
         "Not a valid continuation.");
  MEM_SET(continuation, 1, NIL);
}

boolean is_continuation(oop continuation) {
  return value_eq(MEM_GET(continuation, 0), symbols._continuation);
}

void restore_continuation(oop continuation, interpreter_state_t* state) {
  CHECKV(is_continuation_valid(continuation), continuation,
         "Not a valid continuation.");
  restore_from_frame(MEM_GET(continuation, 1), state);
  stack->size = get_smallint(MEM_GET(continuation, 2));
}


// Interpreter
oop interpret(oop frame, oop procedure) {
  DEBUG_CHECK(is_compiled_lisp_procedure(procedure),
              "Expected compiled procedure.");
  interpreter_state_t state;
  state.reg_frm = frame;
  state.ip = get_smallint(MEM_GET(procedure, CFN_IP));
  state.bytecode = MEM_GET(procedure, CFN_CODE);
  state.oop_lookups = MEM_GET(procedure, CFN_LOOKUP_TABLE);

  if (protected_interpreter_state == NULL) {
    protected_interpreter_state = &state;
  }

  #ifdef INTERPRETER_DEBUG
  unsigned int stack_size_before = stack->size;
  #endif  // INTERPRETER_DEBUG

  for (;;) {
    IPRINT("[i] %5lu: ", state.ip);
    unsigned char operation = read_byte(&state);
    switch (operation) {
    case BC_JUMP:
      state.ip = read_label_address(&state);
      IPRINT("jump %lu\n", state.ip);
      break;
    case BC_JUMP_IF_TRUE: {
      fn_uint address = read_label_address(&state);
      oop condition = stack_pop();
      if (value_eq(condition, symbols._true)) {
        IPRINT("jump-if-true %lu (taken)\n", address);
        state.ip = address;
      } else {
        CHECK(value_eq(condition, symbols._false),
              "Condition evaluated to non-boolean value.");
        IPRINT("jump-if-true %lu (not taken)\n", address);
      }
      break;
    }
    case BC_LOAD_VALUE: {
      oop value = read_oop(&state);
      IPRINT("load-value ");
      IVALUE(value);
      stack_push(value);
      break;
    }
    case BC_READ_VAR: {
      fn_uint depth = read_index(&state);
      fn_uint index = read_index(&state);
      oop frame = nth_frame(state.reg_frm, depth);
      oop value = get_var(frame, index);
      IPRINT("read-var %lu %lu   .oO ", depth, index);
      IVALUE(value);
      stack_push(value);
      break;
    }
    case BC_WRITE_VAR: {
      fn_uint depth = read_index(&state);
      fn_uint index = read_index(&state);
      oop frame = nth_frame(state.reg_frm, depth);
      oop value = stack_peek();
      set_var(frame, index, value);
      IPRINT("write-var %lu %lu  // ", depth, index);
      IVALUE(value);
      break;
    }
    case BC_READ_GLOBAL_VAR: {
      oop key = read_oop(&state);
      oop value = lookup_globally(key);
      IPRINT("load-global-var ");
      IVALUE(key);
      stack_push(value);
      break;
    }
    case BC_WRITE_GLOBAL_VAR: {
      oop key = read_oop(&state);
      // TODO: Make bytecode-level distinction between defining and setting?
      oop value = stack_peek();
      set_globally_oop(key, value);
      IPRINT("write-global-var ");
      IVALUE(key);
      IPRINT("                 ");
      IVALUE(value);
      break;
    }
    case BC_DISCARD: {
      stack_pop();
      IPRINT("discard        .oO stack-size=%d\n", stack->size);
      break;
    }
    case BC_MAKE_LAMBDA: {
      fn_uint start_ip = read_label_address(&state);
      oop lambda_list = read_oop(&state);
      IPRINT("make-lambda %lu ", start_ip);
      IVALUE(lambda_list);
      stack_push(make_compiled_procedure(lambda_list, state.reg_frm,
                                         state.bytecode,
                                         make_smallint(start_ip),
                                         state.oop_lookups));
      break;
    }
    case BC_CALL: {
      fn_uint arg_count = read_index(&state);
      apply_into_interpreter(arg_count, &state, NO);
      if (protected_interpreter_state == &state) {
        gc_run();
      }
      break;
    }
    case BC_TAIL_CALL: {
      fn_uint arg_count = read_index(&state);
      apply_into_interpreter(arg_count, &state, YES);
      break;
    }
    case BC_RETURN:
      IPRINT("return\n");
      oop retvalue = stack_pop();
      oop caller = frame_caller(state.reg_frm);
      // TODO: Set caller to NIL?
      if (likely(is_frame(caller))) {
        stack_push(retvalue);
        restore_from_frame(frame_caller(state.reg_frm), &state);
        if (protected_interpreter_state == &state) {
          gc_run();
        }
      } else {
        // Leaving the interpreter loop.
        DEBUG_CHECKV(is_nil(caller) || is_dframe(caller), caller,
                     "Assumed nil or dframe.");
        #ifdef INTERPRETER_DEBUG
        if (stack_size_before != stack->size) {
          printf("The stack is a mutant!");
          printf("Old stack size: %d\n", stack_size_before);
          printf("New stack size: %d\n", stack->size);
          print_stack();
          exit(1);
        }
        #endif  // INTERPRETER_DEBUG
        if (protected_interpreter_state == &state) {
          protected_interpreter_state = NULL;
        }
        return retvalue;
      }
      break;
    case BC_CALL_CC: {
      IPRINT("call/cc\n");
      // TODO: Optimize proc pop/push.
      oop proc = stack_pop();
      oop continuation = make_continuation(&state);
      stack_push(continuation);
      stack_push(proc);
      stack_push(continuation);
      apply_into_interpreter(2, &state, NO);
      break;
    }
    case BC_INVALIDATE_CONTINUATION: {
      IPRINT("invalidate-continuation\n");
      oop return_value = stack_pop();
      oop continuation = stack_pop();
      stack_push(return_value);
      invalidate_continuation(continuation);
      break;
    }
    case BC_RESTORE_CONTINUATION: {
      IPRINT("restore-continuation\n");
      oop return_value = stack_pop();
      oop continuation = stack_pop();
      restore_continuation(continuation, &state);
      stack_push(continuation);
      stack_push(return_value);
      break;
    }
    case BC_TAIL_CALL_APPLY: {
      IPRINT("apply\n");
      fn_uint arg_count = 1;
      oop arglist = stack_pop();
      while (!is_nil(arglist)) {
        stack_push(first(arglist));
        arglist = rest(arglist);
        arg_count++;
      }
      apply_into_interpreter(arg_count, &state, YES);
      break;
    }
    default:
      printf("Fatal: Unknown byte code!\n");
      exit(1);
    }
  }
}

void my_print_stack_trace() {
  oop frame = native_procedure_caller();
  if (is_nil(frame)) {
    printf("Outside of C function -- current frame is unknown!\n");
    return;
  }

  while (!is_nil(frame)) {
    printf(" - ");
    if (is_frame(frame)) {
      print_frame(frame);
      frame = frame_caller(frame);
    } else {
      CHECKV(is_dframe(frame), frame, "Expected frame or dframe");
      print_dframe(frame);
      frame = dframe_caller(frame);
    }
    printf("\n");
  }
}

void init_interpreter() {
  print_stack_trace = my_print_stack_trace;
  stack = malloc(sizeof(stack_t));
  int i;
  for (i=0; i<MAX_STACK_SIZE; i++) {
    stack->stack[i] = NIL;
  }
  stack->size = 0;
  gc_register_persistent_refs(enumerate_interpreter_roots);
}
