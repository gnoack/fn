#include <string.h>

#include "carcdr.h"
#include "cons.h"
#include "data.h"
#include "debug.h"
#include "gc.h"
#include "interpreter.h"
#include "memory.h"
#include "procedures.h"
#include "symbols.h"
#include "value.h"
#include "vars.h"

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


#define STACK_SIZE_IDX 1
#define STACK_HEADER_SIZE 2

oop make_stack(unsigned int max_size) {
  oop result = mem_alloc(STACK_HEADER_SIZE + max_size);
  MEM_SET(result, 0, symbols._stack);
  MEM_SET(result, 1, make_smallint(0));
  return result;
}


// Pointer to an interpreter state that's protected.
// This will be the first interpreter state on the stack.
interpreter_state_t* protected_interpreter_state = NULL;

void enumerate_interpreter_roots(void (*accept)(oop* place)) {
  if (protected_interpreter_state != NULL) {
    accept(&(protected_interpreter_state->reg_frm));
    accept(&(protected_interpreter_state->bytecode));
    accept(&(protected_interpreter_state->oop_lookups));

    // Hack: Because state->stack.stack points two oops into the
    // stack object, we need to correct that before telling the GC
    // to save it.
    protected_interpreter_state->stack.stack -= STACK_HEADER_SIZE;
    accept((oop*) &(protected_interpreter_state->stack.stack));
    protected_interpreter_state->stack.stack += STACK_HEADER_SIZE;
  }
}

void stack_push(stack_t* stack, oop value) {
  DEBUG_CHECK(stack->size < stack->max_size, "Stack too large, can't push.");
  stack->stack[stack->size] = value;
  stack->size++;
}

unsigned int stack_size(stack_t* stack) {
  return stack->size;
}

oop stack_peek(stack_t* stack) {
  DEBUG_CHECK(stack->size > 0, "Stack empty, can't peek.");
  return stack->stack[stack->size - 1];
}

// Peek at position n from top, 1 being the topmost element.
oop stack_peek_at(stack_t* stack, fn_uint n) {
  int idx = stack->size - n;
  DEBUG_CHECK(n >= 1, "1 is the lowermost peekable index.");
  DEBUG_CHECK(idx >= 0, "Can't peek that deep.");
  return stack->stack[idx];
}

oop stack_pop(stack_t* stack) {
  DEBUG_CHECK(stack->size > 0, "Stack empty, can't pop.");
  stack->size--;
  return stack->stack[stack->size];
}

// Shrink stack by n elements.
void stack_shrink(stack_t* stack, int n) {
  DEBUG_CHECK(stack->size >= n, "Can't shrink below stack size 0.");
  stack->size -= n;
}

// Convenience function to pop n elements from the stack and make them
// into a list.
oop stack_pop_list(stack_t* stack, fn_uint size) {
  oop list = NIL;
  while (size > 0) {
    list = make_cons(stack_pop(stack), list);
    size--;
  }
  return list;
}

#ifdef INTERPRETER_DEBUG
void print_stack(stack_t* stack) {
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
#define FRAME_STACK 5
#define FRAME_HEADER_SIZE 6

// Frame
// TODO: Does IP always need to be set?
oop make_frame(oop procedure, oop caller) {
  oop result = mem_alloc(FRAME_HEADER_SIZE + fn_argnum(procedure));
  MEM_SET(result, 0, symbols._frame);
  MEM_SET(result, 1, MEM_GET(procedure, CFN_ENV));  // next lexical env. (Needed?)
  MEM_SET(result, 2, caller);  // caller frame
  MEM_SET(result, 3, procedure);
  MEM_SET(result, 4, MEM_GET(procedure, CFN_IP));  // Initial IP.
  MEM_SET(result, 5, make_stack(fn_max_stack_depth(procedure)));
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
  
  oop stack = MEM_GET(frame, FRAME_STACK);
  state->stack.stack    = &stack.mem[STACK_HEADER_SIZE];
  state->stack.size     = get_smallint(MEM_GET(stack, STACK_SIZE_IDX));
  state->stack.max_size = mem_size(stack) - STACK_HEADER_SIZE;
}

inline static
void initialize_state_from_fn(oop frame, oop fn, interpreter_state_t* state) {
  DEBUG_CHECKV(is_compiled_lisp_procedure(fn), fn, "Need Lisp procedure.");
  DEBUG_CHECKV(is_frame(frame), frame, "Need frame");
  state->reg_frm = frame;
  state->ip = get_smallint(MEM_GET(fn, CFN_IP));
  state->bytecode = MEM_GET(fn, CFN_CODE);
  state->oop_lookups = MEM_GET(fn, CFN_LOOKUP_TABLE);

  // state->stack.stack does not point to the stack object, but instead it
  // points two oops into the stack object.  That way we can skip the header
  // and addressing becomes more simple.
  state->stack.max_size = fn_max_stack_depth(fn);  // TODO: Use macro?
  state->stack.stack = &(MEM_GET(frame, FRAME_STACK).mem[STACK_HEADER_SIZE]);
  state->stack.size = 0;
}

// Because interpreter_state_t is only a cache for the frame.
void writeback_to_frame(interpreter_state_t* state) {
  MEM_SET(state->reg_frm, FRAME_IP, make_smallint(state->ip));
  MEM_SET(MEM_GET(state->reg_frm, FRAME_STACK),
          STACK_SIZE_IDX, make_smallint(state->stack.size));
}

oop make_frame_from_stack(stack_t* stack, oop cfn, oop caller) {
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

void frame_set_var(oop frame, unsigned int index, oop value) {
  DEBUG_CHECK(index < frame_size(frame), "Index out of bounds.");
  MEM_SET(frame, FRAME_HEADER_SIZE + index, value);
}

static inline
oop frame_get_var(oop frame, unsigned int index) {
  DEBUG_CHECK(index < frame_size(frame), "Index out of bounds.");
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
    print_value(frame_get_var(obj, i));
  }
  printf(")");
}


// Apply compiled Lisp procedures by modifying
// the bytecode interpreter state and letting the interpreter do it.
void apply_into_interpreter(fn_uint arg_count, interpreter_state_t* state,
                            boolean tailcall) {
  stack_t* stack = &state->stack;
  oop cfn = stack_peek_at(stack, arg_count);
  if (is_compiled_lisp_procedure(cfn)) {
    oop caller = state->reg_frm;
    if (tailcall == YES) {
      caller = frame_caller(state->reg_frm);
    }
    oop env;
    if (fn_nested_args(cfn)) {
      oop args = stack_pop_list(stack, arg_count - 1);
      stack_shrink(stack, 1);
      env = make_frame_for_application(cfn, args, caller);
    } else {
      // TODO: Do quick calls with vararg procedures, too.
      env = make_frame_from_stack(stack, cfn, caller);
      CHECKV(arg_count == fn_argnum(cfn) + 1, cfn, "Bad argument number.");
      stack_shrink(stack, arg_count);
    }

    if (tailcall == NO) {
      // Only need to writeback when frame is not discarded.
      writeback_to_frame(state);
    }

    IPRINT("call %lu         .oO ", arg_count);

    // Modify the interpreter state.
    initialize_state_from_fn(env, cfn, state);
  } else if (is_native_procedure(cfn)) {
    // Pass a reference to the local stack frame to the native function.
    size_t argc = arg_count - 1;
    oop* argv = &(stack->stack[stack->size - argc]);
    oop result = apply_native_fn_directly(cfn, argv, argc, state->reg_frm);
    stack_shrink(stack, arg_count);
    stack_push(stack, result);
  } else {
    // Call recursively on the C stack.
    oop values = stack_pop_list(stack, arg_count);
    IPRINT("call %lu         .oO ", arg_count);
    IVALUE(values);
    stack_push(stack, apply_with_caller(values, state->reg_frm));
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


// Interpreter
oop interpret(oop frame, oop procedure) {
  DEBUG_CHECK(is_compiled_lisp_procedure(procedure),
              "Expected compiled procedure.");
  interpreter_state_t state;
  initialize_state_from_fn(frame, procedure, &state);

  if (protected_interpreter_state == NULL) {
    protected_interpreter_state = &state;
  }

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
      oop condition = stack_pop(&state.stack);
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
      stack_push(&state.stack, value);
      break;
    }
    case BC_READ_VAR: {
      fn_uint depth = read_index(&state);
      fn_uint index = read_index(&state);
      oop frame = nth_frame(state.reg_frm, depth);
      oop value = frame_get_var(frame, index);
      IPRINT("read-var %lu %lu   .oO ", depth, index);
      IVALUE(value);
      stack_push(&state.stack, value);
      break;
    }
    case BC_WRITE_VAR: {
      fn_uint depth = read_index(&state);
      fn_uint index = read_index(&state);
      oop frame = nth_frame(state.reg_frm, depth);
      oop value = stack_peek(&state.stack);
      frame_set_var(frame, index, value);
      IPRINT("write-var %lu %lu  // ", depth, index);
      IVALUE(value);
      break;
    }
    case BC_READ_GLOBAL_VAR: {
      oop var = read_oop(&state);
      oop value = var_get(var);
      IPRINT("load-global-var ");
      IVALUE(var_name(var));
      stack_push(&state.stack, value);
      break;
    }
    case BC_WRITE_GLOBAL_VAR: {
      oop var = read_oop(&state);
      // TODO: Make bytecode-level distinction between defining and setting?
      oop value = stack_peek(&state.stack);
      var_set(var, value);
      IPRINT("write-global-var ");
      IVALUE(var_name(var));
      IPRINT("                 ");
      IVALUE(value);
      break;
    }
    case BC_DISCARD: {
      stack_pop(&state.stack);
      IPRINT("discard        .oO stack-size=%u\n", state.stack.size);
      break;
    }
    case BC_MAKE_LAMBDA: {
      fn_uint start_ip = read_label_address(&state);
      fn_uint max_stack_depth = read_index(&state);
      oop lambda_list = read_oop(&state);
      IVALUE(lambda_list);
      stack_push(&state.stack,
                 make_compiled_procedure(lambda_list, state.reg_frm,
                                         state.bytecode,
                                         make_smallint(start_ip),
                                         state.oop_lookups,
                                         max_stack_depth));
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
      oop retvalue = stack_pop(&state.stack);
      DEBUG_CHECK(stack_size(&state.stack) == 0, "Assumed empty stack");
      oop caller = frame_caller(state.reg_frm);
      // TODO: Set caller to NIL?
      // TODO: Discard local stack before returning?  (Higher order procedures)
      if (likely(is_frame(caller))) {
        restore_from_frame(caller, &state);
        stack_push(&state.stack, retvalue);
        if (protected_interpreter_state == &state) {
          gc_run();
        }
      } else {
        // Leaving the interpreter loop.
        DEBUG_CHECKV(is_nil(caller), caller, "Assumed nil.");
        #ifdef INTERPRETER_DEBUG
        if (state.stack.size != 0) {
          printf("The stack had items before returning!");
          print_stack(&state.stack);
          exit(1);
        }
        #endif  // INTERPRETER_DEBUG
        if (protected_interpreter_state == &state) {
          protected_interpreter_state = NULL;
        }
        return retvalue;
      }
      break;
    case BC_TAIL_CALL_APPLY: {
      IPRINT("apply\n");
      oop arglist = stack_pop(&state.stack);
      oop fn = stack_pop(&state.stack);
      if (is_compiled_lisp_procedure(fn)) {
        oop caller = frame_caller(state.reg_frm);
        oop env = make_frame_for_application(fn, arglist, caller);
        initialize_state_from_fn(env, fn, &state);
      } else {
        // Call recursively on the C stack.
        stack_push(&state.stack,
                   apply_with_caller(make_cons(fn, arglist), state.reg_frm));
      }
      break;
    }
    default:
      printf("Fatal: Unknown byte code!\n");
      exit(1);
    }
  }
}

void actual_print_stack_trace(oop frame) {
  while (!is_nil(frame)) {
    printf(" - ");
    CHECKV(is_frame(frame), frame, "Expected frame.");

    print_frame(frame);
    frame = frame_caller(frame);

    printf("\n");
  }
}

void my_print_stack_trace() {
  oop frame = native_procedure_caller();
  if (is_nil(frame)) {
    printf("Outside of C function -- current frame is unknown!\n");
    return;
  }
  actual_print_stack_trace(frame);
}

#define MAX_STACK_SIZE 0x4000

void init_interpreter() {
  print_stack_trace = my_print_stack_trace;
  gc_register_persistent_refs(enumerate_interpreter_roots);
}
