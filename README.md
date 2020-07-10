# The fn programming language

`fn` is a bytecode interpreted dialect of Lisp, implemented in C.

This is a toy language, but not as much of a toy language as others:

 * It's implemented fully in C, in `fn` itself and a little bit of Guile Scheme
   for bootstrapping.
 * Includes a garbage collector ([`runtime/gc.c`](runtime/gc.c))
 * Strings, hash tables
 * Object orientation ([`runtime/objects.fn`](runtime/objects.fn))
 * Full quasiquote support
 * Bridge to C libraries ([`runtime/dl.c`](runtime/dl.c))
 * Test suite (true story, a good chunk of this was developed test driven)

I did learn a lot from looking at other people's language implementations, so
this comes with the full revision history.

## Better late than never!

I've been developing `fn` on and off in my spare time over many years (1) to
prove myself that I could do it and (2) to experiment with interesting features
in programming languages. I'm publishing it today in 2020.

## Interesting stuff to try

### Run a read-eval-print loop

`ifn` (interactive `fn`) is a script that builds and runs the `fn` repl.

```
gnoack:~/fn(master)$ ./ifn
make -C runtime -j8 fnrt.so
make[1]: Entering directory '/home/gnoack/fn/runtime'
make[1]: 'fnrt.so' is up to date.
make[1]: Leaving directory '/home/gnoack/fn/runtime'
make: 'fn.img' is up to date.
make -C repl
make[1]: Entering directory '/home/gnoack/fn/repl'
make[1]: Nothing to be done for 'all'.
make[1]: Leaving directory '/home/gnoack/fn/repl'
fn> (+ 1 2 3 4)
10
fn> It works
*PARSE ERROR
At: "works"
     ^
Error: ("Something left.")
fn>
Goodbye.
gnoack:~/fn(master)$
```

The REPL has tab completion for symbols.

### Disassemble a function's bytecode

```
fn> (load-
load-c-module  load-file
fn> (load-file "examples/dis.fn")
(<COMPILED-PROCEDURE mem-block-reader (mem-block index)> <COMPILED-PROCEDURE dis-code (bytecode startpos oop-table)> <COMPILED-PROCEDURE dis-fn (fn)> <COMPILED-PROCEDURE dis (fn)>)
fn> (dis dis)
(jump 55)
(read-global-var #<DefinedVar dis-fn=<COMPILED-PROCEDURE dis-fn (fn)>>)
(read-var 1 0)
(call 2)
(write-var 0 0)
(discard)
(jump 36)
(read-global-var #<DefinedVar println=<COMPILED-PROCEDURE println (&rest args)>>)
(read-var 1 0)
(call 1)
(call 2)
(discard)
(read-var 1 1)
(tail-call 1)
(return)
(make-lambda 20 2 ())
(write-var 0 1)
(discard)
(read-global-var #<DefinedVar catch=<COMPILED-PROCEDURE catch (thunk handler)>>)
(read-var 0 1)
(read-global-var #<DefinedVar identity=<COMPILED-PROCEDURE identity (x)>>)
(tail-call 3)
(return)
(make-lambda 6 3 (next-instruction! print-remaining!))
(load-value *undefined*)
(load-value *undefined*)
(tail-call 3)
(return)
(make-lambda 3 3 (fn))
(write-global-var #<DefinedVar dis=<COMPILED-PROCEDURE dis (fn)>>)
(return)
stop-iteration
fn>
```

### Introspect an object at runtime

```
fn> (load-file "examples/debug.fn")
(<COMPILED-PROCEDURE inspect (obj)> <COMPILED-PROCEDURE inspect-mem (obj)> <COMPILED-PROCEDURE inspect-generic (obj)>)
fn> (inspect load-file)

INSPECT <COMPILED-PROCEDURE load-file (filename)>:
  0: #<CompiledProcedure class>
  1: load-file
  2: (filename)
  3: 327682
  4: #<Frame () @1fd6aee07060>
  5: #<MEM-BLOCK>
  6: 3
  7: #[#<DefinedVar *module-caching*=true>, #<DefinedVar map=<COMPILED-PROCEDURE map (proc sequence)>>, #<DefinedVar eval=<COMPILED-PROCEDURE eval (expr)>>, #<DefinedVar read-all=<COMPILED-PROCEDURE read-all (in)>>, #<DefinedVar file->string=<COMPILED-PROCEDURE file->string (filename)>>, #<DefinedVar cached-load-file!=<COMPILED-PROCEDURE cached-load-file! (filename)>>, (filename), #<DefinedVar load-file=<COMPILED-PROCEDURE load-file (filename)>>]
Dig into: 4

INSPECT #<Frame () @1fd6aee07060>:
  0: #<Frame class>
  1: ()
  2: ()
  3: <COMPILED-PROCEDURE () ()>
  4: 0
  5: #<Stack>
Dig into: 0

INSPECT #<Frame class>:
  0: #<TYPE Frame class>
  1: #{}
  2: "Frame"
  3: #<TYPE Object>
Dig into: ()                      <---- ctrl+D
fn>
```

This works for all objects, including primitive objects like numbers.

### Run a Smalltalk REPL

This command runs a simple Smalltalk REPL

```
gnoack:~/fn(master)$ ./fn examples/load-st.fn
Arguments:
Methods available on cons cells:
filter:
each:
cdr
car
asString
isCons
map:
asStringInner
Entering repl...
ST> 1 methods
(isOdd > = < upto: / - + * isEven asString %)
ST> ', ' join: ((1 upto: 10) map: #asString)
"1, 2, 3, 4, 5, 6, 7, 8, 9"
ST> Goodbye!
gnoack:~/fn(master)$
```

There is a grammar of 86 lines translating Smalltalk syntax into Lisp, as well
as a minimal set of runtime methods implemented in native Lisp in
[`load-st.fn`](examples/load-st.fn) as well as more utilities implemented in
Smalltalk in [`st.st`](examples/st.st).

### Define and use your own little grammar

from [`examples/math.g`](examples/math.g):

```
// This is a grammar which evaluates math expressions while parsing.
//
// Usage:
// (load-file "examples/grammar-utils.fn")
// (def math (load-grammar! "examples/math.g"))
// (def parse-math (read-and-check-empty-remainder (@@ math-grammar exprAdd)))
// (parse-math "2*(3+4)")
grammar math-grammar ((base-grammar DIGIT)) {
  exprAdd ::= exprMul:a "+" exprAdd:b      => (+ a b)
            | exprMul;
  exprMul ::= exprPri:a "*" exprMul:b      => (* a b)
            | exprPri;
  exprPri ::= "(" exprAdd:a ")"            => a
            | decimal;
  decimal ::= DIGIT+:ds                    => (string->int (list->string ds));
}

```

## Dependencies

* libc
* libreadline for the repl
* GNU Guile for bootstrapping the interpreter

## Inspirations

This language is inspired by:

* the works of Alan Kay's Viewpoints Research Institute
  ([writings](http://vpri.org/writings.php)), in particular:
  * Ian Piumarta's "fonc" and COLA code. The type name `oop` for object pointers
    is inherited from the fonc codebase which was linked at some point from the
    `fonc` mailing list; I lost the code myself, but I think
    [this](https://github.com/chazu/idst) might be a copy.
  * Alessandro Warth's OMeta experiments and papers
* Adele Goldberg's and David Robson's Smalltalk-80 blue book
* Guido van Rossum's Python 2 implementation (the bytecode interpreters are very similar)
* Scheme
* Parsers based on Bryan Ford's [parsing expression grammars (PEGs) and packrat
   parsers](https://bford.info/packrat/)
  * Self-bootstrapping grammar for defining grammars at [examples/pegs.g](examples/pegs.g)
  * A grammar that converts Smalltalk syntax to Lisp at [examples/smalltalk.g](examples/smalltalk.g)
* Rich Hickey's Clojure, where I dug around in the source code
