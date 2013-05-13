LIBS = -lrt -ldl
PROFILING_CFLAGS = -pg -fprofile-arcs
CFLAGS = -I.. -g -Wall -fPIC
RTOBJECTS = \
	arrays.o \
	byte-buffer.o \
	compiler.o \
	cons.o \
	continuations.o \
	data.o \
	debug.o \
	deserialize.o \
	dispatcher.o \
	dl.o \
	eval.o \
	gc.o \
	interpreter.o \
	lang.o \
	macros.o \
	memory.o \
	modules.o \
	objects.o \
	parser.o \
	pegs-parser.o \
	pegs.o \
	pprint.o \
	primitives.o \
	procedures.o \
	runtime.o \
	serialize.o \
	strings.o \
	symbols.o \
	utils.o \
	value.o \

TESTOBJECTS = \
	cons-test.o \
	eval-test.o \
	interpreter-test.o \
	memory-test.o \
	strings-test.o \
	value-test.o \

LISPTARGETS = \
	arrays.c \
	byte-buffer.c \
	compiler.c \
	continuations.c \
	dispatcher.c \
	lang.c \
	macros.c \
	objects.c \
	modules.c \
	parser.c \
	pegs-parser.c \
	pegs.c \
	pprint.c \
	serialize.c \
	utils.c \

LISP = ../tools/translate.scm

fnrt.so: $(RTOBJECTS)
	$(CC) $(CFLAGS) -shared -o fnrt.so $(RTOBJECTS)

%.c: %.fn
	$(LISP) -o $@ $^

run-c-tests: fnrt.so $(TESTOBJECTS)
	$(CC) $(CFLAGS) -o run-c-tests tests.c $(TESTOBJECTS) fnrt.so $(LIBS)

ctests: run-c-tests
	LD_LIBRARY_PATH=`pwd` ./run-c-tests

clean:
	rm -rf $(LISPTARGETS)
	rm -rf run-c-tests
	rm -rf *.o
	rm -rf *.so
	rm -rf *~
	rm -rf \#*\#

