
LIBS = -lrt -ldl
PROFILING_CFLAGS = -pg -fprofile-arcs
CFLAGS = -g -Wall -fPIC
HEADERS = *.h
RTOBJECTS = \
	arrays.o \
	byte-buffer.o \
	compiler.o \
	cons.o \
	continuations.o \
	data.o \
	debug.o \
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
	strings.o \
	symbols.o \
	utils.o \
	value.o \

TESTOBJECTS = \
	arrays-test.o \
	byte-buffer-test.o \
	compiler-test.o \
	cons-test.o \
	continuations-test.o \
	data-test.o \
	dispatcher-test.o \
	dl-test.o \
	eval-test.o \
	interpreter-test.o \
	lang-test.o \
	macros-test.o \
	memory-test.o \
	modules-test.o \
	objects-test.o \
	parser-test.o \
	pegs-parser-test.o \
	pegs-test.o \
	pprint-test.o \
	primitives-test.o \
	strings-test.o \
	tests.o \
	utils-test.o \
	value-test.o \


ALLOBJECTS = $(TESTOBJECTS) $(RTOBJECTS)

LISPTARGETS = \
	arrays-test.c \
	arrays.c \
	byte-buffer-test.c \
	byte-buffer.c \
	compiler-test.c \
	compiler.c \
	continuations-test.c \
	continuations.c \
	data-test.c \
	dispatcher-test.c \
	dispatcher.c \
	dl-test.c \
	lang-test.c \
	lang.c \
	macros-test.c \
	macros.c \
	objects-test.c \
	objects.c \
	parser-test.c \
	parser.c \
	modules.c \
	modules-test.c \
	pegs-parser-test.c \
	pegs-parser.c \
	pegs-test.c \
	pegs.c \
	pprint-test.c \
	pprint.c \
	primitives-test.c \
	utils-test.c \
	utils.c \

LISP = ./translate.scm

%.c: %.fn
	$(LISP) -o $@ $^

tests: fn
	./fn -t

fnrt.so: $(RTOBJECTS)
	$(CC) $(CFLAGS) -shared -o fnrt.so $(RTOBJECTS)

fn: $(TESTOBJECTS) fnrt.so tests.o
	$(CC) $(CFLAGS) -o fn $(TESTOBJECTS) /home/me/proj/fn/fnrt.so $(LIBS)

clean:
	rm -rf $(LISPTARGETS)
	rm -rf fn
	rm -rf *.o
	rm -rf *~
	rm -rf \#*\#
	$(MAKE) -C repl clean

fn.img: fn
	./fn -2 -s -x

REPL: $(RTOBJECTS)
	$(MAKE) -C repl

