
LIBS = -lreadline -lrt
CFLAGS = -g
HEADERS = *.h
OBJECTS = \
	arrays.o \
	compiler.o \
	cons.o \
	data.o \
	debug.o \
	dispatcher.o \
	eval.o \
	gc.o \
	interpreter.o \
	lang.o \
	macros.o \
	memory.o \
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
	compiler-test.o \
	cons-test.o \
	data-test.o \
	dispatcher-test.o \
	eval-test.o \
	interpreter-test.o \
	lang-test.o \
	macros-test.o \
	memory-test.o \
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


ALLOBJECTS = $(TESTOBJECTS) $(OBJECTS)

LISPTARGETS = \
	arrays-test.c \
	arrays.c \
	compiler-test.c \
	compiler.c \
	data-test.c \
	dispatcher-test.c \
	dispatcher.c \
	lang-test.c \
	lang.c \
	macros-test.c \
	macros.c \
	objects-test.c \
	objects.c \
	parser-test.c \
	parser.c \
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
	$(LISP) $*

tests: repl
	./repl -t

repl: $(LISPTARGETS) $(ALLOBJECTS)
	$(CC) $(CFLAGS) -o repl $(ALLOBJECTS) $(LIBS)

clean:
	rm -rf $(LISPTARGETS)
	rm -rf tests-bin
	rm -rf *.o
	rm -rf *~
	rm -rf \#*\#
