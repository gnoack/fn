
CFLAGS = -g
HEADERS = *.h
OBJECTS = \
	arrays.o \
	compiler.o \
	cons.o \
	env.o \
	eval.o \
	lang.o \
	macros.o \
	maps.o \
	memory.o \
	objects.o \
	parser.o \
	pegs-parser.o \
	pegs.o \
	pprint.o \
	primitives.o \
	procedures.o \
	string-interning.o \
	strings.o \
	symbols.o \
	utils.o \
	value.o \
	x86.o \

TESTOBJECTS = \
	arrays-test.o \
	compiler-test.o \
	cons-test.o \
	env-test.o \
	eval-test.o \
	lang-test.o \
	macros-test.o \
	maps-test.o \
	memory-test.o \
	objects-test.o \
	parser-test.o \
	pegs-parser-test.o \
	pegs-test.o \
	pprint-test.o \
	primitives-test.o \
	string-interning-test.o \
	strings-test.o \
	tests.o \
	utils-test.o \
	value-test.o \
	x86-test.o \


ALLOBJECTS = $(TESTOBJECTS) $(OBJECTS)

LISPTARGETS = \
	arrays-test.c \
	arrays.c \
	compiler-test.c \
	compiler.c \
	lang-test.c \
	lang.c \
	macros-test.c \
	macros.c \
	maps-test.c \
	maps.c \
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
	x86-test.c \
	x86.c \

CLFLAGS = --noinform --noprint --disable-debugger
LISP = sbcl $(CLFLAGS) --load translate.cl --eval "(run)"

%.c: %.fn
	$(LISP) $*

tests: tests-bin
	./tests-bin

tests-bin: $(LISPTARGETS) $(ALLOBJECTS)
	$(CC) $(CFLAGS) -o tests-bin $(ALLOBJECTS)

clean:
	rm -rf $(LISPTARGETS)
	rm -rf tests-bin
	rm -rf *.o
	rm -rf *~
	rm -rf \#*\#
