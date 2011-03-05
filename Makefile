
CFLAGS = -g
HEADERS = *.h
OBJECTS = \
	cons.o \
	env.o \
	eval.o \
	lang.o \
	macros.o \
	parser.o \
	primitives.o \
	procedures.o \
	string-interning.o \
	strings.o \
	symbols.o \
	utils.o \
	value.o \

TESTOBJECTS = \
	cons-test.o \
	env-test.o \
	eval-test.o \
	lang-test.o \
	macros-test.o \
	parser-test.o \
	primitives-test.o \
	string-interning-test.o \
	strings-test.o \
	tests.o \
	utils-test.o \
	value-test.o \

ALLOBJECTS = $(TESTOBJECTS) $(OBJECTS)

LISPTARGETS = \
	lang.c \
	lang-test.c \
	utils.c \
	utils-test.c \
	primitives-test.c

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
