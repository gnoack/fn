
CFLAGS = -g
HEADERS = *.h
OBJECTS = \
	cons.o \
	env.o \
	eval.o \
	parser.o \
	primitives.o \
	procedures.o \
	string-interning.o \
	strings.o \
	symbols.o \
	value.o \
	utils.o \

TESTOBJECTS = \
	cons-test.o \
	eval-test.o \
	env-test.o \
	parser-test.o \
	primitives-test.o \
	string-interning-test.o \
	strings-test.o \
	tests.o \
	value-test.o \
	utils-test.o \

ALLOBJECTS = $(TESTOBJECTS) $(OBJECTS)

tests: tests-bin
	./tests-bin

tests-bin: $(ALLOBJECTS)
	$(CC) $(CFLAGS) -o tests-bin $(ALLOBJECTS)

utils.c: utils.fn
	sbcl --load translate.cl utils

utils-test.c: utils-test.fn
	sbcl --load translate.cl utils-test

clean:
	rm -rf utils-test.c utils.c
	rm -rf tests-bin
	rm -rf *.o
	rm -rf *~
	rm -rf \#*\#
