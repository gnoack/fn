
CFLAGS = -g
HEADERS = *.h
OBJECTS = \
	cons.o \
	env.o \
	eval.o \
	lang.o \
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
	parser-test.o \
	primitives-test.o \
	string-interning-test.o \
	strings-test.o \
	tests.o \
	utils-test.o \
	value-test.o \

ALLOBJECTS = $(TESTOBJECTS) $(OBJECTS)

tests: tests-bin
	./tests-bin

tests-bin: $(ALLOBJECTS)
	$(CC) $(CFLAGS) -o tests-bin $(ALLOBJECTS)

utils.c: utils.fn
	sbcl --load translate.cl --eval "(run)" utils

utils-test.c: utils-test.fn
	sbcl --load translate.cl --eval "(run)" utils-test

lang.c: lang.fn
	sbcl --load translate.cl --eval "(run)" lang

lang-test.c: lang-test.fn
	sbcl --load translate.cl --eval "(run)" lang-test

clean:
	rm -rf utils-test.c utils.c
	rm -rf lang-test.c lang.c
	rm -rf tests-bin
	rm -rf *.o
	rm -rf *~
	rm -rf \#*\#
