
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

TESTOBJECTS = \
	cons-test.o \
	eval-test.o \
	parser-test.o \
	primitives-test.o \
	string-interning-test.o \
	strings-test.o \
	tests.o \
	value-test.o \

ALLOBJECTS = $(TESTOBJECTS) $(OBJECTS)

tests: tests-bin
	./tests-bin

tests-bin: $(ALLOBJECTS)
	$(CC) $(CFLAGS) -o tests-bin $(ALLOBJECTS)

clean:
	rm -rf tests-bin
	rm -rf *.o
	rm -rf *~
	rm -rf \#*\#
