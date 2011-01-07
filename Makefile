
HEADERS = *.h
OBJECTS = value.o string-interning.o cons.o eval.o procedures.o symbols.o
TESTOBJECTS = eval-test.o string-interning-test.o cons-test.o value-test.o tests.o
ALLOBJECTS = $(TESTOBJECTS) $(OBJECTS)

tests: tests-bin
	./tests-bin

tests-bin: $(ALLOBJECTS)
	$(CC) -o tests-bin $(ALLOBJECTS)

clean:
	rm -rf tests-bin
	rm -rf *.o
	rm -rf *~
	rm -rf \#*\#
