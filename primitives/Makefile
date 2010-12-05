
HEADERS = *.h

tests: tests-bin
	./tests-bin

tests-bin: tests.c $(HEADERS)
	cc -o tests-bin tests.c

clean:
	rm -rf tests-bin
	rm -rf *.o

