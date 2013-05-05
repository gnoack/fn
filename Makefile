LIBS = -lrt -ldl
PROFILING_CFLAGS = -pg -fprofile-arcs
CFLAGS = -g -Wall -fPIC

fn: fn.c runtime/fnrt.so
	$(CC) $(CFLAGS) -o fn fn.c runtime/fnrt.so $(LIBS)

clean:
	rm -rf fn
	rm -rf *.o
	rm -rf *~
	rm -rf \#*\#
	$(MAKE) -C repl clean
	$(MAKE) -C runtime clean

fn.img: fn
	./fn -2 -s -x

runtime/fnrt.so:
	$(MAKE) -C runtime -j8 fnrt.so

repl/readline.so:
	$(MAKE) -C repl

tests: fn fn.img
	$(MAKE) -C runtime ctests
	./fn -S tools/run-tests.fn `find . -name "*-test.fn"`
