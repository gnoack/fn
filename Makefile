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
	rm -rf fn.img
	rm -rf */*.fnc *.fnc
	rm -rf */*.exprc
	$(MAKE) -C repl clean
	$(MAKE) -C runtime clean

fn.img: fn
	./fn -s -x

runtime/fnrt.so: force_look
	$(MAKE) -C runtime -j8 fnrt.so

repl/readline.so: force_look
	$(MAKE) -C repl

tests: fn fn.img
	$(MAKE) -C runtime ctests
	./fn -S fntools/run-tests.fn `find . -name "*-test.fn"`

force_look:
	@true
