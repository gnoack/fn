LIBS = -lrt -ldl
PROFILING_CFLAGS = -pg -fprofile-arcs
CFLAGS = -g -Wall -fPIC

# -rdynamic exports the runtime's symbols to dlopen()ed C modules.
fn: fn.c runtime/fnrt.a
	$(CC) $(CFLAGS) -rdynamic -o fn fn.c runtime/fnrt.a $(LIBS)

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

runtime/fnrt.a: force_look
	$(MAKE) -C runtime -j8 fnrt.a

repl/readline.so: force_look
	$(MAKE) -C repl

tests: fn fn.img
	$(MAKE) -C runtime ctests
	./fn -S fntools/run-tests.fn `find . -name "*-test.fn"`

force_look:
	@true
