
MODULES = readline.so

all: $(MODULES)

clean:
	rm -f *.o *.so

CFLAGS = -I.. -fPIC

readline.so: readline.o
	cc $(CFLAGS) -shared -o readline.so readline.o -lreadline

