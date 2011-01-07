
#include "cons.h"

#define car(x) first(x)
#define cdr(x) rest(x)
#define caar(x) car(car(x))
#define cadr(x) car(cdr(x))
#define caddr(x) cadr(cdr(x))
#define cadddr(x) caddr(cdr(x))
