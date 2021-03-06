#ifndef _CARCDR_H_
#define _CARCDR_H_

#include "cons.h"

#define car(x) first(x)
#define cdr(x) rest(x)
#define caar(x) car(car(x))
#define cadr(x) car(cdr(x))
#define cddr(x) cdr(cdr(x))
#define caddr(x) cadr(cdr(x))
#define cdddr(x) cddr(cdr(x))
#define cadddr(x) caddr(cdr(x))
#define cddddr(x) cdddr(cdr(x))
#define caddddr(x) cadddr(cdr(x))
#define cdddddr(x) cadddr(cdr(x))

#endif  // _CARCDR_H_
