// Everything to do with conses

#include <stdlib.h>
#include "nuc.h"
#include "gc.h"

nuc_val cons(nuc_val car, nuc_val cdr)
{
	Cons *c = gc_alloc(sizeof *c);
	c->car = car;
	c->cdr = cdr;

	return ((nuc_val)c) | CONS_LOWTAG;
}
