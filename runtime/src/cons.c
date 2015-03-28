// Everything to do with conses

#include <stdlib.h>
#include "nuc.h"
#include "gc.h"

nuc_val rt_cons(nuc_val car, nuc_val cdr)
{
	Cons *c = gc_alloc(sizeof *c);
	c->car = car;
	c->cdr = cdr;

	return ((nuc_val)c) | CONS_LOWTAG;
}

nuc_val rt_set_car(nuc_val cons, nuc_val new_car)
{
	CHECK(cons, CONS_TYPE);

	Cons *c = (Cons *)REMOVE_LOWTAG(cons);
	c->car = new_car;

	return new_car;
}

nuc_val rt_set_cdr(nuc_val cons, nuc_val new_cdr)
{
	CHECK(cons, CONS_TYPE);

	Cons *c = (Cons *)REMOVE_LOWTAG(cons);
	c->cdr = new_cdr;

	return new_cdr;
}

size_t rt_list_length(Cons *cons)
{
	size_t len = 1;
	Cons *tmp = cons;
	for (;;) {
		nuc_val next = tmp->cdr;
		if (next == NIL)
			break;

		CHECK(next, CONS_TYPE);
		tmp = (Cons *)REMOVE_LOWTAG(next);
		len++;
	}

	return len;
}
