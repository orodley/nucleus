// Everything to do with conses

#include <stdlib.h>
#include "nuc.h"
#include "gc.h"

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
