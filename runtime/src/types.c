#include <stdio.h>
#include "assert.h"
#include "nuc.h"

nuc_val type(nuc_val val)
{
	switch (LOWTAG(val))
	{
	case FIXNUM_LOWTAG: return FIXNUM_TYPE;
	case CONS_LOWTAG: return CONS_TYPE;
	}

	assert(!"This should never be reached");
	return NIL;
}
