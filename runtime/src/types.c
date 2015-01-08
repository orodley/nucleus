#include <stdio.h>
#include "assert.h"
#include "nuc.h"

nuc_val rt_type(nuc_val val)
{
	switch (LOWTAG(val)) {
	case FIXNUM_LOWTAG: return FIXNUM_TYPE;
	case CONS_LOWTAG: return CONS_TYPE;
	case SYMBOL_LOWTAG:  return SYMBOL_TYPE;
	case FOREIGN_LOWTAG: return FOREIGN_TYPE;
	case EXTTAG_LOWTAG:
		switch (EXTTAG(val)) {
		case DISCRETE_EXTTAG:
			switch (val) {
			case NIL: return NIL_TYPE;
			case TRUE: case FALSE: return BOOL_TYPE;
			}

			assert(!"This should never be reached");
		case FLOAT_EXTTAG: return FLOAT_TYPE;
		}
	}

	assert(!"This should never be reached");
}
