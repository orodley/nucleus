#include <stdio.h>
#include <stdlib.h>
#include "assert.h"
#include "nuc.h"

nuc_val rt_type(nuc_val val)
{
	switch (LOWTAG(val)) {
	case FIXNUM_LOWTAG: return FIXNUM_TYPE;
	case CONS_LOWTAG: return CONS_TYPE;
	case SYMBOL_LOWTAG:  return SYMBOL_TYPE;
	case STRING_LOWTAG: return STRING_TYPE;
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

static const char *lowtag_name(int lowtag)
{
#define CASE(x) case x##_LOWTAG: return #x;

	switch (lowtag) {
		CASE(FIXNUM) CASE(CONS) CASE(SYMBOL)
		CASE(STRING) CASE(FOREIGN) CASE(EXTTAG)
		default: return "UNKNOWN";
	}

#undef CASE
}

// TODO: doesn't handle exttags
void rt_check_type(nuc_val val, int type_tag, const char *file, const char *func, int line)
{
	if (LOWTAG(val) != type_tag) {
		printf("Wrong type given! Expected %s, got %s.\nError occured at "
				"%s:%d in %s\n", lowtag_name(type_tag),
				lowtag_name((int)LOWTAG(val)), file, line, func);
		exit(1);
	}
}
