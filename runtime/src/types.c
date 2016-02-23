#include <stdio.h>
#include <stdlib.h>
#include "assert.h"
#include "nuc.h"

nuc_val rt_type(nuc_val val)
{
	switch (LOWTAG(val)) {
	case FIXNUM_LOWTAG: return FIXNUM_TYPE;
	case STRUCT_LOWTAG: return STRUCT_TYPE;
	case CONS_LOWTAG: return CONS_TYPE;
	case SYMBOL_LOWTAG:  return SYMBOL_TYPE;
	case STRING_LOWTAG: return STRING_TYPE;
	case LAMBDA_LOWTAG: return LAMBDA_TYPE;
	case FOREIGN_LOWTAG: return FOREIGN_TYPE;
	case EXTTAG_LOWTAG:
		switch (EXTTAG(val)) {
		case DISCRETE_EXTTAG:
			switch (val) {
			case TRUE: case FALSE: return BOOL_TYPE;
			}

			printf("Got an invalid discrete value %lu\n", val);
			UNREACHABLE;
		case FLOAT_EXTTAG: return FLOAT_TYPE;
		}

		printf("Got an invalid exttag '%d'\n", (int)EXTTAG(val));
		UNREACHABLE;
	}

	printf("Got an invalid lowtag '%d'\n", (int)LOWTAG(val));
	UNREACHABLE;
}

static const char *type_name(nuc_val type)
{
#define CASE(x) case x##_TYPE: return #x;

	switch (type) {
		CASE(FIXNUM) CASE(STRUCT) CASE(CONS) CASE(SYMBOL) CASE(STRING)
		CASE(LAMBDA) CASE(FOREIGN) CASE(BOOL) CASE(FLOAT)
		default: printf("Got type %d (ft = %d)\n", (int)type, (int)FIXNUM_TYPE); UNREACHABLE;
	}

#undef CASE
}

// TODO: doesn't handle exttags
// TODO: use libbacktrace for better error messages
void rt_check_type_from_c(nuc_val val, nuc_val expected_type,
		const char *file, const char *func, int line)
{
	nuc_val given_type = rt_type(val);
	if (given_type != expected_type) {
		fprintf(stderr, "Wrong type given! Expected %s, got %s.\n",
				type_name(expected_type), type_name(given_type));
		if (file != NULL)
			fprintf(stderr, "Error occured at %s:%d in %s\n", file, line, func);

		exit(1);
	}
}

void rt_check_type(nuc_val val, nuc_val expected_type)
{
	rt_check_type_from_c(val, expected_type, NULL, NULL, 0);
}

void rt_check_struct_type(nuc_val val, int type_id)
{
	if (LOWTAG(val) != STRUCT_LOWTAG) {
		fprintf(stderr, "Wrong type given! Expected struct %d, got %s.\n",
				type_id, type_name(rt_type(val)));
		exit(1);
	}

	int actual_type_id = val >> 48;
	if (actual_type_id != type_id) {
		fprintf(stderr, "Wrong type given! Expected struct %d, got struct %d.\n",
				type_id, actual_type_id);
		exit(1);
	}
}
