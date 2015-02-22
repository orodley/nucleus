#include <assert.h>
#include <stdint.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

char *rt_nuc_str_to_c_str(nuc_val nuc_str)
{
	CHECK(nuc_str, STRING_LOWTAG);
	String *str = (String *)REMOVE_LOWTAG(nuc_str);

	char *c_str = gc_alloc(str->length + 1);
	strncpy(c_str, str->bytes, str->length);
	c_str[str->length] = '\0';
	return c_str;
}

uint64_t *rt_list_to_array(nuc_val list)
{
	if (list == NIL) {
		uint64_t *result = malloc(sizeof *result);
		result[0] = 0;

		return result;
	}

	CHECK(list, CONS_LOWTAG);
	Cons *cons = (Cons *)REMOVE_LOWTAG(list);
	size_t len = rt_list_length(cons);
	uint64_t *result = malloc(sizeof(*result) * (len + 1));

	size_t i = 0;
	result[i++] = REMOVE_LOWTAG(cons->car);

	for (;;) {
		nuc_val next = cons->cdr;
		if (next == NIL)
			break;
		
		CHECK(next, CONS_LOWTAG);
		cons = (Cons *)REMOVE_LOWTAG(next);
		result[i++] = REMOVE_LOWTAG(cons->car);
	}

	return result;
}

static void *global_null = NULL;

nuc_val rt_get_null(void)
{
	assert(LOWTAG(&global_null) == 0);
	return ((nuc_val)&global_null) | FOREIGN_LOWTAG;
}

nuc_val rt_null_p(nuc_val ptr_val)
{
	if (LOWTAG(ptr_val) != FOREIGN_LOWTAG)
		return FALSE;

	void **ptr = (void **)REMOVE_LOWTAG(ptr_val);
	return *ptr == NULL ? TRUE : FALSE;
}
