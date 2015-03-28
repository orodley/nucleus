#include <assert.h>
#include <stdint.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

char *rt_nuc_str_to_c_str(nuc_val nuc_str)
{
	CHECK(nuc_str, STRING_TYPE);
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
		*result = 0;

		return result;
	}

	CHECK(list, CONS_TYPE);
	Cons *cons = (Cons *)REMOVE_LOWTAG(list);
	size_t len = rt_list_length(cons);
	uintptr_t *result = malloc(sizeof(*result) * (len + 1));

	size_t i = 0;
	result[i++] = *(uintptr_t *)REMOVE_LOWTAG(cons->car);

	for (;;) {
		nuc_val next = cons->cdr;
		if (next == NIL)
			break;
		
		CHECK(next, CONS_TYPE);
		cons = (Cons *)REMOVE_LOWTAG(next);
		CHECK(cons->car, FOREIGN_TYPE);
		result[i++] = *(uintptr_t *)REMOVE_LOWTAG(cons->car); // unbox the pointer
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

nuc_val rt_ptr_eq(nuc_val p1, nuc_val p2)
{
	CHECK(p1, FOREIGN_TYPE);
	CHECK(p2, FOREIGN_TYPE);

	void *ptr1 = *(void **)REMOVE_LOWTAG(p1);
	void *ptr2 = *(void **)REMOVE_LOWTAG(p2);

	return ptr1 == ptr2 ? TRUE : FALSE;
}
