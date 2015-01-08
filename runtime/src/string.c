#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

nuc_val rt_make_string(size_t length, char *bytes)
{
	String *str = gc_alloc(sizeof *str + length);
	str->length = length;
	strncpy(str->bytes, bytes, length);

	return ((nuc_val)str) | STRING_LOWTAG;
}

nuc_val rt_char_list_to_string(nuc_val char_list)
{
	if (char_list == NIL) {
		char nothing = '\0';
		return rt_make_string(0, &nothing);
	}

	// TODO: type checking
	Cons *cons = (Cons *)REMOVE_LOWTAG(char_list);
	size_t len = rt_list_length(cons);

	char *str_bytes = malloc(len);
	str_bytes[0] = (char)NUC_VAL_TO_INT(cons->car);
	size_t i = 1;
	Cons *tmp = cons;
	for (;;) {
		nuc_val next = tmp->cdr;
		if (next == NIL)
			break;

		// TODO: type checking
		tmp = (Cons *)REMOVE_LOWTAG(next);
		str_bytes[i++] = (char)NUC_VAL_TO_INT(tmp->car);
	}

	nuc_val string = rt_make_string(len, str_bytes);
	free(str_bytes);
	return string;
}
