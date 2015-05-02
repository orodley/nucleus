#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

String *rt_make_string(size_t length, char *bytes)
{
	String *str = gc_alloc(sizeof *str + length + 1);
	str->length = length;
	strncpy(str->bytes, bytes, length);
	str->bytes[length] = '\0';

	return str;
}

uint64_t rt_string_length(String *string)
{
	return string->length;
}

char rt_char_at(String *string, unsigned int index)
{
	if (index >= string->length) {
		fprintf(stderr, "Index %zd out of bounds for string\n", index);
		exit(1);
	}

	return string->bytes[index];
}

nuc_val rt_char_list_to_string(nuc_val char_list)
{
	if (char_list == NIL) {
		char nothing = '\0';
		return ((nuc_val)rt_make_string(0, &nothing)) | STRING_LOWTAG;
	}

	CHECK(char_list, CONS_TYPE);
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

		CHECK(next, CONS_TYPE);
		tmp = (Cons *)REMOVE_LOWTAG(next);
		str_bytes[i++] = (char)NUC_VAL_TO_INT(tmp->car);
	}

	nuc_val string = ((nuc_val)rt_make_string(len, str_bytes)) | STRING_LOWTAG;
	free(str_bytes);
	return string;
}

nuc_val rt_string_to_char_list(nuc_val string)
{
	CHECK(string, STRING_TYPE);

	String *str = (String *)REMOVE_LOWTAG(string);
	if (str->length == 0)
		return NIL;

	Cons *cons = gc_alloc(sizeof *cons);
	Cons *head = cons;
	cons->car = INT_TO_NUC_VAL(str->bytes[0]);
	for (size_t i = 1; i < str->length; i++) {
		Cons *next = gc_alloc(sizeof *cons);
		next->car = INT_TO_NUC_VAL(str->bytes[i]);
		cons->cdr = ((nuc_val)next) | CONS_LOWTAG;
		cons = next;
	}
	cons->cdr = NIL;

	return ((nuc_val)head) | CONS_LOWTAG;
}

String *rt_substring(String *str, uint64_t start, uint64_t end)
{
	size_t length = end - start;
	char *bytes = str->bytes + start;

	return rt_make_string(length, bytes);
}
