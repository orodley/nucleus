#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

nuc_val rt_write_char(nuc_val c)
{
	// TODO: Type checking
	putchar((char)NUC_VAL_TO_INT(c));
	return c;
}

nuc_val rt_write_string(nuc_val str_val)
{
	// TODO: Type checking
	String *str = (String *)REMOVE_LOWTAG(str_val);
	fwrite(str->bytes, 1, str->length, stdout);

	return str_val;
}

nuc_val rt_write_string_ln(nuc_val str_val)
{
	// TODO: Type checking
	rt_write_string(str_val);
	putchar('\n');

	return str_val;
}
