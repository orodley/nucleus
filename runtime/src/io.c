#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

nuc_val rt_write_char(nuc_val c)
{
	CHECK(c, FIXNUM_LOWTAG);
	putchar((char)NUC_VAL_TO_INT(c));
	return c;
}

nuc_val rt_write_string(nuc_val str_val)
{
	CHECK(str_val, STRING_LOWTAG);
	String *str = (String *)REMOVE_LOWTAG(str_val);
	fwrite(str->bytes, 1, str->length, stdout);

	return str_val;
}

nuc_val rt_write_string_ln(nuc_val str_val)
{
	CHECK(str_val, STRING_LOWTAG);
	rt_write_string(str_val);
	putchar('\n');

	return str_val;
}

nuc_val rt_read_char(nuc_val stream)
{
	return INT_TO_NUC_VAL(fgetc((FILE *)REMOVE_LOWTAG(stream)));
}
