#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

nuc_val rt_write_char_to_stream(nuc_val stream, nuc_val c)
{
	CHECK(stream, FOREIGN_LOWTAG);
	CHECK(c, FIXNUM_LOWTAG);
	fputc((char)NUC_VAL_TO_INT(c), (FILE *)REMOVE_LOWTAG(stream));
	return c;
}

nuc_val rt_write_string_to_stream(nuc_val stream, nuc_val str_val)
{
	CHECK(stream, FOREIGN_LOWTAG);
	CHECK(str_val, STRING_LOWTAG);
	String *str = (String *)REMOVE_LOWTAG(str_val);
	fwrite(str->bytes, 1, str->length, (FILE *)REMOVE_LOWTAG(stream));

	return str_val;
}

nuc_val rt_write_string_ln_to_stream(nuc_val stream, nuc_val str_val)
{
	rt_write_string_to_stream(stream, str_val);
	fputc('\n', (FILE *)REMOVE_LOWTAG(stream));

	return str_val;
}

nuc_val rt_read_char_from_stream(nuc_val stream)
{
	CHECK(stream, FOREIGN_LOWTAG);
	return INT_TO_NUC_VAL(fgetc((FILE *)REMOVE_LOWTAG(stream)));
}
