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
