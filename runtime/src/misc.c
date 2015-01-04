#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "nuc.h"

nuc_val rt_panic(nuc_val message)
{
	String *str = (String *)REMOVE_LOWTAG(message);
	fputs("(panic! \"", stderr);
	fwrite(str->bytes, 1, str->length, stderr);
	fputs("\")\n", stderr);

	exit(-1);
}
