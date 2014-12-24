#include <stdio.h>
#include "nuc.h"

nuc_val rt_putchar(nuc_val c)
{
	// TODO: Type checking
	putchar((char)NUC_VAL_TO_INT(c));
	return c;
}
