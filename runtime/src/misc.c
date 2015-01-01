#include <assert.h>
#include "nuc.h"

nuc_val rt_panic(nuc_val message)
{
	(void)message;

	assert(!"Panic!");
}
