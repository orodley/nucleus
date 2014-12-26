#include <assert.h>
#include <stdint.h>
#include "nuc.h"

nuc_val rt_make_lambda(uint64_t func_pointer)
{
	assert(LOWTAG(func_pointer) == 0);

	return func_pointer | LAMBDA_LOWTAG;
}
