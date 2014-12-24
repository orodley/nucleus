#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include "nuc.h"

void *gc_alloc(size_t size)
{
	void *ptr = malloc(size);
	assert(LOWTAG(ptr) == 0);

	return ptr;
}
