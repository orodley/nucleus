#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include "nuc.h"

void *gc_alloc(size_t size)
{
	void *ptr = malloc(size);
	// malloc must produce a pointer that is suitably aligned for any builtin
	// type, so it should be safe to use the low bits for tagging
	assert(LOWTAG(ptr) == 0);

	return ptr;
}
