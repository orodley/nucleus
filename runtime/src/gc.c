#include <stdlib.h>

void *gc_alloc(size_t size)
{
	return malloc(size);
}
